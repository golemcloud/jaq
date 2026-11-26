use crate::{Error, ValR, ValT, ValTx};
use alloc::string::{String, ToString};
use alloc::vec::Vec;
use jiff::{civil::DateTime, fmt::strtime, tz, Timestamp};

/// Convert a date-time pair to a UNIX epoch timestamp.
fn timestamp_to_epoch<V: ValT>(ts: Timestamp, frac: bool) -> ValR<V> {
    if frac {
        Ok((ts.as_microsecond() as f64 / 1e6).into())
    } else {
        let seconds = ts.as_second();
        isize::try_from(seconds)
            .map(V::from)
            .or_else(|_| V::from_num(&seconds.to_string()))
    }
}

/// An integer as a value, also where it does not fit `isize`.
fn int<V: ValT>(i: i64) -> ValR<V> {
    isize::try_from(i)
        .map(V::from)
        .or_else(|_| V::from_num(&i.to_string()))
}

/// Convert a `DateTime` to a "broken down time" array
fn datetime_to_array<V: ValT>(dt: DateTime) -> [V; 8] {
    [
        V::from(dt.year() as isize),
        V::from(dt.month() as isize - 1),
        V::from(dt.day() as isize),
        V::from(dt.hour() as isize),
        V::from(dt.minute() as isize),
        if dt.subsec_nanosecond() > 0 {
            V::from(dt.second() as f64 + dt.subsec_nanosecond() as f64 / 1e9)
        } else {
            V::from(dt.second() as isize)
        },
        V::from(dt.weekday().to_sunday_zero_offset() as isize),
        V::from(dt.day_of_year() as isize - 1),
    ]
}

/// Parse an ISO 8601 timestamp string to a number holding the equivalent UNIX timestamp
/// (seconds elapsed since 1970/01/01).
///
/// Actually, this parses RFC 3339; see
/// <https://ijmacd.github.io/rfc3339-iso8601/> for differences.
/// jq also only parses a very restricted subset of ISO 8601.
pub fn from_iso8601<V: ValT>(s: &str) -> ValR<V> {
    timestamp_to_epoch(s.parse().map_err(Error::str)?, s.contains('.'))
}

/// Format a number as an ISO 8601 timestamp string.
pub fn to_iso8601<V: ValT>(v: &V) -> Result<String, Error<V>> {
    let ts = if let Some(i) = v.as_isize() {
        Timestamp::from_second(i as i64)
    } else {
        Timestamp::from_microsecond((v.try_as_f64()? * 1e6) as i64)
    };
    Ok(ts.map_err(Error::str)?.to_string())
}

/// A broken-down time as C's `struct tm` holds it, with the year in full.
#[derive(Clone)]
struct Tm {
    year: i64,
    /// 0 to 11
    mon: i64,
    mday: i64,
    hour: i64,
    min: i64,
    sec: i64,
    /// 0 (Sunday) to 6
    wday: i64,
    /// 0 to 365
    yday: i64,
    /// seconds east of UTC
    gmtoff: i64,
    zone: String,
}

/// The days from 1970-01-01 to a day of the proleptic Gregorian calendar (the month 1 to 12),
/// by Howard Hinnant's `days_from_civil`.
fn days_from_civil(year: i64, month: i64, day: i64) -> i64 {
    let y = if month <= 2 { year - 1 } else { year };
    let era = y.div_euclid(400);
    let yoe = y - era * 400;
    let mp = (month + 9) % 12;
    let doy = (153 * mp + 2) / 5;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe - 719_468 + (day - 1)
}

/// The year, month (1 to 12) and day of the days from 1970-01-01, by Howard Hinnant's
/// `civil_from_days`.
fn civil_from_days(days: i64) -> (i64, i64, i64) {
    let z = days + 719_468;
    let era = z.div_euclid(146_097);
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36524 - doe / 146_096) / 365;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = doy - (153 * mp + 2) / 5 + 1;
    let month = if mp < 10 { mp + 3 } else { mp - 9 };
    (yoe + era * 400 + i64::from(month <= 2), month, day)
}

/// C's `gmtime` of seconds since the epoch moved by `gmtoff`, or `None` where the year does not
/// fit `struct tm` (an `int` counted from 1900).
fn tm_of(secs: i64, gmtoff: i64, zone: String) -> Option<Tm> {
    let local = secs.checked_add(gmtoff)?;
    let days = local.div_euclid(86_400);
    let rem = local.rem_euclid(86_400);
    let (year, month, mday) = civil_from_days(days);
    i32::try_from(year - 1900).ok()?;
    Some(Tm {
        year,
        mon: month - 1,
        mday,
        hour: rem / 3600,
        min: rem / 60 % 60,
        sec: rem % 60,
        wday: (days + 4).rem_euclid(7),
        yday: days - days_from_civil(year, 1, 1),
        gmtoff,
        zone,
    })
}

/// The offset from UTC (in seconds) and the abbreviation of a time zone at seconds since the
/// epoch. Before the times that jiff represents, a time zone has its earliest offset; after them,
/// its rules recur with the Gregorian calendar's 400-year cycle, so the time is moved by whole
/// cycles into the years 1970 to 2369.
fn offset_info(tz: &tz::TimeZone, secs: i64) -> (i64, String) {
    const CYCLE: i64 = 146_097 * 86_400;
    let (min, max) = (Timestamp::MIN.as_second(), Timestamp::MAX.as_second());
    let secs = if secs < min {
        min
    } else if secs > max {
        secs.rem_euclid(CYCLE)
    } else {
        secs
    };
    match Timestamp::from_second(secs) {
        Ok(ts) => {
            let info = tz.to_offset_info(ts);
            let offset = i64::from(info.offset().seconds());
            (offset, info.abbreviation().to_string())
        }
        Err(_) => (0, String::from("UTC")),
    }
}

/// C's `gmtime` (without a time zone) or `localtime` (in the time zone).
fn tm_at(secs: i64, tz: Option<&tz::TimeZone>) -> Option<Tm> {
    match tz {
        None => tm_of(secs, 0, String::from("GMT")),
        Some(tz) => {
            let (offset, zone) = offset_info(tz, secs);
            tm_of(secs, offset, zone)
        }
    }
}

fn convert_error<V: ValT>() -> Error<V> {
    Error::str("error converting number of seconds since epoch to datetime")
}

/// The seconds since the epoch that jq's `gmtime` takes of a number (C's conversion of a
/// `double` to `time_t`, which truncates), and the fraction of a second that it adds back.
fn epoch_secs<V: ValT>(v: &V) -> Result<(i64, f64), Error<V>> {
    let f = v.try_as_f64()?;
    let t = f.trunc();
    // 2^63, the first `double` past `i64::MAX`
    const LIMIT: f64 = 9_223_372_036_854_775_808.0;
    if !(-LIMIT..LIMIT).contains(&t) {
        return Err(convert_error());
    }
    #[allow(clippy::cast_possible_truncation)]
    Ok((t as i64, f - f.floor()))
}

/// jq's `gmtime` (without a time zone) and `localtime`: a number of seconds since the epoch as a
/// "broken down time" array, its seconds with the number's fraction.
pub fn gmtime<V: ValT>(v: &V, tz: Option<tz::TimeZone>) -> ValR<V> {
    let (secs, frac) = epoch_secs(v)?;
    let tm = tm_at(secs, tz.as_ref()).ok_or_else(convert_error)?;
    let sec = if frac > 0.0 {
        #[allow(clippy::cast_precision_loss)]
        Ok(V::from(tm.sec as f64 + frac))
    } else {
        int(tm.sec)
    };
    let fields = [tm.year, tm.mon, tm.mday, tm.hour, tm.min];
    let fields = fields.into_iter().map(int);
    let rest = [tm.wday, tm.yday].into_iter().map(int);
    fields.chain([sec]).chain(rest).collect()
}

/// jq's `jv2tm`: the leading numbers of a broken-down time (missing ones 0), each truncated to an
/// `int`, the year counted from 1900. `None` for an element that is not a number.
fn jv2tm<V: ValT>(bdt: &[V]) -> Option<[i64; 8]> {
    let mut tm = [0i64; 8];
    for (i, (field, v)) in tm.iter_mut().zip(bdt).enumerate() {
        let d = v.as_f64().filter(|d| !d.is_nan())?;
        let d = if i == 0 { d - 1900.0 } else { d };
        #[allow(clippy::cast_possible_truncation)]
        let n = d.clamp(f64::from(i32::MIN), f64::from(i32::MAX)) as i64;
        *field = n;
    }
    Some(tm)
}

/// C's `timegm`: the seconds since the epoch of the fields of [`jv2tm`], normalized.
fn timegm(tm: &[i64; 8]) -> i64 {
    let [year, month, mday, hour, min, sec, ..] = *tm;
    let year = year + 1900 + month.div_euclid(12);
    let month = month.rem_euclid(12) + 1;
    let days = days_from_civil(year, month, 1) + mday - 1;
    days * 86_400 + hour * 3_600 + min * 60 + sec
}

/// C's `mktime` of a local time given as if it were UTC: where the time occurs twice, the earlier
/// instant, and where it is skipped, the instant that the offset before the skip gives.
fn local_secs(tz: &tz::TimeZone, utc: i64) -> i64 {
    let before = offset_info(tz, utc - 86_400).0;
    let after = offset_info(tz, utc + 86_400).0;
    let valid = |offset: i64| offset_info(tz, utc - offset).0 == offset;
    match (valid(before), valid(after)) {
        (true, true) => utc - before.max(after),
        (false, true) => utc - after,
        _ => utc - before,
    }
}

/// The broken-down time that jq formats for the fields of [`jv2tm`]: normalized by `timegm`
/// (without a time zone) or by `mktime` in the time zone, or as they are where the normalized
/// year does not fit `struct tm`.
fn normalized(fields: &[i64; 8], tz: Option<&tz::TimeZone>) -> Tm {
    let utc = timegm(fields);
    let secs = match tz {
        None => utc,
        Some(tz) => local_secs(tz, utc),
    };
    tm_at(secs, tz).unwrap_or_else(|| {
        let [year, mon, mday, hour, min, sec, wday, yday] = *fields;
        let (year, zone) = (year + 1900, String::new());
        let gmtoff = 0;
        Tm {
            year,
            mon,
            mday,
            hour,
            min,
            sec,
            wday,
            yday,
            gmtoff,
            zone,
        }
    })
}

/// Format a date (either number or array), as jq's `strftime` (without a time zone) or
/// `strflocaltime` (in the time zone) does.
///
/// A number is seconds since the epoch, and a "broken down time" array is normalized, both as
/// [`normalized`] does.
pub fn strftime<V: ValT>(v: &V, fmt: &V, tz: Option<tz::TimeZone>, name: &str) -> ValR<V> {
    let inputs = || Error::str(format_args!("{name}/1 requires parsed datetime inputs"));
    let format = || Error::str(format_args!("{name}/1 requires a string format"));
    let tm = match v.clone().into_seq::<Vec<V>>() {
        Ok(bdt) => {
            if fmt.as_utf8_bytes().is_none() {
                return Err(format());
            }
            normalized(&jv2tm(&bdt).ok_or_else(inputs)?, tz.as_ref())
        }
        Err(v) if v.kind_name() == "number" => {
            let (secs, _) = epoch_secs(&v)?;
            tm_at(secs, tz.as_ref()).ok_or_else(convert_error)?
        }
        Err(_) => return Err(inputs()),
    };
    let fmt = match fmt.as_utf8_bytes().map(core::str::from_utf8) {
        Some(Ok(fmt)) => fmt,
        _ => return Err(format()),
    };
    // jq gives the C library's `strftime` a buffer of 100 bytes more than the format.
    let limit = fmt.len() + 100;
    let chars: Vec<char> = fmt.chars().collect();
    let mut out = String::new();
    Format { tm: &tm, limit }.run(&mut out, &chars, false);
    if (out.is_empty() && !fmt.is_empty()) || out.len() >= limit {
        return Err(Error::str(format_args!("{name}/1: unknown system failure")));
    }
    Ok(V::from(out))
}

/// The flags (`_`, `-` or `0` for padding, `^` and `#` for case) and width of a conversion.
#[derive(Clone, Copy)]
struct Spec {
    pad: Option<char>,
    upper: bool,
    swap: bool,
    width: Option<usize>,
}

/// The C library's `strftime` in the C locale (as glibc has it), for a broken-down time.
struct Format<'a> {
    tm: &'a Tm,
    /// the width past which the output cannot fit the buffer anyway
    limit: usize,
}

const DAYS: [&str; 7] = [
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
];
const MONTHS: [&str; 12] = [
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December",
];

/// `s` after padding to the width, with zeros for the flag `0` and else spaces.
fn pad_to(out: &mut String, s: &str, spec: Spec) {
    let fill = if spec.pad == Some('0') { '0' } else { ' ' };
    let len = s.chars().count();
    out.extend(core::iter::repeat(fill).take(spec.width.unwrap_or(0).saturating_sub(len)));
    out.push_str(s);
}

/// A number padded to the width, or else to `digits` with `fill` unless the flag is `-`; zeros
/// come after the sign, spaces before it.
fn number(out: &mut String, n: i64, digits: usize, fill: char, spec: Spec) {
    let fill = match spec.pad {
        Some('-') if spec.width.is_some() => ' ',
        Some('_') => ' ',
        Some('0') => '0',
        _ => fill,
    };
    let width = match (spec.pad, spec.width) {
        (_, Some(width)) => width,
        (Some('-'), None) => 0,
        (_, None) => digits,
    };
    let digits = n.unsigned_abs().to_string();
    let shortage = width.saturating_sub(usize::from(n < 0) + digits.len());
    let fills = core::iter::repeat(fill).take(shortage);
    if fill == '0' {
        out.extend((n < 0).then_some('-'));
        out.extend(fills);
    } else {
        out.extend(fills);
        out.extend((n < 0).then_some('-'));
    }
    out.push_str(&digits);
}

fn is_leap(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || year % 400 == 0
}

/// The days since the Monday that starts ISO week 1 of the year of a day of the year.
fn iso_week_days(yday: i64, wday: i64) -> i64 {
    yday - (yday - wday + 4 + 378).rem_euclid(7) + 3
}

impl Format<'_> {
    fn run(&self, out: &mut String, fmt: &[char], upper: bool) {
        let mut i = 0;
        while i < fmt.len() {
            if fmt[i] != '%' {
                out.push(fmt[i]);
                i += 1;
                continue;
            }
            let start = i;
            i += 1;
            let mut spec = Spec {
                pad: None,
                upper,
                swap: false,
                width: None,
            };
            while let Some(&c) = fmt.get(i) {
                match c {
                    '_' | '-' | '0' => spec.pad = Some(c),
                    '^' => spec.upper = true,
                    '#' => spec.swap = true,
                    _ => break,
                }
                i += 1;
            }
            while let Some(d) = fmt.get(i).and_then(|c| c.to_digit(10)) {
                let width = spec.width.unwrap_or(0).saturating_mul(10);
                spec.width = Some(width.saturating_add(d as usize).min(self.limit));
                i += 1;
            }
            let modifier = match fmt.get(i) {
                Some(&c @ ('E' | 'O')) => {
                    i += 1;
                    Some(c)
                }
                _ => None,
            };
            let conversion = fmt.get(i).copied();
            i = (i + 1).min(fmt.len());
            let valid = conversion.is_some_and(|c| self.conversion(out, c, modifier, spec));
            if !valid {
                let text: String = fmt[start..i].iter().collect();
                let text = if spec.upper {
                    text.to_uppercase()
                } else {
                    text
                };
                pad_to(out, &text, spec);
            }
        }
    }

    /// Write a conversion, or return `false` for one that the C library does not know.
    fn conversion(&self, out: &mut String, c: char, modifier: Option<char>, spec: Spec) -> bool {
        let tm = self.tm;
        let name = |names: &[&'static str], i: i64, len: usize| {
            let name = usize::try_from(i).ok().and_then(|i| names.get(i));
            name.map_or("?", |name: &&str| &name[..len.min(name.len())])
        };
        let text = |out: &mut String, s: &str, lower: bool, upper: bool| {
            let s = if lower {
                s.to_ascii_lowercase()
            } else if upper {
                s.to_ascii_uppercase()
            } else {
                s.into()
            };
            pad_to(out, &s, spec)
        };
        let sub = |out: &mut String, fmt: &str| {
            let chars: Vec<char> = fmt.chars().collect();
            let mut s = String::new();
            self.run(&mut s, &chars, spec.upper);
            pad_to(out, &s, spec)
        };
        let hour12 = match tm.hour {
            0 => 12,
            h if h > 12 => h - 12,
            h => h,
        };
        let iso = || {
            let mut year = tm.year;
            let mut days = iso_week_days(tm.yday, tm.wday);
            if days < 0 {
                year -= 1;
                days = iso_week_days(tm.yday + 365 + i64::from(is_leap(year)), tm.wday);
            } else {
                let next = iso_week_days(tm.yday - 365 - i64::from(is_leap(year)), tm.wday);
                if next >= 0 {
                    year += 1;
                    days = next;
                }
            }
            (year, days / 7 + 1)
        };
        let (e, o) = (modifier == Some('E'), modifier == Some('O'));
        let day_len = |c| if c == 'A' || c == 'B' { usize::MAX } else { 3 };
        let upcase = spec.upper || spec.swap;
        match c {
            'a' | 'A' if modifier.is_none() => {
                text(out, name(&DAYS, tm.wday, day_len(c)), false, upcase)
            }
            'b' | 'h' | 'B' if !e => text(out, name(&MONTHS, tm.mon, day_len(c)), false, upcase),
            'p' | 'P' => {
                let s = if tm.hour > 11 { "PM" } else { "AM" };
                text(out, s, c == 'P' || spec.swap, spec.upper)
            }
            'Z' => text(out, &tm.zone, spec.swap, spec.upper),
            'c' if !o => sub(out, "%a %b %e %H:%M:%S %Y"),
            'x' if !o => sub(out, "%m/%d/%y"),
            'X' if !o => sub(out, "%H:%M:%S"),
            'D' if modifier.is_none() => sub(out, "%m/%d/%y"),
            'F' if modifier.is_none() => sub(out, "%Y-%m-%d"),
            'r' => sub(out, "%I:%M:%S %p"),
            'R' => sub(out, "%H:%M"),
            'T' => sub(out, "%H:%M:%S"),
            'n' => text(out, "\n", false, false),
            't' => text(out, "\t", false, false),
            '%' => text(out, "%", false, false),
            'C' => number(out, tm.year.div_euclid(100), 1, '0', spec),
            'Y' if !o => number(out, tm.year, 1, '0', spec),
            'y' => number(out, tm.year.rem_euclid(100), 2, '0', spec),
            'G' if !e => number(out, iso().0, 1, '0', spec),
            'g' if !e => number(out, iso().0.rem_euclid(100), 2, '0', spec),
            'V' if !e => number(out, iso().1, 2, '0', spec),
            'd' if !e => number(out, tm.mday, 2, '0', spec),
            'e' if !e => number(out, tm.mday, 2, ' ', spec),
            'H' if !e => number(out, tm.hour, 2, '0', spec),
            'I' if !e => number(out, hour12, 2, '0', spec),
            'k' if !e => number(out, tm.hour, 2, ' ', spec),
            'l' if !e => number(out, hour12, 2, ' ', spec),
            'j' if !e => number(out, tm.yday + 1, 3, '0', spec),
            'm' if !e => number(out, tm.mon + 1, 2, '0', spec),
            'M' if !e => number(out, tm.min, 2, '0', spec),
            'S' if !e => number(out, tm.sec, 2, '0', spec),
            'u' => number(out, (tm.wday + 6).rem_euclid(7) + 1, 1, '0', spec),
            'w' if !e => number(out, tm.wday, 1, '0', spec),
            'U' if !e => number(out, (tm.yday - tm.wday + 7) / 7, 2, '0', spec),
            'W' if !e => {
                let monday = (tm.wday + 6).rem_euclid(7);
                number(out, (tm.yday - monday + 7) / 7, 2, '0', spec)
            }
            's' => {
                let days = days_from_civil(tm.year, tm.mon + 1, tm.mday);
                let secs = days * 86_400 + tm.hour * 3_600 + tm.min * 60 + tm.sec;
                pad_to(out, &(secs - tm.gmtoff).to_string(), spec)
            }
            'z' => {
                let off = tm.gmtoff.unsigned_abs();
                let hhmm = i64::try_from(off / 3600 * 100 + off / 60 % 60).unwrap_or(0);
                let prefix = Spec {
                    width: spec.width.map(|w| w.saturating_sub(1)),
                    ..spec
                };
                pad_to(out, "", prefix);
                out.push(if tm.gmtoff < 0 { '-' } else { '+' });
                let digits = Spec {
                    width: spec
                        .width
                        .map(|w| if spec.pad == Some('-') { w } else { w.max(4) }),
                    ..spec
                };
                number(out, hhmm, 4, '0', digits)
            }
            _ => return false,
        }
        true
    }
}

/// Parse a string into a "broken down time" array.
///
/// Without a time zone, this is jq's (with the C library's `strptime`): the fields the format
/// parses, over a `struct tm` of zeros (the year 1900, January, day 0), and the day of the week
/// and of the year computed as jq computes them when the day of the month is 1 to 31, or else
/// jq's markers for them, 8 and 367.
pub fn strptime<V: ValT>(s: &str, fmt: &str) -> ValR<V> {
    let nomatch = |_| Error::str(format_args!("date \"{s}\" does not match format \"{fmt}\""));
    let bdt = strtime::BrokenDownTime::parse(fmt, s).map_err(nomatch)?;
    if (bdt.offset(), bdt.iana_time_zone()) == (None, None) {
        return tm_fields(&bdt).into_iter().map(Ok).collect();
    }
    let dt = bdt.to_zoned().map_err(Error::str)?.into();
    datetime_to_array(dt).into_iter().map(Ok).collect()
}

/// The "broken down time" jq makes of the fields `strptime` parsed (see [`strptime`]).
fn tm_fields<V: ValT>(bdt: &strtime::BrokenDownTime) -> [V; 8] {
    let year = bdt.year().map_or(1900, isize::from);
    let mon = bdt.month().map_or(0, |month| isize::from(month) - 1);
    let mday = bdt.day().map_or(0, isize::from);
    let in_month = (1..=31).contains(&mday);
    let wday = match bdt.weekday() {
        Some(weekday) => weekday.to_sunday_zero_offset() as isize,
        None if in_month => jq_wday(year, mon, mday),
        None => 8,
    };
    let yday = match bdt.day_of_year() {
        Some(day) => isize::from(day) - 1,
        None if in_month => jq_yday(year, mon, mday),
        None => 367,
    };
    let second = match bdt.subsec_nanosecond() {
        Some(nanos) if nanos > 0 => {
            V::from(f64::from(bdt.second().unwrap_or(0)) + f64::from(nanos) / 1e9)
        }
        _ => V::from(bdt.second().map_or(0, isize::from)),
    };
    [
        V::from(year),
        V::from(mon),
        V::from(mday),
        V::from(bdt.hour().map_or(0, isize::from)),
        V::from(bdt.minute().map_or(0, isize::from)),
        second,
        V::from(wday),
        V::from(yday),
    ]
}

/// jq's `set_tm_wday`: the day of the week, by a formula that counts March as month 1.
fn jq_wday(year: isize, mon: isize, mday: isize) -> isize {
    let century = year / 100;
    let mut short_year = year % 100;
    if mon < 2 {
        short_year -= 1;
    }
    let mut month = mon - 1;
    if month < 1 {
        month += 12;
    }
    #[allow(clippy::cast_possible_truncation)]
    let shift = (2.6 * month as f64 - 0.2).floor() as isize;
    #[allow(clippy::cast_possible_truncation)]
    let quarter = (short_year as f64 / 4.0).floor() as isize;
    #[allow(clippy::cast_possible_truncation)]
    let centuries = (century as f64 / 4.0).floor() as isize;
    let wday = (mday + shift + short_year + quarter + centuries - 2 * century) % 7;
    if wday < 0 {
        wday + 7
    } else {
        wday
    }
}

/// jq's `set_tm_yday`: the day of the year, from the first of January (0).
fn jq_yday(year: isize, mon: isize, mday: isize) -> isize {
    const DAYS: [isize; 12] = [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334];
    let leap = (year % 4 == 0 && year % 100 != 0) || year % 400 == 0;
    let leap_day = isize::from(mon > 1 && leap);
    let mut month = mon.abs();
    if month > 11 {
        month %= 12;
    }
    DAYS[month as usize] + leap_day + mday - 1
}

/// Parse an array into a UNIX epoch timestamp, as jq's `mktime` (`jv2tm`, then `timegm`).
pub fn mktime<V: ValT>(v: &V) -> ValR<V> {
    let bdt: Vec<V> = v
        .clone()
        .into_seq()
        .map_err(|_| Error::str("mktime requires array inputs"))?;
    let fields = jv2tm(&bdt).ok_or_else(|| Error::str("mktime requires parsed datetime inputs"))?;
    int(timegm(&fields))
}
