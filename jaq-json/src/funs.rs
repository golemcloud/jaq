use crate::{jv_parse, Error, Num, Rc, Val, ValR};
use alloc::{boxed::Box, vec::Vec};
use bstr::ByteSlice;
use bytes::{BufMut, Bytes, BytesMut};
use jaq_core::box_iter::BoxIter;
use jaq_core::native::{bome, run, unary, v, Filter, Fun};
use jaq_core::{DataT, RunPtr};
use jaq_std::ValT as _;

/// jq's value kinds, where `true` and `false` are two.
fn jq_kind(v: &Val) -> u8 {
    match v {
        Val::Null => 0,
        Val::Bool(false) => 1,
        Val::Bool(true) => 2,
        Val::Num(_) => 3,
        Val::TStr(_) | Val::BStr(_) => 4,
        Val::Arr(_) => 5,
        Val::Obj(_) => 6,
    }
}

impl Val {
    /// Return 0 for null, the absolute value for numbers, and
    /// the length for strings, arrays, and objects.
    ///
    /// Fail on booleans.
    fn length(&self) -> ValR {
        match self {
            Val::Null => Ok(Val::from(0usize)),
            Val::Num(n) => Ok(Val::Num(n.length())),
            Val::TStr(s) => Ok(Val::from(s.chars().count())),
            Val::BStr(b) => Ok(Val::from(b.len())),
            Val::Arr(a) => Ok(Val::from(a.len())),
            Val::Obj(o) => Ok(Val::from(o.len())),
            Val::Bool(_) => Err(crate::type_error(self, "has no length")),
        }
    }

    /// Return the indices of `y` in `self`.
    fn indices<'a>(&'a self, y: &'a Val) -> Result<Box<dyn Iterator<Item = usize> + 'a>, Error> {
        match (self, y) {
            (Val::BStr(_), Val::BStr(y)) | (Val::TStr(_), Val::TStr(y)) if y.is_empty() => {
                Ok(Box::new(core::iter::empty()))
            }
            (Val::TStr(x), Val::TStr(y)) => {
                let index = |(i, _, _)| x.get(i..i + y.len());
                let iw = x.char_indices().map_while(index).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == **y).then_some(i))))
            }
            (Val::BStr(x), Val::BStr(y)) => {
                let iw = x.windows(y.len()).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == **y).then_some(i))))
            }
            (Val::Arr(_), Val::Arr(y)) if y.is_empty() => Ok(Box::new(core::iter::empty())),
            (Val::Arr(x), Val::Arr(y)) => {
                let iw = x.windows(y.len()).enumerate();
                Ok(Box::new(iw.filter_map(|(i, w)| (w == &y[..]).then_some(i))))
            }
            (Val::Arr(x), y) => {
                let ix = x.iter().enumerate();
                Ok(Box::new(ix.filter_map(move |(i, x)| (x == y).then_some(i))))
            }
            (x, y) => Err(crate::index_type_error(x, y)),
        }
    }

    /// `a` contains `b` iff either
    /// * the string `b` is a substring of `a`,
    /// * every element in the array `b` is contained in some element of the array `a`,
    /// * for every key-value pair `k, v` in `b`,
    ///   there is a key-value pair `k, v'` in `a` such that `v'` contains `v`, or
    /// * `a` equals `b`.
    ///
    /// This decides on a stack of pending checks rather than recursing once per level of nesting.
    fn contains(&self, other: &Self) -> bool {
        /// A check under way: whether every one of `rs` is contained in some element of `l`
        /// (an array) or in the value at the same key (an object), or whether some element of
        /// `ls` contains `r`.
        enum Frame<'a> {
            All(&'a Val, alloc::vec::IntoIter<(Option<&'a Val>, &'a Val)>),
            Any(core::slice::Iter<'a, Val>, &'a Val),
        }
        // Whether `l` contains `r` without looking into elements, or the frame that decides it.
        fn start<'a>(l: &'a Val, r: &'a Val) -> Result<bool, Frame<'a>> {
            match (l, r) {
                (Val::BStr(l), Val::BStr(r)) | (Val::TStr(l), Val::TStr(r)) => {
                    Ok(l.contains_str(&**r))
                }
                (Val::Arr(_), Val::Arr(rs)) => {
                    let rs: alloc::vec::Vec<_> = rs.iter().map(|r| (None, r)).collect();
                    Err(Frame::All(l, rs.into_iter()))
                }
                (Val::Obj(ls), Val::Obj(rs)) => {
                    let rs: alloc::vec::Vec<_> = rs.iter().map(|(k, r)| (ls.get(k), r)).collect();
                    Err(Frame::All(l, rs.into_iter()))
                }
                _ => Ok(l == r),
            }
        }
        let mut stack = match start(self, other) {
            Ok(b) => return b,
            Err(frame) => alloc::vec::Vec::from([frame]),
        };
        // the answer of the check that just ended, for the one that waits on it
        let mut answer: Option<bool> = None;
        while let Some(frame) = stack.last_mut() {
            let next = match frame {
                Frame::All(_, _) if answer == Some(false) => {
                    stack.pop();
                    continue;
                }
                Frame::Any(_, _) if answer == Some(true) => {
                    stack.pop();
                    continue;
                }
                Frame::All(l, rs) => match rs.next() {
                    None => {
                        answer = Some(true);
                        stack.pop();
                        continue;
                    }
                    // an object's entry: the value at the same key must contain it
                    Some((Some(l), r)) => start(l, r),
                    Some((None, r)) => match l {
                        Val::Arr(ls) => Err(Frame::Any(ls.iter(), r)),
                        // a key missing from the object
                        _ => Ok(false),
                    },
                },
                Frame::Any(ls, r) => match ls.next() {
                    None => {
                        answer = Some(false);
                        stack.pop();
                        continue;
                    }
                    Some(l) => start(l, r),
                },
            };
            match next {
                Ok(b) => answer = Some(b),
                Err(frame) => {
                    answer = None;
                    stack.push(frame);
                }
            }
        }
        answer.unwrap_or(true)
    }

    fn to_bytes(&self) -> Result<Bytes, Self> {
        match self {
            Val::Num(n) => n
                .as_isize()
                .and_then(|i| u8::try_from(i).ok())
                .map(|u| Bytes::from(Vec::from([u])))
                .ok_or_else(|| self.clone()),
            Val::BStr(b) | Val::TStr(b) => Ok(*b.clone()),
            Val::Arr(a) => {
                let mut buf = BytesMut::new();
                for x in a.iter() {
                    buf.put(Val::to_bytes(x)?);
                }
                Ok(buf.into())
            }
            _ => Err(self.clone()),
        }
    }

    fn as_bytes_owned(&self) -> Option<Bytes> {
        match self {
            Self::BStr(b) | Self::TStr(b) => Some(*b.clone()),
            _ => None,
        }
    }

    fn as_utf8_bytes_owned(&self) -> Option<Bytes> {
        self.is_utf8_str().then(|| self.as_bytes_owned()).flatten()
    }

    /// Return bytes if the value is a (byte or text) string.
    pub fn try_as_bytes_owned(&self) -> Result<Bytes, Error> {
        self.as_bytes_owned()
            .ok_or_else(|| Error::typ(self.clone(), "string"))
    }

    /// Return bytes if the value is a text string.
    pub fn try_as_utf8_bytes_owned(&self) -> Result<Bytes, Error> {
        self.as_utf8_bytes_owned()
            .ok_or_else(|| Error::typ(self.clone(), "string"))
    }
}

self_cell::self_cell!(
    struct BytesValRs {
        owner: Bytes,

        #[not_covariant]
        dependent: ValRs,
    }
);

impl Iterator for BytesValRs {
    type Item = ValR;
    fn next(&mut self) -> Option<Self::Item> {
        self.with_dependent_mut(|_owner, iter| iter.next())
    }
}

type ValRs<'a> = BoxIter<'a, ValR>;

/// Apply a function to bytes and yield the resulting value results.
pub fn bytes_valrs(b: Bytes, f: impl FnOnce(&[u8]) -> ValRs) -> ValRs<'static> {
    Box::new(BytesValRs::new(b, |b| f(b)))
}

/// Functions of the standard library.
pub fn funs<D: for<'a> DataT<V<'a> = Val>>() -> impl Iterator<Item = Fun<D>> {
    base().into_vec().into_iter().map(run)
}

fn base<D: for<'a> DataT<V<'a> = Val>>() -> Box<[Filter<RunPtr<D>>]> {
    Box::new([
        // jq 1.8's `fromjson`, `tonumber` and `toboolean`, with its messages.
        ("fromjson", v(0), |cv| {
            bome(match &cv.1 {
                Val::TStr(s) => jv_parse::parse_sized(s).map_err(Error::str),
                v => Err(crate::type_error(v, "only strings can be parsed")),
            })
        }),
        ("tonumber", v(0), |cv| {
            let message = "cannot be parsed as a number";
            bome(match &cv.1 {
                Val::Num(_) => Ok(cv.1.clone()),
                // jq reads the string as a C string: a NUL in it fails.
                Val::TStr(s) if !s.contains(&0) => jv_parse::number_literal(s)
                    .map(Val::Num)
                    .ok_or_else(|| crate::type_error(&cv.1, message)),
                v => Err(crate::type_error(v, message)),
            })
        }),
        ("toboolean", v(0), |cv| {
            bome(match &cv.1 {
                Val::Bool(_) => Ok(cv.1.clone()),
                Val::TStr(s) if &***s == b"true" => Ok(Val::Bool(true)),
                Val::TStr(s) if &***s == b"false" => Ok(Val::Bool(false)),
                v => Err(crate::type_error(v, "cannot be parsed as a boolean")),
            })
        }),
        ("tojson", v(0), |cv| bome(Ok(Val::utf8_str(cv.1.to_json())))),
        ("tobytes", v(0), |cv| {
            let fail = |v| Error::str(format_args!("cannot convert {v} to bytes"));
            bome(cv.1.to_bytes().map(Val::byte_str).map_err(fail))
        }),
        ("length", v(0), |cv| bome(cv.1.length())),
        // jq checks containment only between values of one kind (`true` and `false` are two).
        ("contains", v(1), |cv| {
            unary(cv, |x, y| {
                if jq_kind(&x) != jq_kind(&y) {
                    let message = "cannot have their containment checked";
                    return Err(crate::type_error2(&x, &y, message));
                }
                Ok(Val::from(x.contains(&y)))
            })
        }),
        ("has", v(1), |cv| {
            unary(cv, |v, k| match (&v, &k) {
                (Val::Null, _) => Ok(false.into()),
                (Val::Obj(o), Val::TStr(_) | Val::BStr(_)) => Ok(o.contains_key(&k).into()),
                (Val::Arr(a), Val::Num(n)) => {
                    // jq truncates the index; NaN and negative indices are never present.
                    let d = n.as_f64();
                    #[allow(clippy::cast_possible_truncation, clippy::cast_sign_loss)]
                    let present = !d.is_nan() && d > -1.0 && (d as usize) < a.len();
                    Ok(present.into())
                }
                _ => Err(Error::str(format_args!(
                    "Cannot check whether {} has a {} key",
                    crate::type_name(&v),
                    crate::type_name(&k)
                ))),
            })
        }),
        ("indices", v(1), |cv| {
            let to_int = |i: usize| Val::from(i as isize);
            unary(cv, move |x, v| {
                x.indices(&v).map(|idxs| idxs.map(to_int).collect())
            })
        }),
        ("delpaths", v(1), |cv| unary(cv, delpaths)),
        ("bsearch", v(1), |cv| {
            unary(cv, move |a, x| {
                a.as_arr().map(|a| Val::from(bsearch(a, &x)))
            })
        }),
    ])
}

/// jq's binary search: the index of `x` in `a`, or `-1 - i` for the index `i` it would go at.
/// On an unsorted array, it finds what jq's own search finds.
fn bsearch(a: &[Val], x: &Val) -> isize {
    let (mut start, mut end) = (0, a.len());
    while start < end {
        let mid = start + (end - start) / 2;
        match x.cmp(&a[mid]) {
            core::cmp::Ordering::Equal => return mid as isize,
            core::cmp::Ordering::Less => end = mid,
            core::cmp::Ordering::Greater => start = mid + 1,
        }
    }
    -1 - start as isize
}

/// jq's `jv_delpaths`: the paths sorted, then deleted as [`delpaths_sorted`] does.
fn delpaths(v: Val, paths: Val) -> ValR {
    let Val::Arr(paths) = paths else {
        return Err(Error::str("Paths must be specified as an array"));
    };
    let mut paths: Vec<&Val> = paths.iter().collect();
    paths.sort();
    let paths = paths
        .into_iter()
        .map(|p| match p {
            Val::Arr(p) => Ok(&p[..]),
            p => Err(Error::str(format_args!(
                "Path must be specified as array, not {}",
                crate::type_name(p)
            ))),
        })
        .collect::<Result<Vec<_>, _>>()?;
    match paths.first() {
        None => Ok(v),
        Some([]) => Ok(Val::Null),
        Some(_) => delpaths_sorted(v, &paths),
    }
}

/// jq's `delpaths_sorted`, on a stack of levels rather than recursing once per key of a path.
///
/// At a level, the paths share their first keys up to the level's depth, and `v` is the value
/// at those keys. They are grouped by their next key, an equal key in one group. A group that has
/// a path ending at the key deletes that key; any other group deletes the rest of its paths from
/// the value at the key, in the value as the groups before it changed it. The keys are deleted
/// last.
fn delpaths_sorted<'p>(v: Val, paths: &'p [&'p [Val]]) -> ValR {
    struct Level<'p> {
        v: Val,
        paths: &'p [&'p [Val]],
        // the group under way, and where the next one starts
        i: usize,
        j: usize,
        delkeys: Vec<&'p Val>,
    }
    let level = |v, paths| Level {
        v,
        paths,
        i: 0,
        j: 0,
        delkeys: Vec::new(),
    };
    let mut stack = Vec::from([level(v, paths)]);
    loop {
        let depth = stack.len() - 1;
        let Some(top) = stack.last_mut() else {
            unreachable!()
        };
        if top.i < top.paths.len() {
            let paths = top.paths;
            let path: &'p [Val] = paths[top.i];
            let key = &path[depth];
            let group = paths[top.i..].iter().take_while(|p| p[depth] == *key);
            let j = top.i + group.count();
            if path.len() == depth + 1 {
                top.delkeys.push(key);
                top.i = j;
            } else {
                let sub = jaq_core::ValT::index(top.v.clone(), key)?;
                if sub == Val::Null {
                    top.i = j;
                } else {
                    top.j = j;
                    let group = &paths[top.i..j];
                    stack.push(level(sub, group));
                }
            }
            continue;
        }
        let Some(done) = stack.pop() else {
            unreachable!()
        };
        let v = dels(done.v, &done.delkeys)?;
        let Some(parent) = stack.last_mut() else {
            return Ok(v);
        };
        let path: &'p [Val] = parent.paths[parent.i];
        parent.v = set(core::mem::take(&mut parent.v), &path[depth - 1], v)?;
        parent.i = parent.j;
    }
}

/// jq's `jv_set` of a key where indexing read a value other than `null`.
fn set(mut v: Val, key: &Val, x: Val) -> ValR {
    match (&mut v, key) {
        (Val::Obj(o), Val::TStr(_) | Val::BStr(_)) => {
            Rc::make_mut(o).insert(key.clone(), x);
        }
        (Val::Arr(a), Val::Num(n)) => {
            let i = crate::jq_array_index(n, a.len());
            let i = i.ok_or_else(|| Error::str("Out of bounds negative array index"))?;
            Rc::make_mut(a)[i] = x;
        }
        (Val::Arr(a), Val::Obj(o)) => {
            let (skip, take) = crate::slice_bounds(crate::slice_range(o)?, a.len())?;
            let Val::Arr(x) = x else {
                return Err(Error::str(
                    "A slice of an array can only be assigned another array",
                ));
            };
            Rc::make_mut(a).splice(skip..skip + take, x.iter().cloned());
        }
        (Val::TStr(_) | Val::BStr(_), Val::Obj(_)) => {
            return Err(Error::str("Cannot update string slices"))
        }
        _ => {
            return Err(Error::str(format_args!(
                "Cannot update field at {} index of {}",
                crate::type_name(key),
                crate::type_name(&v)
            )))
        }
    }
    Ok(v)
}

/// jq's `jv_dels`: delete sorted keys from a value.
fn dels(v: Val, keys: &[&Val]) -> ValR {
    match v {
        Val::Null => Ok(v),
        _ if keys.is_empty() => Ok(v),
        Val::Arr(mut a) => {
            let mut del = alloc::vec![false; a.len()];
            for key in keys {
                match key {
                    Val::Num(n) => {
                        if let Some(i) = del_index(n, a.len()) {
                            del[i] = true;
                        }
                    }
                    Val::Obj(o) => {
                        let (skip, take) = crate::slice_bounds(crate::slice_range(o)?, a.len())?;
                        del[skip..skip + take].fill(true);
                    }
                    k => {
                        return Err(Error::str(format_args!(
                            "Cannot delete {} element of array",
                            crate::type_name(k)
                        )))
                    }
                }
            }
            if del.contains(&true) {
                let mut del = del.into_iter();
                Rc::make_mut(&mut a).retain(|_| !del.next().unwrap_or(false));
            }
            Ok(Val::Arr(a))
        }
        Val::Obj(mut o) => {
            let is_str = |k: &Val| matches!(k, Val::TStr(_) | Val::BStr(_));
            if let Some(k) = keys.iter().find(|k| !is_str(k)) {
                return Err(Error::str(format_args!(
                    "Cannot delete {} field of object",
                    crate::type_name(k)
                )));
            }
            if keys.iter().any(|k| o.contains_key(*k)) {
                let gone = |k: &Val| keys.binary_search(&k).is_ok();
                Rc::make_mut(&mut o).retain(|k, _| !gone(k));
            }
            Ok(Val::Obj(o))
        }
        v => Err(Error::str(format_args!(
            "Cannot delete fields from {}",
            crate::type_name(&v)
        ))),
    }
}

/// The element that jq's `jv_dels` deletes for a number: the number truncated to an `int` (one
/// outside its range deletes nothing), from the end of the array when the number is negative.
fn del_index(n: &Num, len: usize) -> Option<usize> {
    let d = n.as_f64();
    let t = d.trunc();
    if !(f64::from(i32::MIN)..=f64::from(i32::MAX)).contains(&t) {
        return None;
    }
    #[allow(clippy::cast_possible_truncation)]
    let t = t as i64;
    let i = if d < 0.0 { len as i64 + t } else { t };
    usize::try_from(i).ok().filter(|i| *i < len)
}
