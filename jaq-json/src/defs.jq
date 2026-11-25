
# Arrays
def transpose: [range([.[] | length] | max) as $i | [.[][$i]]];

# Indexing
def in(xs)    : . as $x | xs | has     ($x);
def inside(xs): . as $x | xs | contains($x);
def  index($i): indices($i)[ 0];
def rindex($i): indices($i)[-1];

# jq 1.8's `delpaths`: every path addresses the original value. An array index means the
# element that `.[i]` reads, an index outside the array or a path through `null` deletes
# nothing, and a final slice deletes each element it covers. The distinct resolved paths are
# deleted in descending order, so no deletion moves an element that a later one addresses.
def delpaths($paths):
  def absolute($c): floor | if . < 0 then . + ($c | length) end | select(0 <= . and . < ($c | length));
  if $paths | type != "array" then error("Paths must be specified as an array") end |
  reduce $paths[] as $p (.;
    if $p | type == "array" then . else error("Path must be specified as array, not \($p | type)") end) |
  if any($paths[]; . == []) then null else
  . as $v |
  [ $paths[] as $p
    | reduce $p[:-1][] as $k ([];
        if . == null then . else
          . as $q | ($v | getpath($q)) as $c |
          if $c == null then null
          elif ($c | type) == "array" and ($k | type) == "number" then [$k | absolute($c) | $q + [.]][0]
          else $q + [$k] end
        end)
    | values as $q | ($v | getpath($q)) as $c | $p[-1] as $k
    | if $c == null then empty
      elif ($c | type) != "array" then $q + [$k]
      elif ($k | type) == "number" then $k | absolute($c) | $q + [.]
      elif ($k | type) == "object" then [range(0; $c | length)][$k.start:$k.end][] | $q + [.]
      else $q + [$k] end
  ] | unique | reverse | reduce .[] as $path ($v; getpath($path) |= empty) end;
def del(f): delpaths([path(f)]);

# Formatting
def @json: tojson;

# Numbers keep the text they were read from, as jq's decNumber literals do.
def have_decnum: true;
def have_literal_numbers: true;

# jq's streaming forms, as jq 1.8 defines them
def tostream:
  path(def r: (.[]? | r), .; r) as $p |
  getpath($p) |
  reduce path(.[]?) as $q ([$p, .]; [$p + $q]);
def fromstream(i): {x: null, e: false} as $init |
  foreach i as $i ($init;
    if .e then $init else . end |
    if $i | length == 2
    then setpath(["e"]; $i[0] | length == 0) | setpath(["x"] + $i[0]; $i[1])
    else setpath(["e"]; $i[0] | length == 1) end;
    if .e then .x else empty end);
def truncate_stream(stream):
  . as $n | null | stream | . as $input |
  if (.[0] | length) > $n then setpath([0]; $input[0][$n:]) else empty end;

# jq 1.8's object builders: a key that is not a string is an error
def from_entries: map({ (.key // .Key // .name // .Name):
  if has("value") then .value else .Value end }) | add // {};
def with_entries(f): to_entries | map(f) | from_entries;

# jq 1.8's `reverse`: through indices, so a string or number errs and `null` gives `[]`
def reverse: [.[length - 1 - range(0; length)]];

# jq 1.8's `combinations`, through indices
def combinations: if length == 0 then [] else .[0][] as $x | (.[1:] | combinations) as $y | [$x] + $y end;
def combinations(n): . as $dot | [range(n) | $dot] | combinations;
