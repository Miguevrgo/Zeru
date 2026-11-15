# regex

`std.regex` finds patterns in text. A pattern is compiled once with
`regex::compile` and then used as many times as needed:

```zeru
import std.regex;

fn main() {
    const date = regex::compile("(\\d+)-(\\d+)-(\\d+)").unwrap();
    const line = "from 2026-11-11 to 2026-12-01";
    println("{}", date.is_match(line)); // true
    println("{}", date.find(line).unwrap()); // 2026-11-11
    println("{}", date.find_all(line)); // ["2026-11-11", "2026-12-01"]
    println("{}", date.captures(line).unwrap()); // ["2026-11-11", "2026", "11", "11"]
    println("{}", date.replace_all(line, "a date")); // from a date to a date
}
```

`compile` returns the error 22 (`EINVAL`) when the pattern is not valid,
for example `(` left open or `*` with nothing before it.

## Patterns

| Pattern | Matches |
|---|---|
| `a` | that byte; `\` before any byte other than a letter or digit takes it as it is |
| `.` | any byte but a line break |
| `[abc]`, `[a-z]`, `[^abc]` | one byte of the set, or one outside it |
| `\d`, `\w`, `\s` | a digit; a letter, digit or `_`; a space, tab or line break |
| `\D`, `\W`, `\S` | any byte that is not one of those |
| `\n`, `\t`, `\r` | a line break, a tab, a carriage return |
| `x*`, `x+`, `x?` | `x` any number of times, once or more, at most once |
| `x{n}`, `x{n,}`, `x{n,m}` | `x` exactly n times, at least n, from n to m |
| `x|y` | `x`, or else `y` |
| `(x)` | `x`, as a group `captures` gives back |
| `^`, `$` | the start, the end of the text |

A pattern works on bytes, as `str` does, so `\w` and `[a-z]` know only
ASCII letters. Repeats take as much as they can, and of the matches the
one found is the one that starts first. A Zeru string needs `\\` for each
`\` of the pattern, or a raw string: `` regex::compile(`\d+`) ``.

## Always fast

Matching takes time proportional to the length of the text times the size
of the pattern. No pattern, however written, makes it slow down on a long
text, so patterns from users or files are safe to run. The price is that
there are no back references (`\1`) and no lookahead.
