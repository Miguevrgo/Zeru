# Panics and assert

Some mistakes are bugs in the program rather than errors to handle: an
index past the end of a list, arithmetic that overflows, dividing by zero,
`unwrap()` on `None`. When one happens, the program stops at once. That is
a panic. It writes what went wrong and where to the standard error, writes
out anything still waiting to be printed, and ends the process with the
`SIGABRT` signal:

```zeru,ignore
fn main() {
    var list: Vec<i32> = [1, 2, 3];
    var i: usize = 5;
    println("{}", list[i]);
}
```

```text
panic at main.zr:4:19: index out of bounds
```

What panics:

| Mistake | Message |
|---|---|
| index outside an array, `Vec`, slice or text | `index out of bounds` |
| integer arithmetic that does not fit its type | `arithmetic overflow` |
| dividing by zero | `division by zero, or MIN / -1` |
| a float too big for the integer it is cast to | `float out of range for the integer type` |
| `unwrap()` on `None` | `unwrap on None` |
| `unwrap()` on an error, `unwrap_err()` on a success | `unwrap on Err`, `unwrap_err on Ok` |
| no memory left | `out of memory` |

## assert

`assert(condition)` panics when the condition is false. Use it to state
what must hold at a point of the program; the message shows the condition
as written:

```zeru,ignore
fn average(values: &Vec<f64>) f64 {
    assert(values.len() > 0);
    return values.sum() / values.len() as f64;
}
```

```text
panic at main.zr:2:5: assertion failed: values.len() > 0
```

A message of your own goes after the condition, with `{}` like `println`.
Its values are only computed when the assertion fails:

```zeru,ignore
assert(balance >= 0, "balance went negative: {}", balance);
```

```text
panic at main.zr:7:5: balance went negative: -20
```

## Checks in release-fast

The build mode `--release-fast` leaves every check above out, and every
`assert` with its condition, to run as fast as possible. A bug that would
panic then does whatever the machine happens to do. See
[Build Modes](../tools/modes.md).
