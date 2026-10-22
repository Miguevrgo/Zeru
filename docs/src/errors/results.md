# Results

`T!` is either a value of type `T` or an error. The error is an `i32` code:
what failed is told by the code, and where by the function that returned
it. A function returns the value itself on success, and `Err(code)` on
failure:

```zeru
const NEGATIVE: i32 = 1;

fn checked_sqrt(n: i64) i64! {
    if n < 0 {
        return Err(NEGATIVE);
    }
    var root: i64 = 0;
    while (root + 1) * (root + 1) <= n {
        root += 1;
    }
    return root;
}

fn main() {
    println("{}", checked_sqrt(17).unwrap()); // 4
    println("{}", checked_sqrt(-1).unwrap_err() == NEGATIVE); // true
}
```

Give your error codes names with constants, as `NEGATIVE` above. The
standard library returns the operating system's codes, and the most common
ones are always available by name: `ENOENT` (no such file), `EACCES` (not
allowed), `EEXIST`, `ENOTDIR`, `EISDIR`, `EINVAL` (bad argument), `EPIPE`,
`ENOTEMPTY`, `EADDRINUSE`, `ENETUNREACH`, `ECONNRESET`, `ETIMEDOUT`,
`ECONNREFUSED` and `EHOSTUNREACH`.

## Taking the value out

| Call | Gives |
|---|---|
| `is_ok()`, `is_err()` | whether it succeeded |
| `unwrap()` | the value; stops the program with a panic on an error |
| `unwrap_err()` | the error code; stops the program with a panic on success |

`match` handles both at once:

```zeru
import std.fs;

fn main() {
    match fs::read_file("settings.txt") {
        Ok(text) => println("{} bytes", text.len()),
        Err(code) => println("cannot read it: {}", code == ENOENT), // cannot read it: true
    }
}
```

## Passing errors on

Inside a function that returns a result itself, `x!` takes the value out of
`x`, or returns its error from the function at once:

```zeru
fn parse_pair(text: str) (i64, i64)! {
    var parts = text.split(",");
    if parts.len() != 2 {
        return Err(EINVAL);
    }
    var a = parts[0].trim().parse_int()!;
    var b = parts[1].trim().parse_int()!;
    return (a, b);
}

fn main() {
    var pair = parse_pair("3, 4").unwrap();
    println("{} {}", pair.0, pair.1); // 3 4
    println("{}", parse_pair("3, x").is_err()); // true
}
```

This keeps the normal path of a function straight, with every place that
can fail marked by a `!`.

## Functions that only succeed or fail

A function with nothing to return on success returns `!` alone. It
succeeds by reaching its end, or early with `return Ok();`:

```zeru
import std.fs;

fn save(name: str, text: str) ! {
    if text.len() == 0 {
        return Ok();
    }
    fs::write_file(name, text)!;
}

fn main() {
    save("note.txt", "hi").unwrap();
    println("{}", fs::read_file("note.txt").unwrap()); // hi
}
```

## Results must be looked at

A `T!` has to be taken apart before its value can be used: there is no
way to read the value and forget the error by accident. A result is not a
number either, so `parse_int() + 1` is a compile error that points at the
missing `!` or `unwrap()`.
