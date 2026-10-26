# os

```zeru,ignore
import std.os;
```

## The program and its environment

| Function | Gives |
|---|---|
| `args() Vec<str>` | the command-line arguments, the program's path first |
| `env(name) String?` | an environment variable |
| `set_env(name, value) !`, `remove_env(name) !` | change one, for this program and those it runs |
| `pid()`, `parent_pid()`, `uid()`, `gid()` | the process and user ids |
| `temp_dir() String` | where temporary files go |

```zeru
import std.os;

os::set_env("GREETING", "hi").unwrap();
println("{}", os::env("GREETING").unwrap()); // hi
println("{}", os::env("NOT_SET_ANYWHERE").is_none()); // true
println("{}", os::args().len() >= 1); // true
```

## Input

| Function | Gives |
|---|---|
| `stdin() &var fs::File` | the standard input, to read a line at a time |
| `read_stdin() String!` | all of the standard input |

```zeru,ignore
var line = os::stdin().read_line().unwrap();
```

## Time

| Function | Gives |
|---|---|
| `sleep(ms)` | waits `ms` milliseconds |
| `Clock.now()`, `clock.elapsed()` | nanoseconds passed since `now`, from a clock that only moves forward |
| `time() i64` | nanoseconds since 1970, the time of day |

```zeru
import std.os;

var clock = os::Clock.now();
os::sleep(10);
println("{}", clock.elapsed() >= 10_000_000); // true
```

## Running programs

| Function | Does |
|---|---|
| `run(program, args) i32!` | runs a program, waits, and gives its exit code |
| `output(program, args) Output!` | the same, keeping its `stdout`, `stderr` and `code` |
| `kill(pid, signal) !` | sends `SIGINT`, `SIGTERM` or `SIGKILL` |

The program is looked up in `PATH` unless the name has a `/`.

```zeru
import std.os;

var args: Vec<str> = Vec["hello"];
var result = os::output("echo", args).unwrap();
println("{} {}", result.code, result.stdout.trim()); // 0 hello
```

## Random numbers

| Function | Gives |
|---|---|
| `random() u64` | a random number from the operating system |
| `random_range(first, last) u64` | a random number from `first` to `last`, both included |

## Ending

`exit(code)` ends the program at once with that code. It is in the prelude,
so it needs no import. Control never goes past it, so a function may end
with it instead of a `return`.
