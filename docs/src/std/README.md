# The Standard Library

The standard library is small on purpose: it covers what most programs
need, with one way to do each task.

| Module | Import | For |
|---|---|---|
| prelude | none | printing, `assert`, `exit`, `replace`, `zip`, `Vec`, `String`, text methods, error codes |
| [math](math.md) | `import std.math;` | min, max, powers and other number helpers |
| [fs](fs.md) | `import std.fs;` | files and directories |
| [os](os.md) | `import std.os;` | arguments, environment, time, processes, input |
| [net](net.md) | `import std.net;` | TCP, UDP and Unix sockets |
| [collections](collections.md) | `import std.collections.hashmap::{HashMap};` ... | hash maps and sets, queues, heaps |

These pages give an overview of each module with examples. Every function
is documented in full, with its errors and more examples, next to its code
in the `std/` folder; editors with the Zeru plugin show that documentation
when you hover over a name.

The standard library is written in Zeru itself and talks to Linux directly,
without a C library. Its errors are the operating system's error codes,
the ones listed on the [Results](../errors/results.md) page.
