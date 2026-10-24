# Hello, World

Create a file called `main.zr`:

```zeru
fn main() {
    println("Hello, World!"); // Hello, World!
}
```

Every program starts in `main`. `println` writes a line to the standard
output.

Compile and run it in one step:

```sh
$ zeru run main.zr
   Compiling v0.3.0 (main.zr)
    Finished unoptimized + debug info in 0.03s
     Running build/main
Hello, World!
```

`zeru build main.zr` only compiles, leaving the executable in `build/main`:

```sh
$ zeru build main.zr
$ ./build/main
Hello, World!
```

The executable is a single static file. It does not need Zeru or any
library to run, so it can be copied to any Linux x86-64 machine.

## Arguments

Arguments for the program go after `--`:

```sh
$ zeru run main.zr -- input.txt 3
```

The program reads them with `os::args()`, its own path first:

```zeru,check
import std.os;

fn main() {
    var args = os::args();
    println("{} arguments", args.len() - 1);
}
```

## Printing

`print` and `println` take a format string, where each `{}` is replaced by
the next value. Numbers, bools and text can be printed, and so can lists,
tuples and optionals of them; `{{}}` writes the braces themselves.

```zeru
var name = "Ann";
var age = 30;
println("{} is {} years old", name, age); // Ann is 30 years old
println("pi is about {}", 3.14159); // pi is about 3.14159
println("{{}} stays"); // {} stays
var list: Vec<i32> = Vec[1, 2, 3];
println("{} and {}", list, (1, "one")); // [1, 2, 3] and (1, "one")
```

`eprint` and `eprintln` write to the standard error instead.

## Comments

Comments start with `//` and run to the end of the line. A comment right
above a function, struct or constant is its documentation, shown by
editors:

```zeru
// Returns `n` times two.
fn double(n: i32) i32 {
    return n * 2; // a comment can follow code too
}

fn main() {
    println("{}", double(21)); // 42
}
```

## Formatting

`zeru fmt main.zr` lays the file out in the one standard style: four-space
indents, lines up to 100 columns, comments and blank lines kept. There are
no options, so every Zeru program looks the same.
