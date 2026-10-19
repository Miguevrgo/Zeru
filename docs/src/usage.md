# Usage

Now that you have installed Zeru its time to write your first program. Create a file with `.zr` extension and write some code, you may find examples/ directory helpful, here we will show a simple hello world program:

`filename: main.zr`

```rust
fn main() {
    println("Hello, World!");
}
```

Now you can compile and run it:

```sh
$ zeru build main.zr && ./build/main
Hello, World!
```

If you prefer to immediately run the compiled executable, use:

```sh
zeru run main.zr
```

Arguments for the program go after `--`, and the program reads them with
`os::args()` from `std.os`, its own path first:

```sh
zeru run main.zr -- input.txt 3
```

To only look for errors, without generating code or writing `build/`, use
`zeru check main.zr`. Editors run `zeru check --json main.zr`, which prints
each error as one JSON object per line.

There are more flags available, which you can read using the help command `zeru --help`. The executable will be generated inside the `build` directory.
