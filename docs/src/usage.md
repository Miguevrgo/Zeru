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

There are more flags available, which you can read using the help command `zeru --help`. The executable will be generated inside the `build` directory.
