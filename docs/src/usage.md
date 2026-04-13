# Usage

Create a file with `.zr` extension and write some code, you may find it useful to read the `examples` directory in the repository.

Now you can compile and run it:

```sh
zeru build file_name.zr
```

If you prefer to immediately run the compiled executable, use:

```sh
zeru run file_name.zr
```

There are more flags available, which you can read using the help command `zeru --help`. The executable will be generated inside the `build` directory.

### Example Program

```rust
// hello.zr
import std.math;

fn main() {
    var a = -5;
    var b = math::abs(a);
    println("Absolute value of -5 is {}", b);
}
```

```sh
$ ZERU_STD_PATH=/path/to/Zeru/std zeru run hello.zr
Absolute value of -5 is 5
```
