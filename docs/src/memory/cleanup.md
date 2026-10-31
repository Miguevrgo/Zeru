# Cleanup

When a variable goes out of scope, at the end of its block or when the
function returns, the value it owns is freed. Nothing else needs to be
written:

```zeru
fn main() {
    var outer: Vec<i32> = Vec[1, 2];
    {
        var inner: Vec<i32> = Vec[3, 4];
        println("{}", inner.len()); // 2
    }
    println("{}", outer.len()); // 2
}
```

`inner`'s memory is freed at the `}` of its block, and `outer`'s at the end
of `main`. A value moved somewhere else is freed by its new owner instead,
and assigning a new value to a variable frees the old one first.

A value no variable holds, as one a function just returned, is freed at
the end of its statement. When a variable is made a reference to it, as
`var r = &make();`, or a slice or view of it, the value lives until the
end of the block instead:

```zeru
fn make() Vec<i32> {
    return Vec[1, 2, 3];
}

fn main() {
    var all = &make();
    var tail = make()[1..];
    println("{} {}", all.len(), tail); // 3 [2, 3]
}
```

## drop

A struct can run code of its own as it goes away, by having a method named
`drop` that takes `var self`. It is how the standard library closes files
and sockets without the program asking:

```zeru
struct Noisy {
    name: str,

    fn drop(var self) {
        println("dropping {}", self.name);
    }
}

fn main() {
    var a = Noisy { name: "a" };
    {
        var b = Noisy { name: "b" };
    }
    var c = Noisy { name: "c" };
    println("end of main");
}
```

```text
dropping b
end of main
dropping c
dropping a
```

Variables are dropped in the reverse order they were declared. `drop` runs
first, then the fields of the value are freed. `drop` is never called by
hand.

Because cleanup is tied to scope, an early `return`, `break` or error
still cleans up everything that was alive at that point.
