# Modules

Each `.zr` file is a module. `import` brings another file's module into
this one, by its path from the folder of the file you compile, with dots
between folders and no `.zr`:

```text
shop/
├── main.zr
├── prices.zr
└── units/
    └── money.zr
```

```zeru,ignore
// main.zr
import prices;
import units.money;

fn main() {
    var total = prices::with_tax(100);
    println("{}", money::euros(total)); // 1.21
}
```

```zeru,ignore
// prices.zr
const TAX: i64 = 21;

pub fn with_tax(cents: i64) i64 {
    return cents + cents * TAX / 100;
}
```

```zeru,ignore
// units/money.zr
pub fn euros(cents: i64) f64 {
    return cents as f64 / 100.0;
}
```

`zeru run main.zr` finds and compiles the other files on its own; there is
no project file to write.

## Using what a module has

An imported module's items are reached with `::`, through the last part of
its path: `prices::with_tax`, `money::euros`. Only items marked `pub` can
be used from other files: functions, structs, enums, traits, constants and
methods. `TAX` above stays private to `prices.zr`. The fields of a struct
are always visible wherever the struct is.

To use some names without the prefix, list them in braces:

```zeru,ignore
import units.money::{euros};

fn main() {
    println("{}", euros(250)); // 2.5
}
```

The prefix still works for everything else in the module.

Imports go at the top of the file. Two modules can import each other.

## The standard library

The standard library's modules are imported the same way, under `std`:

```zeru
import std.math;
import std.collections.hashmap::{HashMap};

fn main() {
    var ages: HashMap<i32, i32> = HashMap.new();
    ages.insert(1, 30);
    println("{} {}", math::max(3, 7), *ages.get(1).unwrap()); // 7 30
}
```

Some things are always there, without an import: `print` and its
siblings, `assert`, `exit`, `Vec`, `String`, the methods of `str`,
optionals and results, and the error codes like `ENOENT`.
