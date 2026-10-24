# Structs and Methods

A struct groups named fields into a new type:

```zeru
struct Point {
    x: i32,
    y: i32,
}

fn main() {
    var p = Point { x: 3, y: 4 };
    p.x += 1;
    println("{} {}", p.x, p.y); // 4 4
}
```

A literal names every field. Fields are read and written with a dot.

## Methods

Functions written inside the struct, after its fields, are its methods. A
method that takes `self` reads the value it is called on; one that takes
`var self` can change it:

```zeru
struct Counter {
    count: i32,
    step: i32,

    fn next(var self) i32 {
        self.count += self.step;
        return self.count;
    }

    fn is_past(self, limit: i32) bool {
        return self.count > limit;
    }
}

fn main() {
    var c = Counter { count: 0, step: 5 };
    c.next();
    c.next();
    println("{} {}", c.count, c.is_past(8)); // 10 true
}
```

`self` is not copied: the method works on the value itself, and `var self`
changes the caller's variable.

A function inside the struct that takes no `self` belongs to the type
itself and is called on the type's name with a dot. It is the usual way to
make values:

```zeru
struct Rect {
    w: f64,
    h: f64,

    fn square(side: f64) Rect {
        return Rect { w: side, h: side };
    }

    fn area(self) f64 {
        return self.w * self.h;
    }
}

fn main() {
    var r = Rect.square(3.0);
    println("{}", r.area()); // 9.0
}
```

## Structs inside structs

Fields can be of any type, other structs and `Vec`s included:

```zeru
struct Item {
    name: str,
    price: f64,
}

struct Cart {
    items: Vec<Item>,

    fn total(self) f64 {
        var sum = 0.0;
        for item in self.items {
            sum += item.price;
        }
        return sum;
    }
}

fn main() {
    var cart = Cart { items: Vec.new() };
    cart.items.push(Item { name: "pen", price: 1.5 });
    cart.items.push(Item { name: "book", price: 12.0 });
    println("{} items, {} total", cart.items.len(), cart.total()); // 2 items, 13.5 total
}
```

A struct whose fields are plain data, numbers, bools, `str` or arrays of
them, is plain data too and is copied on assignment. A struct holding a
`Vec`, a `String` or anything else that owns memory is moved instead; the
[Memory](../memory/README.md) chapter explains the difference.

## Comparing structs

Structs compare field by field, in the order the fields are written, like
tuples: `==` when every field is equal, and `<` by the first field that
differs. A struct whose fields all compare can be sorted, and one whose
fields are all integers, bools or enums without data can be a hash map
key:

```zeru
struct Version {
    major: i32,
    minor: i32,
}

fn main() {
    var versions: Vec<Version> = Vec[Version { major: 2, minor: 0 }, Version { major: 1, minor: 4 }];
    versions.sort();
    println("{}.{}", versions[0].major, versions[0].minor); // 1.4
    println("{}", versions[0] == Version { major: 1, minor: 4 }); // true
}
```

## Visibility

Fields are always visible to the code using the struct. Methods and the
struct itself are visible to other files only when marked `pub`, as the
[Modules](modules.md) page shows.

## Cleanup with drop

A method named `drop` that takes `var self` runs on its own when the value
goes away. It is the place to release something the value holds, like a
file or a connection; see [Cleanup](../memory/cleanup.md).
