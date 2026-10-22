# References

A reference lets code read or change a value without taking it over.

- `&x` is a reference to read `x`. Its type is `&T`.
- `&var x` is a reference to change `x`. Its type is `&var T`.

```zeru
fn total(list: &Vec<i32>) i32 {
    var sum = 0;
    for x in list {
        sum += x;
    }
    return sum;
}

fn add_one(list: &var Vec<i32>) {
    list.push(1);
}

fn main() {
    var list: Vec<i32> = [1, 2, 3];
    add_one(&var list);
    println("{} {}", total(&list), list.len()); // 7 4
}
```

`list` still belongs to `main`; the functions only borrow it. Fields and
methods are reached through a reference directly. To read or write the
value a reference points at, put `*` in front:

```zeru
var count = 1;
var r = &var count;
*r += 10;
println("{}", count); // 11
```

References can point at a part of a value too, a field or an element:

```zeru
struct Player {
    hp: i32,
    mana: i32,
}

fn main() {
    var p = Player { hp: 10, mana: 5 };
    var hp = &var p.hp;
    var mana = &var p.mana;
    *hp -= 3;
    *mana += 1;
    println("{} {}", p.hp, p.mana); // 7 6
}
```

## What the compiler prevents

A reference must never outlive what it points into. The compiler checks
that while a reference is still going to be used:

- the value it points into is not moved, and does not go out of scope;
- what it points into is not freed by assigning a new value over it;
- a `Vec` or `String` it points into does not grow, shrink or get
  cleared, since that may move the elements to new memory.

```zeru,fails
fn main() {
    var list: Vec<i32> = [1, 2, 3];
    var first = &list[0];
    list.push(4);
    println("{}", *first);
}
```

```text
Error: Cannot change 'list' while 'first' still refers into it
```

`push` may move the elements to a bigger block of memory, which would
leave `first` pointing at freed memory. Once `first` is no longer used,
the `Vec` can grow again. Reading `*first` before the `push` instead would
be fine.

What the compiler does not forbid is having several references to the
same thing, even ones that change it, or writing a value while a reference
to it lives. Zeru runs on one thread, so those cannot corrupt memory; the
reference simply sees the new value:

```zeru
var list: Vec<i32> = [1, 2, 3];
var first = &list[0];
list[0] = 9;
println("{}", *first); // 9
```

## Returning references

A function can return a reference only into something one of its
reference parameters points at. Returning a reference to its own local
variables, which end with the function, is an error:

```zeru
fn first(list: &Vec<i32>) &i32 {
    return &list[0];
}

fn main() {
    var list: Vec<i32> = [4, 5];
    println("{}", *first(&list)); // 4
}
```

```zeru,fails
fn broken() &i32 {
    var x = 1;
    return &x;
}

fn main() {
    println("{}", *broken());
}
```

```text
Error: Cannot return a reference into 'x', which ends with this function
```

There are no lifetime annotations to write: these rules are all the
compiler needs.

## Loops and match

`for x in list` reads each element in place, and `for x in &var list` lets
the loop change them. Either way the `Vec` is borrowed for the whole loop,
so the loop body cannot push to it or clear it.

A `match` arm that takes a value out of a variant, as `Some(x)`, borrows
it while `x` is used, unless the value is plain data, which is simply
copied.

## Raw pointers

For low-level code, such as talking to the operating system, `*T` is a raw
pointer: an address, with arithmetic and no checks at all. `&x` converts
to one where a `*T` is expected, and `*p` reads or writes through it:

```zeru
var bytes: Array<u8, 4> = [1, 2, 3, 4];
var at: *u8 = &bytes[0];
var sum: u32 = 0;
for i in 0..4 {
    sum += *(at + i) as u32;
}
println("{}", sum); // 10
```

The compiler cannot check what a raw pointer points at. Ordinary programs
do not need them; the standard library uses them inside, behind safe
functions.
