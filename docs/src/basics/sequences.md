# Arrays, Vecs and Tuples

Zeru has two kinds of lists: arrays, whose length is fixed and part of
their type, and `Vec`s, which grow and shrink. Slices look into either.
Tuples group a few values of different types.

## Arrays

`Array<T, N>` holds exactly `N` values of type `T`, stored in place with
no allocation. Write the values out, or one value and a count:

```zeru
var primes: Array<i32, 4> = [2, 3, 5, 7];
var zeros: Array<u8, 1024> = [0; 1024];
var grid: Array<Array<i32, 3>, 2> = [[0; 3]; 2];
grid[1][2] = 7;
println("{} {} {}", primes[3], zeros[500], grid[1][2]); // 7 0 7
```

Indexes start at 0 and are checked: reading past the end stops the program
with `index out of bounds` and the line where it happened.

An array is plain data, so assigning it copies every element:

```zeru
var a: Array<i32, 3> = [1, 2, 3];
var b = a;
b[0] = 9;
println("{} {}", a[0], b[0]); // 1 9
```

The length is part of the type, so an array has no `len()`: it is the `N`
you wrote.

## Vec

`Vec<T>` is a list that grows on the heap. It is built in, so it needs no
import. Make an empty one with `Vec.new()`, write its values as
`Vec[5, 3, 8]`, or ask for `n` copies of one value with `Vec[x; n]`, where
`n` can be worked out as the program runs:

```zeru
var names: Vec<str> = Vec.new();
names.push("ann");
names.push("bob");
var numbers = Vec[5, 3, 8];
var size: usize = 3;
var seen = Vec[false; size * size];
println("{} {} {}", names.len(), numbers[2], seen.len()); // 2 8 9
```

Plain brackets are always an array: `[5, 3, 8]` has a fixed length and
lives in place, `Vec[5, 3, 8]` grows on the heap. A value that owns memory,
as in `Vec[Vec.new(); n]`, is copied into each place.

The methods:

| Method | Does |
|---|---|
| `Vec.new()`, `Vec.with_capacity(n)` | an empty Vec, the second with room for `n` |
| `push(x)`, `pop()` | add at the end, take from the end (`T?`, `None` when empty) |
| `insert(i, x)`, `remove(i)` | add or take at index `i`, moving the rest |
| `len()`, `is_empty()`, `capacity()` | sizes |
| `get(i)` | a reference to element `i`, or `None` past the end |
| `swap(i, j)`, `sort()`, `reverse()` | reorder in place |
| `rotate_left(n)`, `rotate_right(n)` | move every element `n` places, wrapping around |
| `dedup()` | drop each element equal to the one before it |
| `chunks(n)` | views of the elements `n` at a time |
| `windows(n)` | views of every run of `n` elements next to each other |
| `contains(x)`, `sum()`, `min()`, `max()` | look at the values |
| `clear()`, `reserve(n)`, `shrink_to_fit()` | manage the memory |
| `copy()` | a second Vec with the same elements |

```zeru
var v: Vec<i32> = Vec[5, 3, 8];
v.sort();
println("{} {} {}", v[0], v[1], v[2]); // 3 5 8
println("{} {}", v.contains(8), v.sum()); // true 16
println("{}", v.max().unwrap()); // 8
println("{}", v.get(10).is_none()); // true
```

`println` writes a whole list, nested ones too, with any text inside in
quotes:

```zeru
var grid: Vec<Vec<i32>> = Vec[Vec[1, 2], Vec[3]];
var words: Vec<str> = Vec["a", ""];
println("{} {}", grid, words); // [[1, 2], [3]] ["a", ""]
```

Unlike an array, a `Vec` owns memory on the heap, so assigning it moves it
instead of copying it. `copy()` makes a real copy. The
[Memory](../memory/README.md) chapter explains why.

## Slices

A slice is a view into part of an array or a `Vec`, without copying
anything. `v[a..b]` takes the elements from `a` up to, not including, `b`;
either end can be left out:

```zeru
var v: Vec<i32> = Vec[10, 20, 30, 40];
var middle = v[1..3];
var tail = v[2..];
println("{} {} {}", middle.len(), middle[0], tail[1]); // 2 20 40
```

A slice has the `Vec` methods that only read: `len`, `contains`, `sum`,
`min`, `max`, `windows` and `chunks`. `a[..]` gives an array them too:

```zeru
var v: Vec<i32> = Vec[9, 4, 7, 1];
var fixed: Array<i32, 3> = [2, 5, 3];
println("{} {}", v[1..].sum(), fixed[..].max().unwrap()); // 12 5
```

`&[T]` is the type of a slice. A function that takes one works with arrays
and Vecs alike: `&v` lends all of a Vec or an array as a slice.

```zeru
fn total(values: &[i32]) i32 {
    var sum = 0;
    for x in values {
        sum += x;
    }
    return sum;
}

fn main() {
    var list: Vec<i32> = Vec[1, 2, 3];
    var fixed: Array<i32, 2> = [10, 20];
    println("{} {}", total(&list), total(&fixed)); // 6 30
    println("{}", total(list[1..])); // 5
}
```

While a slice is in use, the compiler does not let its `Vec` grow or be
freed, since that could move the elements it points at.

### Slices that write

A slice reads its elements. `&var v[a..b]` makes one that may also change
them, of type `&var [T]`, from a `var` array or Vec; `&var v` lends all of
one. Besides the methods that read, it has those that move elements around
without changing how many there are: `sort`, `swap`, `reverse`,
`rotate_left` and `rotate_right`.

```zeru
fn clear(values: &var [i32]) {
    for x in values {
        x = 0;
    }
}

fn main() {
    var v: Vec<i32> = Vec[5, 1, 4, 2, 3];
    var front = &var v[..3];
    front.sort();
    println("{}", v); // [1, 4, 5, 2, 3]
    clear(&var v[3..]);
    println("{}", v); // [1, 4, 5, 0, 0]
}
```

## Tuples

A tuple groups a fixed number of values that can have different types.
Its fields are numbered:

```zeru
var person = ("ann", 30, true);
println("{} is {}", person.0, person.1); // ann is 30
var point: (i32, i32) = (3, 4);
println("{}", point.0 * point.1); // 12
println("{}", point); // (3, 4)
```

Tuples are most useful to return several values from a function. They
compare part by part: `==` when every part is equal, and `<` by the first
part that differs, so a list of tuples sorts by its first part, then its
second:

```zeru
var scores: Vec<(i32, str)> = Vec[(2, "bob"), (1, "zoe"), (2, "ann")];
scores.sort();
println("{}", scores); // [(1, "zoe"), (2, "ann"), (2, "bob")]
```

### Taking a tuple apart

`var` and `const` can give each part of a tuple its own name. Types may be
written after each name, and `_` leaves a part out:

```zeru
fn divide(a: i32, b: i32) (i32, i32) {
    return (a / b, a % b);
}

fn main() {
    var (quotient, remainder) = divide(17, 5);
    println("{} {}", quotient, remainder); // 3 2
    const (low: u8, high: u8) = (1, 200);
    var (_, (x, y)) = ("origin", (0.5, 2.0));
    println("{} {} {}", low + high, x, y); // 201 0.5 2.0
}
```

The names are all `var` or all `const`. Values that own memory move into
their new names.

Assigning to a tuple of places sets them all at once. The whole right side
is worked out first, so two values swap without a helper:

```zeru
var a = 1;
var b = 2;
(a, b) = (b, a);
var v: Vec<i32> = Vec[1, 2, 3];
(v[0], v[2]) = (v[2], v[0]);
println("{} {} {}", a, b, v); // 2 1 [3, 2, 1]
```
