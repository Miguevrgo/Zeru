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
import. Make an empty one with `Vec.new()`, or write its first values like
an array:

```zeru
var names: Vec<str> = Vec.new();
names.push("ann");
names.push("bob");
var numbers: Vec<i32> = [5, 3, 8];
println("{} {}", names.len(), numbers[2]); // 2 8
```

The methods:

| Method | Does |
|---|---|
| `Vec.new()`, `Vec.with_capacity(n)` | an empty Vec, the second with room for `n` |
| `push(x)`, `pop()` | add at the end, take from the end (`T?`, `None` when empty) |
| `insert(i, x)`, `remove(i)` | add or take at index `i`, moving the rest |
| `len()`, `is_empty()`, `capacity()` | sizes |
| `get(i)` | a reference to element `i`, or `None` past the end |
| `swap(i, j)`, `sort()` | reorder in place |
| `contains(x)`, `sum()`, `min()`, `max()` | look at the values |
| `clear()`, `reserve(n)`, `shrink_to_fit()` | manage the memory |
| `copy()` | a second Vec with the same elements |

```zeru
var v: Vec<i32> = [5, 3, 8];
v.sort();
println("{} {} {}", v[0], v[1], v[2]); // 3 5 8
println("{} {}", v.contains(8), v.sum()); // true 16
println("{}", v.max().unwrap()); // 8
println("{}", v.get(10).is_none()); // true
```

Unlike an array, a `Vec` owns memory on the heap, so assigning it moves it
instead of copying it. `copy()` makes a real copy. The
[Memory](../memory/README.md) chapter explains why.

## Slices

A slice is a view into part of an array or a `Vec`, without copying
anything. `v[a..b]` takes the elements from `a` up to, not including, `b`;
either end can be left out:

```zeru
var v: Vec<i32> = [10, 20, 30, 40];
var middle = v[1..3];
var tail = v[2..];
println("{} {} {}", middle.len(), middle[0], tail[1]); // 2 20 40
```

`&[T]` is the type of a slice. A function that takes one works with arrays
and Vecs alike:

```zeru
fn total(values: &[i32]) i32 {
    var sum = 0;
    for x in values {
        sum += x;
    }
    return sum;
}

fn main() {
    var list: Vec<i32> = [1, 2, 3];
    var fixed: Array<i32, 2> = [10, 20];
    println("{} {}", total(list[..]), total(fixed[..])); // 6 30
}
```

While a slice is in use, the compiler does not let its `Vec` grow or be
freed, since that could move the elements it points at.

## Tuples

A tuple groups a fixed number of values that can have different types.
Its fields are numbered:

```zeru
var person = ("ann", 30, true);
println("{} is {}", person.0, person.1); // ann is 30
var point: (i32, i32) = (3, 4);
println("{}", point.0 * point.1); // 12
```

Tuples are most useful to return several values from a function.
