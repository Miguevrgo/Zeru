# Optionals

`T?` is either a value of type `T` or `None`. A function returns the value
itself, or `None`:

```zeru
fn find(names: &Vec<str>, wanted: str) usize? {
    for i, name in names {
        if name == wanted {
            return i;
        }
    }
    return None;
}

fn main() {
    var names: Vec<str> = Vec["ann", "bob"];
    var at = find(&names, "bob");
    if at.is_some() {
        println("found at {}", at.unwrap()); // found at 1
    }
    println("{}", find(&names, "zoe").is_none()); // true
}
```

A plain `usize` becomes a `usize?` on its own wherever one is wanted, so
`return i;` needs nothing around it.

## Taking the value out

| Call | Gives |
|---|---|
| `is_some()`, `is_none()` | whether there is a value |
| `unwrap()` | the value; stops the program with a panic on `None` |

`match` handles both cases at once and names the value:

```zeru
fn describe(age: i32?) {
    match age {
        Some(years) => println("{} years old", years),
        None => println("age unknown"),
    }
}

fn main() {
    describe(30); // 30 years old
    describe(None); // age unknown
}
```

## Passing None on

Inside a function that returns an optional itself, `x?` takes the value out
of `x`, or returns `None` from the function at once when there is none:

```zeru
fn half(n: i32) i32? {
    if n % 2 != 0 {
        return None;
    }
    return n / 2;
}

fn quarter(n: i32) i32? {
    return half(half(n)?);
}

fn main() {
    println("{}", quarter(12).unwrap()); // 3
    println("{}", quarter(6).is_none()); // true
}
```

## Optional references

Lookups that point into a collection, like `Vec.get` or `HashMap.get`,
return an optional reference, `(&T)?`. `*` reads the value once it is out:

```zeru
var scores: Vec<i32> = Vec[10, 20];
var second = scores.get(1);
if second.is_some() {
    println("{}", *second.unwrap()); // 20
}
```
