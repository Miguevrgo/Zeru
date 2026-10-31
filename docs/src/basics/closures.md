# Closures

A closure is a function without a name, written where it is used. It is
written as a function, with `fn`, its parameters and a block:

```zeru
fn main() {
    var square = fn(x: i32) i32 {
        var y = x * x;
        return y;
    };
    println("{}", square(4)); // 16
}
```

## Passing a function

A parameter written `fn(..)` takes a closure, or a function by its name.
The types inside are what it takes, then what it returns, if anything:

```zeru
fn apply(f: fn(i32) i32, x: i32) i32 {
    return f(x);
}

fn double(x: i32) i32 {
    return x * 2;
}

fn main() {
    println("{}", apply(double, 5)); // 10
    println("{}", apply(fn(x) x + 1, 5)); // 6
}
```

When the closure is a whole argument of a call, its body may be one
expression, with no braces, and its parameters may leave out their types,
which the call already says. Anywhere else the body is a block, as a
function's:

```zeru
fn count(v: &Vec<i32>, keep: fn(i32) bool) usize {
    var n: usize = 0;
    for x in v {
        if keep(x) {
            n += 1;
        }
    }
    return n;
}

fn main() {
    var v: Vec<i32> = Vec[1, 2, 3, 4];
    println("{}", count(&v, fn(x) x % 2 == 0)); // 2
}
```

A function that takes a `fn(..)` is compiled again for each closure it is
given, as a generic is for each type, so calling the closure costs no more
than calling a function.

## Vec methods

A Vec, an array, a slice and a `str` take a closure in `map`, `filter`,
`any` and `all`; a Vec, a `var` array and a `&var [T]`, in `sort_by`:

```zeru
var v: Vec<i32> = Vec[3, 1, 4, 1, 5];
println("{}", v.map(fn(x) x * 2)); // [6, 2, 8, 2, 10]
println("{}", v.filter(fn(x) x > 2)); // [3, 4, 5]
println("{} {}", v.any(fn(x) x == 4), v.all(fn(x) x > 0)); // true true
v.sort_by(fn(a, b) b - a);
println("{}", v); // [5, 4, 3, 1, 1]
```

`map` gives a new Vec of what the closure returns for each element, and
`filter` a new Vec with a copy of each element the closure accepts.
`any` says whether it accepts some element, `all` whether it accepts every
one. `sort_by` puts the Vec in the order the closure gives: a negative
number when its first element goes before its second.

## What a closure is given

A closure is given each value as a `for` is each element: a value that
owns memory, as a `String`, stays its caller's, and the closure only reads
it. To keep it, or change it, it makes a copy:

```zeru,fails
fn main() {
    var words: Vec<String> = Vec[String.from("a")];
    var same = words.map(fn(w) w);
}
```

```text
Error: Cannot move 'w', which is borrowed
```

`words.map(fn(w) w.copy())` gives a second Vec of the same words.

## Using what is around

A closure uses the variables of the code around it as references: a `&`
when it only reads one, and a `&var` when it changes it. So a closure that
adds up into a variable changes that variable itself:

```zeru
fn each(v: &Vec<i32>, f: fn(i32)) {
    for x in v {
        f(x);
    }
}

fn main() {
    var v: Vec<i32> = Vec[1, 2, 3];
    var total = 0;
    each(&v, fn(x) {
        total += x;
    });
    println("{}", total); // 6
}
```

Those references follow the rules of every reference: while the closure
can still be called, what it refers to cannot be moved away or freed.

```zeru,fails
fn take(v: Vec<i32>) {}

fn main() {
    var v: Vec<i32> = Vec[1];
    var first = fn() i32 {
        return v[0];
    };
    take(v);
    println("{}", first());
}
```

```text
Error: Cannot move 'v' while 'first' still refers into it
```

Since it only borrows, a closure lives where it is made: it is passed as
an argument or kept in a variable, and never returned or kept inside a
struct or a `Vec`.
