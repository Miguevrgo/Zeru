# Functions

`fn` declares a function. Each parameter has a name and a type, and the
return type comes after the parentheses:

```zeru
fn add(a: i32, b: i32) i32 {
    return a + b;
}

fn main() {
    println("{}", add(2, 3)); // 5
}
```

A function that returns nothing leaves the return type out. `return;` ends
it early:

```zeru
fn greet(name: str, times: i32) {
    if times <= 0 {
        return;
    }
    for i in 0..times {
        println("hello, {}", name);
    }
}

fn main() {
    greet("ann", 1); // hello, ann
    greet("bob", 0);
}
```

Every path out of a function that returns a value ends in a `return`. Functions can
be declared in any order and can call themselves:

```zeru
fn main() {
    println("{}", fib(10)); // 55
}

fn fib(n: i32) i32 {
    if n < 2 {
        return n;
    }
    return fib(n - 1) + fib(n - 2);
}
```

## Parameters

Parameters cannot be assigned to, unless declared with `var`. A `var`
parameter is the function's own copy, so changing it does not change the
caller's value:

```zeru
fn count_down(var n: i32) {
    while n > 0 {
        print("{} ", n);
        n -= 1;
    }
    println("go");
}

fn main() {
    var start = 3;
    count_down(start); // 3 2 1 go
    println("{}", start); // 3
}
```

To let a function change the caller's variable, pass it a reference, as
the [References](../memory/references.md) page shows.

## Returning several values

A tuple returns more than one value at once:

```zeru
fn divide(a: i32, b: i32) (i32, i32) {
    return (a / b, a % b);
}

fn main() {
    var result = divide(17, 5);
    println("{} remainder {}", result.0, result.1); // 3 remainder 2
}
```

## The entry point

`main` takes no parameters and returns nothing. The program ends when
`main` returns, with exit code 0, after writing out anything still waiting
to be printed. `exit(code)` ends it at once from anywhere, also writing out
what was printed:

```zeru,check
fn main() {
    println("before");
    exit(3);
    println("never printed");
}
```
