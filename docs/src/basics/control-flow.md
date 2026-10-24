# Control Flow

## if

```zeru
var temperature = 24;
if temperature > 30 {
    println("hot");
} else if temperature > 15 {
    println("mild"); // mild
} else {
    println("cold");
}
```

The condition must be a `bool`: a number is never true or false by
itself. No parentheses go around it, and the braces are always there.

An `if` whose branches each hold one expression gives a value, as long as
it has an `else`:

```zeru
var temperature = 24;
var feel = if temperature > 30 { "hot" } else if temperature > 15 { "mild" } else { "cold" };
println("{}", feel); // mild
```

A branch may instead be a block that leaves with `return`, `break` or
`continue`, so it gives no value:

```zeru
fn half(n: i32) i32 {
    var even = if n % 2 == 0 { n } else { return -1; };
    return even / 2;
}

fn main() {
    println("{} {}", half(8), half(3)); // 4 -1
}
```

## while

`while` repeats its block while the condition holds:

```zeru
var n = 1;
while n < 100 {
    n *= 2;
}
println("{}", n); // 128
```

`break` leaves the loop and `continue` jumps to the next turn:

```zeru
var i = 0;
var odd_sum = 0;
while true {
    i += 1;
    if i > 9 {
        break;
    }
    if i % 2 == 0 {
        continue;
    }
    odd_sum += i;
}
println("{}", odd_sum); // 25
```

## for

`for` walks a range of numbers or the elements of a list.

A range `a..b` counts from `a` up to, but not including, `b`:

```zeru
var total = 0;
for i in 1..5 {
    total += i;
}
println("{}", total); // 10
```

`a..=b` takes `b` too:

```zeru
var total = 0;
for i in 1..=5 {
    total += i;
}
println("{}", total); // 15
```

Over an array, a `Vec` or a string, `for` gives each element in turn, and
with two names also its index:

```zeru
var names: Vec<str> = Vec["ann", "bob", "cid"];
for name in names {
    print("{} ", name);
}
println(""); // ann bob cid
for i, name in names {
    print("{}={} ", i, name);
}
println(""); // 0=ann 1=bob 2=cid
```

To change the elements in place, loop over `&var`:

```zeru
var scores: Vec<i32> = Vec[1, 2, 3];
for score in &var scores {
    score *= 10;
}
println("{} {} {}", scores[0], scores[1], scores[2]); // 10 20 30
```

A list of tuples can be taken apart as it is walked:

```zeru
var scores: Vec<(str, i32)> = Vec[("ann", 7), ("bob", 9)];
for (name, points) in scores {
    print("{}:{} ", name, points);
}
println(""); // ann:7 bob:9
```

`zip` walks two lists side by side, stopping at the end of the shorter
one. It copies and allocates nothing: each pair is read in place, as a
single list would be.

```zeru
var prices: Vec<f64> = Vec[1.5, 2.0, 4.0];
var amounts: Array<i32, 3> = [2, 1, 3];
var total = 0.0;
for (price, amount) in zip(prices, amounts) {
    total += price * amount as f64;
}
println("{}", total); // 17.0
```

`zip(&var a, b)` lets the loop change `a`, and `for i, (x, y) in zip(a, b)`
counts too. `zip` exists only inside a `for`.

`for` is the only way to walk a list, and the loop always ends: it cannot
run past the last element.

## match

`match` picks a branch by value. It is mostly used with enums and is shown
on the [Enums and Match](../structure/enums.md) page, but it works on
numbers too, where `default` catches everything else:

```zeru
var day = 6;
var kind = match day {
    6 | 7 => "weekend",
    default => "weekday",
};
println("{}", kind); // weekend
```

`low..=high` matches every integer from one to the other. An arm may add a
condition after `if`, which must hold too, or else the next arms are
tried; a name alone with such a condition stands for the whole value:

```zeru
fn kind(c: u8) str {
    return match c {
        'a'..='z' | 'A'..='Z' => "letter",
        '0'..='9' => "digit",
        x if x > 127 => "beyond ASCII",
        default => "other",
    };
}

fn main() {
    println("{} {} {} {}", kind('q'), kind('7'), kind(200), kind('?')); // letter digit beyond ASCII other
}
```
