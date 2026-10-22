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

Over an array, a `Vec` or a string, `for` gives each element in turn, and
with two names also its index:

```zeru
var names: Vec<str> = ["ann", "bob", "cid"];
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
var scores: Vec<i32> = [1, 2, 3];
for score in &var scores {
    score *= 10;
}
println("{} {} {}", scores[0], scores[1], scores[2]); // 10 20 30
```

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
