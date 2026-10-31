# Enums and Match

An enum is a type whose value is one of a fixed list of variants:

```zeru
enum Direction {
    North,
    East,
    South,
    West,
}

fn main() {
    var heading = Direction::East;
    println("{}", heading == Direction::East); // true
}
```

## Variants with data

A variant can carry values, of its own types:

```zeru,ignore
enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Empty,
}
```

`Shape::Circle(2.0)` is a circle of radius 2. To use what a variant
carries, `match` on it.

## match

`match` compares a value against patterns, top to bottom, and runs the
first arm that fits. The values a variant carries come out by name:

```zeru
enum Shape {
    Circle(f64),
    Rect(f64, f64),
    Empty,
}

fn area(shape: Shape) f64 {
    return match shape {
        Shape::Circle(r) => 3.14159 * r * r,
        Shape::Rect(w, h) => w * h,
        Shape::Empty => 0.0,
    };
}

fn main() {
    println("{}", area(Shape::Rect(2.0, 3.0))); // 6.0
    println("{}", area(Shape::Empty)); // 0.0
}
```

A `match` must cover every variant. Leaving one out is a compile error,
so adding a variant to an enum points at every `match` that needs a new
arm. `default`, the last arm, covers everything not listed, `|` joins
patterns that share an arm, and an `if` after a pattern adds a condition,
as in `Some(x) if x > 3 =>`:

```zeru
enum Day {
    Mon,
    Tue,
    Wed,
    Thu,
    Fri,
    Sat,
    Sun,
}

fn is_weekend(day: Day) bool {
    return match day {
        Day::Sat | Day::Sun => true,
        default => false,
    };
}

fn main() {
    println("{} {}", is_weekend(Day::Sun), is_weekend(Day::Wed)); // true false
}
```

An arm is either an expression, as above, or a block. With blocks, the
`match` is a statement and gives no value:

```zeru
var code = 404;
match code {
    200 => {
        println("ok");
    }
    404 => {
        println("not found"); // not found
    }
    default => {
        println("other");
    }
}
```

Numbers, bools and constants work as patterns too, as `200` and `404`
above.

`match` also takes apart optionals and results, with the patterns
`Some(x)`, `None`, `Ok(x)` and `Err(code)`; the [Errors](../errors/README.md)
chapter shows them.

## Matching several values at once

A tuple matches all its parts together. Each part of a pattern is a
pattern for that part, a name that binds it, or `_` for any value, and
references are matched through as the values they refer to:

```zeru
enum Value {
    Int(i64),
    Text(str),
    Nil,
}

fn same(a: &Value, b: &Value) bool {
    return match (a, b) {
        (Value::Int(x), Value::Int(y)) => x == y,
        (Value::Text(x), Value::Text(y)) => x == y,
        (Value::Nil, Value::Nil) => true,
        default => false,
    };
}

fn main() {
    println("{} {}", same(&Value::Int(2), &Value::Int(2)), same(&Value::Nil, &Value::Int(2))); // true false
}
```

The compiler checks every combination of the parts, so a `match` on two
bools needs all four or a `default`:

```zeru
fn both(a: bool, b: bool) str {
    return match (a, b) {
        (true, true) => "both",
        (true, false) | (false, true) => "one",
        (false, false) => "none",
    };
}

fn main() {
    println("{} {}", both(true, false), both(false, false)); // one none
}
```

## Methods on enums

Enums have no fields or methods of their own. Functions that take the enum,
like `area` above, do that job.
