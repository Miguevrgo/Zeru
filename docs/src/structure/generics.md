# Generics and Traits

## Generic functions

A function can take a type as a parameter, written in angle brackets after
its name. The same code then works for every type it is called with:

```zeru
fn swap<T>(pair: (T, T)) (T, T) {
    return (pair.1, pair.0);
}

fn main() {
    var numbers = swap((1, 2));
    var words = swap(("left", "right"));
    println("{} {}", numbers.0, words.0); // 2 right
}
```

The compiler works out `T` from the arguments. When nothing tells it, the
type is written at the call:

```zeru
fn zero<T: Num>() T {
    return 0 as T;
}

fn main() {
    println("{} {}", zero<i32>(), zero<f64>()); // 0 0.0
}
```

## Bounds

Inside a generic function, `T` can only be used in ways every possible
type allows. A bound after the name, as `T: Ord`, promises more, and the
compiler checks the promise at each call. Four bounds are built in:

| Bound | Allows | Met by |
|---|---|---|
| `Eq` | `==` and `!=` | numbers, bools, `str`, enums without data |
| `Ord` | `<`, `<=`, `>`, `>=` | numbers |
| `Num` | arithmetic, and `as` from a number | numbers |
| `Hash` | being a key of a `HashMap` or `HashSet` | integers, bools, enums without data |

```zeru
fn largest<T: Ord>(items: &Vec<T>) T {
    var best = items[0];
    for item in items {
        if item > best {
            best = item;
        }
    }
    return best;
}

fn main() {
    var ints: Vec<i64> = [3, 9, 4];
    var floats: Vec<f64> = [2.5, 0.5];
    println("{} {}", largest(&ints), largest(&floats)); // 9 2.5
}
```

Calling `largest` with a `Vec` of structs is a compile error at the call,
since structs cannot be compared with `>`.

## Generic structs

A struct can take type parameters too. The type of the variable says which
type a new value is made for:

```zeru
struct Stack<T> {
    items: Vec<T>,

    fn new() Stack<T> {
        return Stack { items: Vec.new() };
    }

    fn push(var self, item: T) {
        self.items.push(item);
    }

    fn pop(var self) T? {
        return self.items.pop();
    }
}

fn main() {
    var stack: Stack<str> = Stack.new();
    stack.push("a");
    stack.push("b");
    println("{}", stack.pop().unwrap()); // b
}
```

## Traits

A trait names a set of methods. A type meets the trait by having those
methods, nothing more to declare, and a bound can then ask for the trait:

```zeru
trait Named {
    fn name(self) str;
}

struct Cat {
    lives: i32,

    fn name(self) str {
        return "cat";
    }
}

struct Robot {
    model: i32,

    fn name(self) str {
        return "robot";
    }
}

fn greet<T: Named>(who: T) {
    println("hello, {}", who.name());
}

fn main() {
    greet(Cat { lives: 9 }); // hello, cat
    greet(Robot { model: 3 }); // hello, robot
}
```

Generic code is compiled once for each type it is used with, so it runs
as fast as code written for that type by hand.
