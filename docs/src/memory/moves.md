# Moves and Copies

## Plain data is copied

Numbers, bools, `str`, and arrays, tuples and structs made only of those,
are plain data. Assigning them, or passing them to a function, makes a
copy, and both copies live on independently:

```zeru
var a = 5;
var b = a;
b += 1;
println("{} {}", a, b); // 5 6
```

## Owners move

A `Vec`, a `String`, or anything that holds one, owns memory on the heap.
Copying it silently would copy all that memory, so assigning it moves it
instead: the new variable owns the memory, and the old one cannot be used
any more.

```zeru,fails
fn main() {
    var first: Vec<i32> = Vec[1, 2, 3];
    var second = first;
    println("{}", first.len());
}
```

```text
Error: Use of moved value 'first'. Value was previously moved and is no longer valid
```

Passing a value to a function moves it into the function the same way,
and the function frees it when it ends:

```zeru,fails
fn consume(list: Vec<i32>) {
    println("{} items", list.len());
}

fn main() {
    var list: Vec<i32> = Vec[1, 2, 3];
    consume(list);
    consume(list);
}
```

A function returns ownership by returning the value:

```zeru
fn numbers() Vec<i32> {
    var list: Vec<i32> = Vec.new();
    list.push(1);
    list.push(2);
    return list;
}

fn main() {
    var mine = numbers();
    println("{}", mine.len()); // 2
}
```

A moved-from variable can be given a new value and used again:

```zeru
var list: Vec<i32> = Vec[1];
var taken = list;
list = Vec[2, 3];
println("{} {}", taken.len(), list.len()); // 1 2
```

## Taking a value out

An owner inside a field, an element or another variable's payload cannot
simply be moved out, since what holds it would be left with nothing.
`replace` puts a new value in its place and gives back the old one:

```zeru
struct Job {
    name: String,
    log: Vec<str>,
}

fn main() {
    var job = Job { name: String.from("build"), log: Vec["start", "done"] };
    var finished = replace(&var job.log, Vec.new());
    println("{} {} {}", job.name, finished.len(), job.log.len()); // build 2 0
}
```

A `match` on a value that has no variable of its own, as one a function
just returned, moves the parts it names into its bindings. On a variable,
its bindings only borrow, so `replace` takes the value out first:

```zeru
enum Door {
    Open(String),
    Closed,
}

fn main() {
    var door = Door::Open(String.from("Ada"));
    var guests: Vec<String> = Vec.new();
    match replace(&var door, Door::Closed) {
        Door::Open(guest) => guests.push(guest),
        Door::Closed => {}
    }
    println("{} {}", guests[0], guests.len()); // Ada 1
}
```

## Copying on purpose

When you really need two independent copies, `copy()` makes one:

```zeru
var original: Vec<i32> = Vec[1, 2, 3];
var backup = original.copy();
original.push(4);
println("{} {}", original.len(), backup.len()); // 4 3
```

Every copy of owned memory is written out, so it is easy to spot in the
code. Most of the time, though, a function does not need its own copy: it
can borrow the value through a reference, as the next page shows.
