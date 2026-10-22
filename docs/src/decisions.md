# Design Decisions

Each part of Zeru follows from a few choices. This chapter collects them,
with the reasons behind each.

## One way to do each task

There is one loop over a list (`for`), one way to report an error (a
result), one way to make text grow (`String`), one way to free memory
(letting it go out of scope), and one code style (`zeru fmt`, which has no
settings). Fewer ways means less to learn, and code written by someone
else looks like your own.

The same goes for the standard library: it covers what most programs
need, each task once, and leaves out what only a few would use.

## No garbage collector, no manual freeing

Memory is freed when its owner goes out of scope, at a point you can see
in the code. There are no pauses, nothing runs in the background, and no
`free` can be forgotten or done twice. The price is the move rule: a value
that owns memory has one owner at a time. Plain data is exempt and simply
copied, so numbers, small structs and arrays behave as they would on
paper.

## References without annotations

A reference is checked at compile time, but you never write how long it
lives. The compiler looks at where each reference is used and forbids
only what could leave it pointing at freed or moved memory: moving or
freeing the value, or growing the `Vec` it points into, while the
reference is still going to be used.

Two references to the same value, even ones that change it, are allowed.
That is safe because a Zeru program runs on one thread: nothing can
change a value between two lines of your own code.

## One thread

Zeru programs are single threaded. Most programs do not need threads, and
those that serve many clients at once can do it from one thread by
waiting on many sockets, as `net::wait` does. Without threads there are
no data races, no locks, and the reference rules above can stay simple.

## Errors are values

A function that can fail says so in its type, `T!`, and its caller has to
look at the result before using the value. There are no exceptions, so
there is no hidden path out of a function: every early return is a
`return`, an `x!` or an `x?` you can see.

An error is an `i32` code. It is what the operating system uses, it costs
nothing to return, and a program defines its own codes as constants. What
failed is told by the code, and where by the function that returned it.

Bugs are different from errors: an index out of bounds is not something a
caller can handle. Those stop the program with a panic that says where.

## Nothing implicit about numbers

Numbers never change type on their own: an `i32` and an `i64` cannot be
added without an `as`. Overflow stops the program instead of wrapping
around; when wrapping is what you want, `+%`, `-%` and `*%` say so. Both
rules turn silent wrong results into errors you see.

## Safety by default, speed when asked

The default build modes check indexes, arithmetic and `unwrap` at run
time, and stop with a file and line on a mistake. `--release-fast`
removes those checks for code that needs every bit of speed. It is a
choice made at build time, for the whole program, not a different way of
writing code. The compile-time checks of moves and references stay in
every mode.

## Traits by shape

A type meets a trait by having its methods. There is no separate block to
declare that a type implements a trait, so adding a trait never means
going back to edit the types that already fit it.

## Public fields

Struct fields are always visible. `pub` controls which functions, types
and methods other files can use; within a struct, data is just data. A
type that must keep its fields consistent offers methods to change them,
and the code that uses it calls those.

## Text is bytes

`str` and `String` are bytes, normally UTF-8, and indexing gives a byte.
There is no character type, so nothing is decoded or validated behind
your back, and `len()` is always the size in memory.

## Small, static programs

A Zeru program is one static executable. The standard library talks to
the Linux kernel directly, with no C library, so the program runs on any
Linux x86-64 machine without installing anything, and starts at once.
