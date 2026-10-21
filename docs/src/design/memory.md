# Zeru Memory Model Design Document

> **Version:** 0.3.0  
> **Status:** Approved Design  
> **Date:** January 2026

## Overview

This document formalizes Zeru's approach to memory management, ownership, and safety. The design prioritizes:

1. **Simplicity** - Easy to learn, no complex lifetime annotations
2. **Performance** - Closer to C/Rust/Zig than Go/Java (no GC)
3. **Safety** - Catch memory bugs without Rust-level complexity

## Core Philosophy

Zeru achieves memory safety through a combination of:

- **Move semantics** (compile-time) - Prevents double-free and use-after-move
- **Borrow checking** (compile-time) - Prevents dangling references
- **RAII** - Automatic cleanup at scope end

This is inspired by Rust (moves and borrows) and Zig (simplicity).

---

## 1. Ownership Model

### 1.1 Move Semantics

A value that owns memory (a `Vec`, or anything holding one, or anything with a
`drop` method) is **moved** when passed to a function or assigned to a new
variable. Plain data, a number, a plain struct or an array of them, is copied.

```rust
fn process(data: Vec<i32>) {
    // data is owned here
    data.push(100);
}  // data freed here

fn main() {
    var list: Vec<i32> = Vec.new();
    list.push(1);
 
    process(list);       // MOVE: ownership transfers to process()

    // list.push(2);     // COMPILE ERROR: list was moved
    list = Vec.new();    // OK: list holds a value again
}
```

**Rules:**

- After a move, the original variable is **invalid** (compile error to use)
  until it is assigned a new value; a move on one path of an `if` or a
  `match` counts after it
- Moves have **zero runtime cost** (just pointer copy)
- A move inside a loop is an error unless the loop is left, or the variable
  assigned again, before the next turn
- This prevents double-free bugs at compile time

### 1.2 Explicit Copy

When you need to retain the original value, use `.copy()`:

```rust
fn main() {
    var list: Vec<i32> = [1, 2, 3];
    
    process(list.copy());  // Deep copy passed, list still valid
    
    list.push(4);          // OK: list wasn't moved
}
```

**Rules:**

- `.copy()` performs a deep clone
- Cost is O(n) for collections
- Original variable remains valid

### 1.3 References (Borrowing)

References allow temporary access without ownership transfer.

#### Immutable Reference: `&T`

```rust
fn sum(data: &Vec<i32>) i32 {
    var total: i32 = 0;
    for item in data {
        total = total + item;
    }
    return total;
}

fn main() {
    var list: Vec<i32> = [1, 2, 3];

    var s = sum(&list);   // Borrow: list still valid
    list.push(4);         // OK: we still own it
}
```

#### Mutable Reference: `&var T`

```rust
fn double_all(data: &var Vec<i32>) {
    var i: usize = 0;
    while i < data.len() {
        data[i] = data[i] * 2;
        i = i + 1;
    }
}

fn main() {
    var list: Vec<i32> = [1, 2, 3];

    double_all(&var list);  // Mutable borrow

    println("{}", list[0]);  // 2 (modified)
}
```

**Rules:**

- `&T` = read-only access
- `&var T` = read-write access
- References do NOT transfer ownership
- References are checked at compile time (see Section 2)

---

## 2. Borrow Checking

### 2.1 The Problem

References can become invalid if the underlying data is freed or reallocated:

```rust
fn main() {
    var list: Vec<i32> = [1, 2, 3];
    var first: &i32 = &list[0];

    list.push(4);  // Might reallocate internal buffer!

    println("{}", *first); // DANGER: first might point to freed memory
}
```

### 2.2 The Rule

Zeru is single threaded, so a reference forbids only what can invalidate the
memory it points into. Two references to one place, `&var` or not, are fine,
and so is writing through a path while references into it live: the
reference sees the new value. The compiler rejects the example above because
`push` may move the buffer `first` points into.

A **loan** is taken by `&place` or `&var place` (and by `get`, a `match`
binding and a `for` loop). It is **live** from where it is taken to the last
use, along any path, of a variable holding it, directly or inside a value
such as a struct, a `T?`, an array or an enum's payload. A raw pointer
(`*T`) takes no loan and is not checked.

Some steps of a path **own** what comes after them: an element of a `Vec`,
which lives in its heap buffer, and the payload of an enum, a `T?` or a `T!`,
which a new variant replaces. While a loan is live, these are errors:

1. Moving out of what it points into, or out of anything inside that.
2. Ending the variable it points into: its block ends, or the function
   returns. A function returns a reference only into what its reference
   parameters (`self` among them) point at.
3. Assigning a new value to a place the loan reaches through an owning step
   after it, which frees the old value.
4. Assigning anything to an enum whose payload the loan points into.
5. Reallocating a place the loan reaches through an owning step after it:
   `push`, `pop`, `insert`, `remove`, `clear`, `reserve`, `shrink_to_fit`,
   or a call given a `&var` to it, directly or inside an argument (a
   struct field, a `T?`, a `self` that holds it). Taking the `&var` is
   fine; using it to reallocate is what counts.

```rust
var first = &v[0];
v[2] = 9;                 // fine: a plain write
v.push(4);                // error (5): the buffer may move
println("{}", *first);

var r = &v;
v = [1];                  // fine: r still points at a Vec
var e = &v[0];
v = [1];                  // error (3): the old buffer is freed
println("{} {}", r.len(), *e);

var hp = &var p.hp;
var mana = &var p.mana;   // fine: two &var, plain data
*hp -= 1;
*mana += 1;

match o {                 // o: Vec<i64>?
    Some(x) => { o = None; println("{}", x.len()); } // error (4): x points into Some
    None => { o = [5]; }                              // fine: no binding is live
}
```

An access through a reference counts as the same access to what it points
at: with `var m = &var v` and `var e = &m[0]`, `m.push(1)` is an error while
`e` is used later. A reference parameter is its own root: inside
`fn grow(v: &var Vec<i64>)`, `var e = &v[0]; v.push(2);` is an error while
`e` is used. A reference a function returns may point anywhere inside what
its reference arguments point at, and is used as `&var v[0]` would be:
with `var s = pick(&var v)`, `s.push(4)` is fine, but an error while a
reference taken from `s`, such as `&s[0]` or `head(s)`, is used later.

A call sees everything its arguments carry. Each `&var` among them may
reallocate what it points at, and every reference among them may be stored
where such a `&var` points, when that can hold a reference:

```rust
fn set(dst: &var (&i64)?, src: &i64) { *dst = src; }

set(&var slot, &v[0]);    // slot may now refer into v
v.push(1);                // error (5) while slot is used later
first(&var v, &v);        // error (5): first may push to v under the &v
heal(&var p.hp, &p.mana); // fine: plain data
```

A method call takes its receiver first and uses it after the arguments, as
Rust's two-phase borrows: `v.push(v.len())` and `v.push(*e)` with
`e = &v[0]` are fine, since the push happens once the arguments are read,
but `vs[0].push(grow(&var vs))` is an error, since `grow` may move the
element the receiver points at.

A `for` loop borrows every element of its sequence for the whole loop, and
a `match` binding borrows the scrutinee while the binding is used, unless
it is plain data (it owns no memory and refers to nothing) that is never
borrowed: that is a copy, as Rust's bindings of `Copy` values.

```rust
for x in &var v { x += 1; }          // fine
for x in v { v.push(x); }             // error (5): the loop walks v's buffer
for x in v { v[0] = x; }              // fine: a plain write

match st {
    Counting(n) => { st = Done; println("{}", n); }   // fine: n is a copy
    Done => {}
}
```

## 3. Returning References

### 3.1 Compile-Time Rule

References can only be returned if derived from reference parameters:

```rust
// ✅ ALLOWED: Return ref derived from ref parameter
fn first(data: &Vec<i32>) &i32 {
    return &data[0];
}

fn get_x(point: &Point) &i32 {
    return &point.x;
}

// ❌ COMPILE ERROR: Cannot return reference to local
fn dangling() &i32 {
    var x: i32 = 42;
    return &x;  // ERROR: cannot return reference to local variable
}

// ❌ COMPILE ERROR: Cannot return reference to owned parameter  
fn bad(data: Vec<i32>) &i32 {
    return &data[0];  // ERROR: data is freed at function end
}
```

### 3.2 Rationale

This simple rule, with Section 2, catches dangling references at compile time without lifetime annotations.

---

## 4. Compile Modes

Zeru supports three compilation modes with different safety/performance tradeoffs:

```
┌─────────────────────────────────────────────────────────────┐
│                     COMPILE MODES                           │
├──────────────┬───────────────┬──────────────┬──────────────┤
│    Check     │    debug      │ release-safe │ release-fast │
├──────────────┼───────────────┼──────────────┼──────────────┤
│ Move checks  │ ✅ Compile    │ ✅ Compile   │ ✅ Compile   │
│ Borrow checks│ ✅ Compile    │ ✅ Compile   │ ✅ Compile   │
│ Bounds check │ ✅ Runtime    │ ✅ Runtime   │ ❌ Skipped   │
│ Overflow     │ ✅ Runtime    │ ✅ Runtime   │ ❌ Skipped   │
├──────────────┼───────────────┼──────────────┼──────────────┤
│ Use case     │ Development   │ Production   │ Max perf     │
│ Performance  │ Slow          │ Fast         │ Fastest      │
│ Safety       │ Maximum       │ High         │ Minimal      │
└──────────────┴───────────────┴──────────────┴──────────────┘
```

### Recommended Usage

- **debug**: Development and testing
- **release-safe**: Production deployments (recommended default)
- **release-fast**: Performance-critical code where safety is manually verified

---

## 5. RAII and Automatic Cleanup

All owned values are automatically freed when they go out of scope:

```rust
fn process() {
    var list: Vec<i32> = Vec.new();
    list.push(1);
    list.push(2);
 
    if some_condition {
        var temp: Vec<i32> = Vec.new();
        temp.push(100);
    }  // temp freed here

}  // list freed here
```

### 5.1 Drop Order

Variables are dropped in **reverse declaration order**:

```rust
fn main() {
    var a = Resource.new();  // Created first
    var b = Resource.new();  // Created second
}  // b dropped first, then a
```

### 5.2 Custom Destructors

Types can define a `drop` method for custom cleanup:

```rust
struct FileHandle {
    fd: i32,

    fn drop(var self) {
        close(self.fd);
    }
}
```

---

## 6. `Vec<T>` Specification

`Vec<T>` is a **built-in type** (no import required), representing a dynamically-sized array.

### 6.1 Memory Layout

```rust
Vec<T> {
    ptr: *T,           // Pointer to heap-allocated buffer
    len: usize,        // Number of elements
    capacity: usize,   // Allocated capacity
}

Heap Buffer:
┌─────┬─────┬─────┬───────┬──────────┐
│  T  │  T  │  T  │  ...  │ unused   │
│ [0] │ [1] │ [2] │       │          │
└─────┴─────┴─────┴───────┴──────────┘
```

### 6.2 API

```rust
// Construction
fn new() Vec<T>                          // Empty vector
fn with_capacity(cap: usize) Vec<T>      // Pre-allocated

// Mutation (requires var)
fn push(self: &var Vec<T>, item: T)      // Append element
fn pop(self: &var Vec<T>) T?             // Remove last, return optional
fn insert(self: &var Vec<T>, idx: usize, item: T)
fn remove(self: &var Vec<T>, idx: usize) T
fn clear(self: &var Vec<T>)              // Remove all elements

// Access
fn len(self: &Vec<T>) usize
fn capacity(self: &Vec<T>) usize
fn is_empty(self: &Vec<T>) bool
fn get(self: &Vec<T>, idx: usize) (&T)? // Safe access, None if OOB; .copy() gives a T?
fn [](self: &Vec<T>, idx: usize) T       // Index access, panics on OOB*
fn [](self: &var Vec<T>, idx: usize) &var T  // Mutable index access

// Memory management
fn reserve(self: &var Vec<T>, additional: usize)
fn shrink_to_fit(self: &var Vec<T>)

// Copying
fn copy(self: &Vec<T>) Vec<T>            // Deep clone

// Written in the prelude
fn sort(self: &var Vec<T>)               // Ascending, for numbers
fn swap(self: &var Vec<T>, i: usize, j: usize)
fn contains(self: &Vec<T>, value: T) bool
fn sum(self: &Vec<T>) T                  // Numbers
fn min(self: &Vec<T>) T?                 // Numbers
fn max(self: &Vec<T>) T?                 // Numbers

// Slicing: v[a..b], v[a..], v[..b] give a &[T] that shares the memory

// Iteration is a `for` loop: `for x in v` reads each element (a copy of
// plain data), `for x in &var v` writes each one in place
```

*Bounds checking behavior depends on compile mode

### 6.3 Usage Examples

```rust
fn main() {
    // Creation
    var numbers: Vec<i32> = Vec.new();
    var preallocated: Vec<i32> = Vec.with_capacity(100);
    var from_literal: Vec<i32> = [1, 2, 3, 4, 5];
 
    // Mutation
    numbers.push(10);
    numbers.push(20);
    numbers.push(30);

    var last = numbers.pop();  // i32? = Some(30)
 
    // Access
    var first = numbers[0];           // 10
    var maybe = numbers.get(100);     // (&i32)? = None

    // Iteration
    for n in numbers {
        println("{}", n);
    }

    // Passing to functions
    process(numbers);          // Move
    // numbers.push(1);        // ERROR: moved
}

fn process(data: Vec<i32>) {
    for item in data {
        println("{}", item);
    }
}  // data freed here
```

---

## 7. Text and Slices

A string literal is a `str`: a pointer and a length into memory that lives as
long as the program. Text made at runtime is a `String`, which is a `Vec<u8>`
under another name: it owns its bytes, moves like any `Vec`, and frees them
when dropped. A `String` goes wherever a `str` is expected, and every `str`
method, `==` and printing work on it; the `str` it gives shares its memory.

`s[a..b]`, `s[a..]` and `s[..b]` give a view of part of a `str`, `String`,
`Vec` or array without copying: a `str` for text, a `&[T]` for elements,
checked to lie within at runtime.

A view must not outlive what it points into, and two rules catch the common
ways it would:

- The text or slice of a temporary `String`, `Vec` or array can be used
  within its statement, to print, compare or call `len()`, but not kept: the
  temporary is freed at the end of the statement. `var t = make().trim();`
  is an error that asks to store the value in a variable first.
- A function cannot return the text of a `String` it owns, since the
  `String` is freed as the function returns.

What the rules do not catch yet is a view kept in a variable while the
`String` or `Vec` it came from grows or is dropped:

```rust
var s = String.from("abc");
var t: str = s;
s.push_str("a lot more text");   // may move the bytes; t is now stale
```

---

## 8. Comparison with Other Languages

| Feature | Zeru | Rust | Zig | Go | C++ |
| --------- | ------ | ------ | ----- | ----- | ----- |
| Move semantics | ✅ Default | ✅ Default | ❌ Copy | ❌ Copy | Opt-in |
| Borrow checker | ✅ Yes | ✅ Yes | ❌ No | ❌ No | ❌ No |
| Lifetimes | ❌ No | ✅ Yes | ❌ No | ❌ No | ❌ No |
| GC | ❌ No | ❌ No | ❌ No | ✅ Yes | ❌ No |
| RAII | ✅ Yes | ✅ Yes | Defer | Defer | ✅ Yes |
| Bounds checks | Configurable | ✅ Always | Debug only | ✅ Always | ❌ Never |
| Null safety | ✅ Optional | ✅ Option | ✅ Optional | ❌ nil | ❌ nullptr |

### Performance Expectations

```
Fastest ─────────────────────────────────────────────► Slowest

C    Zig    Rust    Zeru(fast)    Zeru(safe)    C++    Go    Java
│     │      │          │              │          │      │      │
└─────┴──────┴──────────┴──────────────┴──────────┴──────┴──────┘
         │                    │
         └── No runtime ──────┘
             overhead         │
                              └── Bounds and overflow checks
```

---

## 9. Implementation Roadmap

### Phase 1: Foundation

- [x] Add `Vec<T>` type to type system
- [x] Add `&T` and `&var T` reference types  
- [x] Implement move tracking in semantic analyzer
- [x] Compile error on use-after-move

### Phase 2: Runtime

- [x] Implement heap allocation (mmap/brk syscalls)
- [x] Add compile mode flags
- [x] Borrow checking of references at compile time

### Phase 3: `Vec<T>`

- [x] Implement Vec struct and methods
- [x] Index operator codegen
- [x] Iteration (`for x in v`, `for x in &var v`)
- [x] Integration tests

### Phase 4: Polish

- [x] Error messages for ownership violations
- [x] Documentation and examples
- [ ] Performance benchmarks vs Rust/Go

---

## 10. Open Questions

1. **Shared ownership**: Should `Rc<T>` be provided for reference counting?
2. **Weak references**: Needed for cyclic data structures?
3. **Interior mutability**: Equivalent to Rust's `Cell`/`RefCell`?
4. **Custom allocators**: Arena/pool allocator API design?

---

## References

- [Rust Ownership](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)
- [Zig Manual - Memory](https://ziglang.org/documentation/master/#Memory)
- [Linear Types](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems)
