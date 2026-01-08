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
- **Generational references** (runtime) - Catches dangling references
- **RAII** - Automatic cleanup at scope end

This is inspired by Rust (moves), Vale (generational refs), and Zig (simplicity).

---

## 1. Ownership Model

### 1.1 Move Semantics (Default)

Values are **moved** by default when passed to functions or assigned to new variables.

```zeru
fn process(data: Vec<i32>) {
    // data is owned here
    data.push(100);
}  // data freed here

fn main() {
    var list: Vec<i32> = Vec::new();
    list.push(1);
    
    process(list);       // MOVE: ownership transfers to process()
    
    // list.push(2);     // COMPILE ERROR: list was moved
}
```

**Rules:**

- After a move, the original variable is **invalid** (compile error to use)
- Moves have **zero runtime cost** (just pointer copy)
- This prevents double-free bugs at compile time

### 1.2 Explicit Copy

When you need to retain the original value, use `.copy()`:

```zeru
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

```zeru
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

```zeru
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
    
    print(list[0]);         // 2 (modified)
}
```

**Rules:**

- `&T` = read-only access
- `&var T` = read-write access
- References do NOT transfer ownership
- References are protected by generational checks (see Section 2)

---

## 2. Generational References

### 2.1 The Problem

References can become invalid if the underlying data is freed or reallocated:

```zeru
fn main() {
    var list: Vec<i32> = [1, 2, 3];
    var first: &i32 = &list[0];
    
    list.push(4);  // Might reallocate internal buffer!
    list.push(5);
    list.push(6);
    
    print(*first); // DANGER: first might point to freed memory
}
```

### 2.2 The Solution: Generational References

Every heap allocation includes a **generation counter**:

```
┌─────────────────────────────────────────┐
│ HeapHeader                              │
├─────────────────────────────────────────┤
│ generation: u64  │  ... other metadata  │
├─────────────────────────────────────────┤
│              User Data                  │
└─────────────────────────────────────────┘
```

When creating a reference, we capture the current generation:

```
Reference {
    ptr: *T,
    expected_generation: u64
}
```

On dereference, we verify:

```zeru
// Compiler-generated check:
fn deref(ref: &T) T {
    if ref.expected_generation != ref.ptr.header.generation {
        panic("dangling reference: memory was freed or reallocated");
    }
    return *ref.ptr;
}
```

### 2.3 When Generation Changes

- `free()` increments generation (catches use-after-free)
- `realloc()` increments generation (catches iterator invalidation)
- New allocation at same address has different generation

### 2.4 Performance

Generational reference checks are controlled by compile mode:

| Mode | Gen-Ref Checks | Overhead |
|------|----------------|----------|
| `debug` | ✅ Enabled | ~10-15% |
| `release-safe` | ✅ Enabled | ~5-10% |
| `release-fast` | ❌ Disabled | 0% |

---

## 3. Returning References

### 3.1 Compile-Time Rule

References can only be returned if derived from reference parameters:

```zeru
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

This simple rule catches most dangling reference bugs at compile time, without requiring Rust-style lifetime annotations. The remaining cases (iterator invalidation) are caught by generational references at runtime.

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
│ Gen-refs     │ ✅ Runtime    │ ✅ Runtime   │ ❌ Skipped   │
│ Bounds check │ ✅ Runtime    │ ✅ Runtime   │ ❌ Skipped   │
│ Overflow     │ ✅ Runtime    │ ❌ Skipped   │ ❌ Skipped   │
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

```zeru
fn process() {
    var list: Vec<i32> = Vec::new();
    list.push(1);
    list.push(2);
    
    if some_condition {
        var temp: Vec<i32> = Vec::new();
        temp.push(100);
    }  // temp freed here
    
}  // list freed here
```

### 5.1 Drop Order

Variables are dropped in **reverse declaration order**:

```zeru
fn main() {
    var a = Resource::new();  // Created first
    var b = Resource::new();  // Created second
}  // b dropped first, then a
```

### 5.2 Custom Destructors

Types can define a `drop` method for custom cleanup:

```zeru
struct FileHandle {
    fd: i32,
    
    fn drop(var self) {
        close(self.fd);
    }
}
```

---

## 6. Vec<T> Specification

`Vec<T>` is a **built-in type** (no import required), representing a dynamically-sized array.

### 6.1 Memory Layout

```
Vec<T> {
    ptr: *T,           // Pointer to heap-allocated buffer
    len: usize,        // Number of elements
    capacity: usize,   // Allocated capacity
}

Heap Buffer (with generational header):
┌──────────────┬─────────────────────────────────────┐
│ generation   │  T  │  T  │  T  │  ...  │ unused   │
│   (u64)      │ [0] │ [1] │ [2] │       │          │
└──────────────┴─────────────────────────────────────┘
```

### 6.2 API

```zeru
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
fn get(self: &Vec<T>, idx: usize) T?     // Safe access, returns None if OOB
fn [](self: &Vec<T>, idx: usize) T       // Index access, panics on OOB*
fn [](self: &var Vec<T>, idx: usize) &var T  // Mutable index access

// Memory management
fn reserve(self: &var Vec<T>, additional: usize)
fn shrink_to_fit(self: &var Vec<T>)

// Copying
fn copy(self: &Vec<T>) Vec<T>            // Deep clone

// Iteration
fn iter(self: &Vec<T>) Iterator<&T>
fn iter_mut(self: &var Vec<T>) Iterator<&var T>
```

*Bounds checking behavior depends on compile mode

### 6.3 Usage Examples

```zeru
fn main() {
    // Creation
    var numbers: Vec<i32> = Vec::new();
    var preallocated: Vec<i32> = Vec::with_capacity(100);
    var from_literal: Vec<i32> = [1, 2, 3, 4, 5];
    
    // Mutation
    numbers.push(10);
    numbers.push(20);
    numbers.push(30);
    
    var last = numbers.pop();  // i32? = Some(30)
    
    // Access
    var first = numbers[0];           // 10
    var maybe = numbers.get(100);     // i32? = None
    
    // Iteration
    for n in &numbers {
        print(n);
    }
    
    // Passing to functions
    process(numbers);          // Move
    // numbers.push(1);        // ERROR: moved
}

fn process(data: Vec<i32>) {
    for item in &data {
        print(item);
    }
}  // data freed here
```

---

## 7. Comparison with Other Languages

| Feature | Zeru | Rust | Zig | Go | C++ |
|---------|------|------|-----|-----|-----|
| Move semantics | ✅ Default | ✅ Default | ❌ Copy | ❌ Copy | Opt-in |
| Borrow checker | ❌ No | ✅ Yes | ❌ No | ❌ No | ❌ No |
| Lifetimes | ❌ No | ✅ Yes | ❌ No | ❌ No | ❌ No |
| Gen-refs | ✅ Yes | ❌ No | ❌ No | ❌ No | ❌ No |
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
                              └── Small gen-ref overhead (~5-10%)
```

---

## 8. Implementation Roadmap

### Phase 1: Foundation

- [ ] Add `Vec<T>` type to type system
- [ ] Add `&T` and `&var T` reference types  
- [ ] Implement move tracking in semantic analyzer
- [ ] Compile error on use-after-move

### Phase 2: Runtime

- [ ] Implement heap allocation (mmap/brk syscalls)
- [ ] Add generational header to allocations
- [ ] Implement gen-ref checking in codegen
- [ ] Add compile mode flags

### Phase 3: Vec<T>

- [ ] Implement Vec struct and methods
- [ ] Index operator codegen
- [ ] Iterator support
- [ ] Integration tests

### Phase 4: Polish

- [ ] Error messages for ownership violations
- [ ] Documentation and examples
- [ ] Performance benchmarks vs Rust/Go

---

## 9. Open Questions

1. **Shared ownership**: Should `Rc<T>` be provided for reference counting?
2. **Weak references**: Needed for cyclic data structures?
3. **Interior mutability**: Equivalent to Rust's `Cell`/`RefCell`?
4. **Custom allocators**: Arena/pool allocator API design?

---

## References

- [Vale Language - Generational References](https://vale.dev/blog/generational-references)
- [Rust Ownership](https://doc.rust-lang.org/book/ch04-00-understanding-ownership.html)
- [Zig Manual - Memory](https://ziglang.org/documentation/master/#Memory)
- [Linear Types](https://en.wikipedia.org/wiki/Substructural_type_system#Linear_type_systems)
