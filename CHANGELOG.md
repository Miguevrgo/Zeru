# Changelog

## Unreleased (since 0.4.0)

### Language
- Closures: `fn(x) x * 2`, block bodies, capture by reference; a function's name works as one.
- `match` on text, on tuples, on integer ranges; a range match covering the whole type needs no `default`.
- Partly overlapping match arms are accepted; only fully covered ones are an error.
- `== None` / `!= None` are errors; ask with `is_none()` / `is_some()`.
- `==` and `!=` on `T?` compare with a value or another `T?`.
- `Some(x)` makes a `T?` where nothing asks for one.
- `None` beside plain branches makes the `if`/`match` a `T?`.
- A number literal branch takes the type of the other branches.
- `u8 as str` gives that one byte as text.
- `!` is a type anywhere a type goes; `()!` and `void!` are errors suggesting `!`.
- A shift amount can be any integer type.
- `var _ = x;` drops a value on purpose; `exit()` and `assert(false)` end control flow.
- A temporary a variable borrows lives until the end of its block.
- A field can be moved out of a temporary.
- Structs and tuples compare field by field; NaN in a field makes them unequal.
- Constants computed by functions run drops, evaluate slices, text methods, sort, parsing and `T!`.
- A constant cannot have a type with a drop or that owns memory; pointers are an error there.
- Methods of generic structs are checked only when called.
- A private method meets no bound outside its own file.
- Modules are keyed by file: import cycles through the main file and two modules with the same last name work.
- Stack overflow panics with a message when checks are on.

### Tooling
- `zeru test` runs a `#test` block per file, each test alone; its asserts are checked in every build mode.
- `--native` builds for the processor compiling.
- Warnings: unused, unreachable, redundant code; a `var` that never changes; a `var` parameter or `var self` that never changes (not in generic code); division by zero; shorter forms (`is_empty`).
- Every error explains the lines involved and comes with a fix where one change fixes it.
- Errors recognise slips from other languages and suggest the name or import meant.
- `zeru fmt` keeps comments inside lists, calls, signatures and operator chains; wraps long generics.
- UI test snapshots keep the warnings of a build that succeeds.

### Std
- `std.term`: raw mode, cursor, clearing, `key(ms)`.
- `std.simd`: vectors as wide as the target's registers.
- Vec `map`, `filter`, `any`, `all`, `sort_by`, `extend`; slices `join`; arrays have the slice methods.
- `str` has the slice methods (`sum`, `min`, `max`, `windows`, `chunks`) since it is a `&[u8]`.
- `str.is_empty()`, `len()` on arrays, `is_empty()` on arrays and slices.
- `format()` returns the text `print` would write.
- `str` and `String` can be HashMap and HashSet keys.
- `math::div_euclid` takes integers only.
- Heap blocks are cache-line aligned, large ones on huge pages.

### Removed
- `push_str`: `extend` adds text to a String.

### Fixed
- Matching a tuple of references freed the bound payload.
- Borrow checker accepted reads of freed memory: stores through parameters, calls linking a value to itself, drops after what they refer into, `&` arguments returned through `self`.
- False borrow errors through `&&`, closures in loops, `var self` methods with `&` arguments.
- NaN in struct and tuple `==`.
- f32 vectors claimed 8-byte alignment.
- `parse_float` rounding with more than 19 digits and with huge exponents; `parse_int` error code on non-numbers.
- `simd::min`/`max` differed between build modes.
- `reserve(usize.MAX)` did nothing; `chunks(0)`/`windows(0)` ran away in release-fast.
- `mod_euclid(MIN, -1)` panicked.
- std fds took 0-2 and leaked into child processes; `run`/`output` hung with stdout closed.
- `os::output` leaked a pipe when the program could not start.
- A directory in `PATH` hid the program; `run("")` gave the wrong error.
- `copy_file` emptied the target when the source was a directory or unsupported.
- `term::key` left parts of unlisted escape sequences.
- `UnixListener` removed the wrong file after a directory change.
- `real_path`, empty Unix paths, IPv4 leading zeros and long IPv6 groups.
- Statement `match` mixing unit and void arms; temporaries in `print`; `'a' as str`; `return Vec.new()` in a `T!`.
- Closure parameter types from a literal argument.
- A test failing through `!` now reports its error code.
