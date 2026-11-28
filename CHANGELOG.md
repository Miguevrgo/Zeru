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
- `v[a..=b]` slices through `b`.
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
- `var x = Vec.new()` (or `T.new()`, `with_capacity` of a generic struct) takes its type from the first use that says it.

### Tooling
- `zeru test` runs a `#test` block per file, each test alone; its asserts are checked in every build mode.
- `--native` builds for the processor compiling.
- `--release-fast` takes `+`, `-` and `*` as never overflowing, so loops optimize further.
- Warnings: unused, unreachable, redundant code; a `var` that never changes; a `var` parameter or `var self` that never changes (not in generic code); a cast to the type a value already has; division by zero; shorter forms (`is_empty`).
- Every error explains the lines involved and comes with a fix where one change fixes it.
- Errors recognise slips from other languages and suggest the name or import meant.
- `zeru fmt` keeps comments inside lists, calls, signatures and operator chains; wraps long generics.
- UI test snapshots keep the warnings of a build that succeeds.

### Std
- `std.term`: raw mode, cursor, clearing, `key(ms)`.
- `std.simd`: vectors as wide as the target's registers.
- `std.regex`: `compile`, then `is_match`, `find`, `find_all`, `captures`, `replace_all`; each search in linear time.
- `fs::embed_file(path)`: a file's bytes as a `str`, read while compiling, from the caller's folder.
- Vec `map`, `filter`, `any`, `all`, `sort_by`, `extend`; slices `join`; arrays have the slice methods.
- `str` has the slice methods (`sum`, `min`, `max`, `windows`, `chunks`) since it is a `&[u8]`.
- `str.is_empty()`, `len()` on arrays, `is_empty()` on arrays and slices.
- `str.parse()` reads the number type its result goes to, `i32` when nothing says; `parse<u8>()` names it.
- `format()` returns the text `print` would write.
- `str` and `String` can be HashMap and HashSet keys.
- `math::div_euclid` takes integers only.
- Heap blocks are cache-line aligned, large ones on huge pages.
- `sort` and `sort_by` are about 3x faster, more with many equal values.
- `format()` and printing long text are faster.
- Printing a float from 0.03 to 2.9e17 is about 4x faster.

### Removed
- `isize`.
- `push_str`: `extend` adds text to a String.
- `parse_int`, `parse_float`: `parse()` reads every number type.

### Fixed
- f32 literals could round to the wrong f32.
- The `&var` fix for a `for` over `zip` edited the wrong place.
- Boolean `if` simplifications preserve operator precedence.
- `fmt --check -` reports unformatted standard input.
- File metadata preserves timestamps throughout the representable nanosecond range.
- Process lookup reports permission denied for non-executable files in `PATH`.
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
- `<<=` and `>>=` in constants worked in the shift amount's type.
- An `if`/`match` value in unreachable code was typed void.
- A literal, literal arithmetic or literal branches on the left of an operator ignored the right side's type.
- A generic call with only literal arguments took `T` from a wanted `T?`.
- False borrow error removing from a map by a temporary key when values hold views.
- False borrow error extending a Vec from a temporary array whose elements hold views.
- False borrow error storing a slice of a `str` element back into its Vec.
- False borrow error from a closure that only reads a captured `&var`.
- False borrow error passing a `&var` where a `&` is wanted while looping over it.
- A constant comparing tuples or structs holding NaN disagreed with the program.
- The compiler's own `__zeru_*` functions could be called from a program.
- `zeru fmt` added a pair of parentheses around a parenthesised literal on every run.
- `for i, x in a..b` reported `i` as undeclared after its own error.
- An unreadable file was reported without its name.
- Constants built from other constants took exponential time and memory.
- A slice read the Vec's length before running its bounds.
- A constant array passed to a `var` parameter was written in read-only memory.
- `Vec[x; n]` evaluated `n` before `x`.
- `zeru fmt` moved a `{ // comment` into an earlier empty block.
- Crashes on a one-letter constant used as a `u8` and on errors inside parentheses.
- A private function could be passed as a value from another module.
- `x <<= 64` and `x /= 0` skipped the checks `x << 64` and `x / 0` get.
- A match guard followed by `->` named the wrong mistake.
- Nested `if` values in blocks took exponential time to parse.
- Thousands of warnings took quadratic time to print.
- File times after 2262 or before 1677 panicked `fs::metadata` and `fs::exists`.
- An empty entry in `PATH` was searched as `/` instead of the current directory.
- `zeru fmt` broke a long `if` value inside its arithmetic, and refused an `if` value nested in a branch.
- An unused `for` variable was reported on the sequence, without a fix.
- `&[1, 2]`, `&Vec[1, 2]` and `&Vec[x; n]` ignored the element type of the slice they were passed as.
- A literal shifted, as `(1 << n) - big`, ignored the other side's type.
- `Vec.from(..)` named the wrong type or asked for an element type; moving a borrowed loop element got a garbled label.
- A typed tuple declaration got a help with made-up names; a stray token in `#test { }` was called top level.
- A `for`, `while`, `var` or `const` as a match arm got no help.
- A value too wide for where it goes was fixed with a cast that drops its high bits.
