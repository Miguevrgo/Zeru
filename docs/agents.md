# Zeru for Agents

The intent of the language and what is settled, so nothing gets reinvented.
Details live in the book (`docs/src`), the compiler's design in
`docs/compiler.md`, and every change since the last release in `CHANGELOG.md`.

## Goal

Fast as C, safe like Rust where it is cheap, simple as Go. One way to do
each task. A small compiler a person can read. Correctness and speed of the
generated code come before fewer lines.

## Settled

- **Ownership.** A value that owns memory (`Vec`, `String`, anything holding
  one, a type with `drop`) moves and is freed when its owner ends. Plain data
  copies. `.copy()` is the explicit deep copy.
- **References.** `&T` and `&var T` are checked when compiling, with no
  lifetimes written. Aliasing and writes are allowed; only what invalidates
  the memory is forbidden: moving, freeing or reallocating it while a
  reference is still used. `*T` is raw and unchecked.
- **Errors.** `T?` and `T!` (an `i32` code). A value wraps itself, `x?` and
  `x!` return early, a `T!` is never ignored. No exceptions. Bugs panic with
  file and line.
- **Numbers.** No implicit conversion, `as` only. Literals are `i32` or
  `f64` unless the context says otherwise. Overflow panics; `+% -% *%` wrap.
- **Names.** `const` by default, `var` only for what changes.
- **Text.** `str` is bytes, `String` is a `Vec<u8>`. No char type, no `+`
  on text; `format()` builds it.
- **Generics.** Checked at each type they are used with. Bounds `Eq`, `Ord`,
  `Num`, `Hash`; traits are met by shape. No trait objects, no inheritance.
- **Closures.** `fn(x) x * 2`, capture by reference, passed or kept in a
  local, never stored or returned. No named function inside a function.
- **Modules.** A file is a module. `pub` exports. No import aliases.
- **Build modes.** debug and release-safe check indexes, overflow, division
  and `unwrap`. release-fast drops them: a program without bugs behaves the
  same, and a programmer's mistake there is undefined behaviour, on purpose.
- **Platform.** Linux x86-64-v2, one thread, static executables with no
  libc. No Windows, no threads.
- **Tooling.** `zeru build | run | test | check | fmt`. The language server
  lives in the `zeru-editors` repository with its own parser and must follow
  every language change; none of its code comes here.

## Refused, do not propose again

Nested block comments, import aliases, `+` for text, messages inside `T!`,
`unwrap_or`, labeled `break`, nested payload patterns, typing a literal from
its later use, a second way to do something that already has one, layouts
that make one type special, silencing warnings other than a `_` name.

## Working here

- Syntax, safety rules and design are Miguel's decisions: ask, never pick.
  Implementation, tests and cleanup are free.
- Std stays small: read all of `std/` before saying something is missing,
  and propose additions instead of adding them.
- Every change a user can see gets one terse line in `CHANGELOG.md`, and
  the book page it touches is updated.
- Tests are the snapshots in `tests/ui`. A test states an intention of the
  language: a rule, a diagnostic, a result that must hold. It never restates
  what a line of the compiler does, and no two tests state the same thing.
- A diagnostic says the real cause in the user's terms, with a help line and
  a fix when one edit solves it.
- Comments only as a doc on a type or a function whose name does not say it.
