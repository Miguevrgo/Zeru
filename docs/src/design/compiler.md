# Compiler Architecture

The compiler is a pipeline of passes. Each pass reads the output of the one
before and produces something the next one reads; no pass revisits a decision
an earlier one made. The layout follows what rustc, Zig and Carbon do.

```text
source ─▶ lexer ─▶ parser ─▶ items ─▶ check ─▶ lower ─▶ clang
          tokens    AST      DefIds    typed IR  LLVM IR   executable
```

| Pass | Files | Produces |
|---|---|---|
| Driver | `driver.rs` | Loads the prelude, the root file and every imported module, runs the passes, links |
| Lexer, parser | `lexer.rs`, `parser.rs`, `ast.rs` | One syntax tree per file. The tree is never changed after parsing |
| Items | `items.rs` | A `DefId` for every function, method, struct, enum, trait and constant, and a scope per module |
| Check | `check/` | The typed IR of every function instance, or the errors |
| Lower | `lower/` | An LLVM module |

## Names

A name is resolved by looking it up, never by rewriting the source. Each module
has a scope from names to `DefId`s, split into types, traits and values. Code
reaches another module's items through `module::name`, or by the names a
selective import brings in. The prelude and the root file share the global
scope. A local shadows any item of the same name, because locals are looked up
first, by the checker.

## Types

Every type is a `Ty`, an index into the interning table of `ty.rs`. Comparing
two types is comparing two numbers. A generic struct at some arguments is
`Adt(def, args)`, never a copy of its declaration, and a type parameter is
`Param(i)`. Substitution maps indices to indices. `Types::show` prints a type
the way the programmer writes it, so messages never show internal names.

## Checking

`check/mod.rs` resolves item signatures and owns the queue of function
instances. An instance is a `DefId` with its type arguments; it is made the
first time something needs it and checked once. A generic function is checked
at each set of types it is called with, as Zig and C++ templates are. When a
generic instance fails, every error it reports carries a note pointing at the
call that asked for it.

`check/body.rs` checks one instance in a function context (`FnCx`), in the
manner of rustc's `FnCtxt`. Locals are numbered `LocalId`s. Which ones are
moved along each path is a vector indexed by them, merged at every join, so a
move inside a branch or a loop is tracked exactly. Expressions are in
`expr.rs`, calls and built-in methods in `call.rs`, `match` in `pattern.rs`,
and constant evaluation, at the width of each integer type, in `consts.rs`.

Two rules keep errors from cascading:

- An expression that failed to check has the type `Types::ERROR`. Every check
  accepts it silently, so one mistake is reported once.
- A value goes where a type is wanted only through `coerce`. That one function
  decides whether it fits, and records how: unchanged, retyped between pointer
  kinds, or wrapped in `Some`.

## The typed IR

`tir.rs` is the contract between checking and code generation. Every
expression carries its exact type. Every implicit conversion is a node of its
own, every call names the function instance it reaches, and every place
(`Local`, `Field`, `Index`, `Deref`, `Temp`) is explicit. Every move is a
`Move` and every drop of a replaced value an `Assign { drop_old }`. The
checker has decided all of it before code generation starts.

## Lowering

`lower/` translates the typed IR and makes no decision of its own. It follows
the memory model of rustc's code generation:

- Scalars (integers, floats, bools, pointers, enums without data) are SSA values.
- Everything else lives in memory and is passed around as a pointer to it,
  copied with `memcpy`. A big array or struct never becomes one huge LLVM value.
- An aggregate argument is a pointer to a copy the callee owns. An aggregate
  result is written through a pointer the caller passes first.
- A value that owns memory has a drop flag, raised while it owns the value and
  lowered by a `Move`. Each type gets its drop and copy functions once.
- Runtime checks (overflow, bounds, null, division, shifts, `unwrap`,
  float-to-int range) are emitted in debug and release-safe builds, and none in
  release-fast.

## Tests

`tests/ui` holds snapshot tests in the style of rustc's UI tests. Each `.zr`
file is built in every safety mode, run, and its status, output and
diagnostics compared against the `.out` file next to it. `BLESS=1 cargo test
--test ui` rewrites the snapshots, and a review of their diff shows exactly
what a change did to the language. `tests/ui/KNOWN_BUGS.md` lists the
snapshots that pin a known bug.
