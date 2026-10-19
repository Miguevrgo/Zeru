# Compiler Architecture

The compiler is a pipeline of passes. Each pass reads the output of the one
before and produces something the next one reads; no pass revisits a decision
an earlier one made. The layout follows what rustc, Zig and Carbon do.

```text
source ─▶ lexer ─▶ parser ─▶ items ─▶ check ─▶ mir ─▶ lower ─▶ LLVM ─▶ cc
          tokens    AST      DefIds    typed IR  MIR    LLVM IR  object  executable
```

| Pass | Files | Produces |
|---|---|---|
| Driver | `driver.rs` | Loads the prelude, the root file and every imported module, runs the passes, has LLVM optimize and emit an object file, and links it statically with no libc: `lower/start.s` gives the entry point and the few functions LLVM calls by name |
| Lexer, parser | `lexer.rs`, `parser.rs`, `ast.rs` | A token list per file, which the parser walks with one cursor into a syntax tree. The tree is never changed after parsing |
| Items | `items.rs` | A `DefId` for every function, method, struct, enum, trait and constant, and a scope per module |
| Check | `check/` | The typed IR of every function instance, or the errors |
| MIR | `mir/` | Each function as a graph of basic blocks, borrow checked, its drops elaborated |
| Lower | `lower/` | An LLVM module |
| Language server | `lsp/` | `zeru lsp`: the passes up to the borrow checker over the editor's text. With `Checker::record` set, the checker notes what each name resolves to, the type of each expression, each local and the locals in scope where a name resolves to nothing |

## Names

A name is resolved by looking it up, never by rewriting the source. Each module
has a scope from names to `DefId`s, split into types, traits and values. As in
Go and Zig, a module sees its own items first, then the names its selective
imports bring in, then the prelude's, and reaches the items of a module it
imports through `module::name`. No module sees the root file's items, and a
name imported twice is an error. A local shadows any item of the same name,
because locals are looked up first, by the checker.

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
manner of rustc's `FnCtxt`. Locals are numbered `LocalId`s. Expressions are
in `expr.rs`, calls and built-in methods in `call.rs`, `match` in
`pattern.rs`, and constant evaluation, at the width of each integer type, in
`consts.rs`.

The checker decides what each expression means, and nothing about where
values go afterwards:

- Whether a value is read or moved depends on its type: a value that owns
  memory is moved, plain data is copied. Moving out of a field, an element,
  a pointer or a view (a `match` binding, `self`) is an error here, since it
  needs no flow. Whether a moved local is used again is the borrow
  checker's question.
- As it walks, `FnCx::diverges` says whether control can reach the current
  point, as rustc's: a `return`, `break` or `continue` sets it, an `if`
  diverges when both branches do, a `match` when every arm does, and a
  `while true` with no `break` out of it. It decides that a function can
  finish without returning a value, and which arms of a `match` leave, so
  their type does not count.

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

## The MIR

`mir/` turns the typed IR of each function into a graph of basic blocks over
numbered locals, as rustc's MIR does. Local 0 is the return place, then come
the arguments, the variables and the temporaries. A statement assigns an
rvalue to a place or runs a built-in; a block ends in a terminator: a jump, a
switch on an integer, a call, a drop, a check, or a return.

`mir/build.rs` decides everything the tree left open, once:

- Every value an expression needs is evaluated, left to right, into an
  operand: a constant, or a temporary local. A place is projected where it is
  used, after its indices have been evaluated into locals, so nothing holds an
  address across code that may move what it points into.
- Assignment evaluates its right side before its target.
- A scope drops what it owns as it ends, and every exit (`return`, `break`,
  `continue`, `?`, `!`) drops what the scopes it leaves still own, temporaries
  of the statements it leaves first.
- `match` is a switch on the discriminant. Each binding is set as its arm
  is entered: a copy of a value that owns no memory, refers to nothing and
  is never borrowed, as Rust's bindings of `Copy` values, or else a
  reference into the value matched.
- `for x in seq` walks a reference from the first element to one past the
  last, as Rust's slice iterator; `x` is a copy of the element, or for
  `&var` or an element that owns memory, the element itself.
- A built-in method's receiver, and the base of an element whose index
  calls something, is reached through a reference taken before its
  arguments are evaluated. As with Rust's two-phase borrows, taking a
  `&var` changes nothing; what it is used for is checked where it is used.
- Every scope ends its locals with a `StorageDead`, after their drops, at
  its end and at every exit; a statement ends its temporaries.
- Runtime checks (bounds, null, `unwrap`) are `Assert` terminators, and the
  arithmetic, division, shift and float-to-int checks are marked on the
  operation; none exist in release-fast.

`mir/borrowck.rs` then checks the body (see below), and
`mir/elaborate.rs` decides each drop, as rustc's drop elaboration: a
forward dataflow over the blocks finds whether the local may hold a value
there and whether it may not. A drop runs, goes away, or tests a drop flag, a
bool local raised as the value arrives and lowered as it leaves.

## The borrow checker

`mir/borrowck.rs` checks references and moves on the MIR of each function,
in the manner of rustc's MIR borrowck. Zeru is single threaded, so a live
reference forbids only what can invalidate the memory it points into, never
aliasing or plain writes (`docs/src/design/memory.md`, section 2).

- **Loans.** `&place` and `&var place` take a loan; so do `get`, a `match`
  binding and a `for` loop, which the MIR writes as taking an address. A raw
  `*T` takes none. A reference parameter holds a loan on what it points at
  from the start.
- **Places.** A loan's place is a path: a local, then fields, elements,
  derefs and variants, the payload of a `T?` or `T!` counting as a variant.
  Through a reference a local holds, a path continues from the places that
  reference may point at, so an access through `*m` is the same access to
  what `m` borrows. A reference a call returns may point anywhere inside
  what its reference arguments point at, and takes a loan of its own, as a
  reborrow in rustc: paths through it compare exactly with paths through
  the same result, unless the call returned another that is still live.
- **Calls.** A call may do anything a `&var` lets it do: each `&var` its
  arguments carry, directly or inside a struct, a `T?` or a `self`, may
  reallocate what it points at, and every reference the arguments carry may
  be stored where such a `&var` points, when that can hold a reference.
  What a call is lent it may reach anywhere inside, so `f(&var v, &v)` is an
  error while `f(&var p.hp, &p.mana)` is fine.
- **Which locals hold which loans** is a forward dataflow: an assignment
  hands the destination the loans of the value, and a whole assignment or the
  end of a local's scope takes them away. A value holds a loan directly or
  through a part that is a reference.
- **Liveness** follows the locals that can hold a reference: a local is
  live where it may be used later, found by walking back from each use to
  the definitions that reach it. A loan is live where a local holding it is
  live or is reached through a live reference. A `drop` counts as a use
  only for a type with a `drop` method.
- **Moves.** A forward dataflow finds the moves out of a local that may have
  happened before each point; using the local there is an error.

With a loan live, it is an error to move out of a place that overlaps it,
to end the local it points into, and to drop, reallocate (the Vec mutators, a
call through a `&var`) or replace (for a variant) a place whose loan
continues under it through an owning step: an element of a Vec or a
variant's payload. Taking a `&var` is none of these; only what it is used
for is. Each error points at the access and has notes at the loan and at
the later use that keeps it live.

The work grows with the size of a function and how long its references
live, not with the square of its size.

## Lowering

`lower/` translates the MIR and makes no decision of its own. It follows
the memory model of rustc's code generation:

- Scalars (integers, floats, bools, pointers, enums without data) are SSA
  values: a temporary assigned once and never borrowed has no slot at all.
- Everything else lives in memory and is passed around as a pointer to it,
  copied with `memcpy`. A big array or struct never becomes one huge LLVM value.
- An aggregate argument is a pointer to a copy the callee owns, or to the local
  itself when it is moved. An aggregate result is written through a pointer the
  caller passes first.
- Each type gets its drop and copy functions once.

## Tests

`tests/ui` holds snapshot tests in the style of rustc's UI tests. Each `.zr`
file is built in every safety mode, run, and its status, output and
diagnostics compared against the `.out` file next to it. `BLESS=1 cargo test
--test ui` rewrites the snapshots, and a review of their diff shows exactly
what a change did to the language. `tests/ui/KNOWN_BUGS.md` lists the
snapshots that pin a known bug.
