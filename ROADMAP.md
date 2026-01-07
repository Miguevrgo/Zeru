Zeru Roadmap: From Prototype to Production

# Warning

### This roadmap is an orientation, provided by LLM support along with my unexpertised guidance

This roadmap outlines the development path for Zeru, targeting a performance profile between C/Rust/Zig and C++ with modern safety features.

## Version 0.1.0: "The Solid Foundation"

Goal: A robust compiler supporting arithmetic, control flow, and complex data types (Structs and static Arrays) on the stack.

    Compiler Backend (CodeGen)

- [x] Professional README.md with installation guide, syntax examples, and this roadmap.
- [x] Field Access: Support reading/writing fields (point.x = 10;).
- [x] Refactor: Split expression compilation into L-Values (pointers) vs R-Values (values) to support complex assignments.
- [x] GitHub Actions
- [x] Aggregate Types: Implement Structs and Arrays.
- [x] Test Suite Integration: Expand the test suite.
- [x] Negative Tests: Ensure valid errors are thrown for invalid syntax or types (e.g., assigning string to int).
- [x] Benchmark: Measure compilation time vs. LOC (Lines of Code).
- [x] Code Cleanup: Run cargo clippy, remove dead code, and ensure 0 compiler warnings.

## Version 0.2.0: "The System Language"

Goal: Transition from a "calculator" to a true system language capable of interacting with the OS and managing memory manually.

##### Syntax & Parser

- [x] Pointers: Add * (dereference) and & (address-of) tokens.
- [x] Tuples: Support tuple types and expressions (i32, bool).
- [x] Mutable Parameters: Add `var` keyword for function parameters.
- [x] Optional Types: Add `T?` syntax for nullable types.
- [x] Enum Match Codegen: Generate code for match expressions with enum variants.
- [x] Friendly errors messages (ariadne integration)

##### Semantic Analyzer (Sema)

- [x] Pointer Validation: Strict type checking for pointers (*i32 vs i32).
- [x] Optional Type Checking: Basic optional type checking (T? accepts T and None).

##### CodeGen & Runtime

- [x] Freestanding Runtime: Direct syscalls via inline assembly (no libc dependency)
- [x] Exit Code Fix: Remove the explicit i32 return requirement for main; the runtime will handle exit(0).

##### Module System

- [x] Module imports: `import std.math` -> access via `math::func()`
- [x] Selective imports: `import std.math::{abs}` -> direct `abs()` access

## Version 0.3.0: "The Abstraction Layer"

Goal: Introduce high-level abstractions with zero runtime cost, moving closer to Rust/C++ capabilities.

##### Tooling

- [x] Raw strings support (backtick syntax: `` `raw string` ``)
- [x] Basic syntax highlighting and file icons (.zr with emoji support ⚡).
- [ ] LSP Initial: Basic Language Server Protocol scaffolding.

##### Generics (Templates)

- [x] Generic Structs: Support struct Box<T> { value: T }.
- [x] Generic Functions: Support fn id<T>(x: T) T.
- [x] Traits: Duck typing with trait definitions

##### Memory & Safety

- [x] Allocator trait API: Safe allocation interface (std/mem.zr)
- [ ] GlobalAlloc implementation with libc
- [ ] Arena allocator
- [ ] Consider possible solutions for easy/secure/fast language (debug|release-safe|release-fast)
- [ ] Generational References / Borrow Checker / Arenas ??

##### Standard Library (STL) - Phase 1

- [x] Option<T>: Nullable types with ? syntax
- [x] Result<T, E>: Tagged union for error handling
- [ ] Vec<T>: Implement dynamic arrays using malloc/realloc.

## Version 1.0.0: "Production Ready"

Goal: A safe, fast language with a complete developer ecosystem.

##### Ecosytem

- [x] Module System: robust import system for external libraries and multi-file projects.
- [ ] Full LSP: Autocomplete, "Go to Definition", and error diagnostics in the editor.
- [ ] Complete STL: Functional iterators (map, filter, reduce) and advanced math libraries.
