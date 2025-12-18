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
- [ ] FFI (Foreign Function Interface): Add extern "C" support to link against C libraries.
- [x] Friendly errors messages (ariadne integration)

##### Semantic Analyzer (Sema)

- [x] Pointer Validation: Strict type checking for pointers (*i32 vs i32).
- [ ] Proper Main: Allow fn main() void.
- [x] Optional Type Checking: Basic optional type checking (T? accepts T and None).

##### CodeGen & Runtime

- [ ] LibC Linking: Enable calls to malloc, free, exit, and printf || Enable calls not depending on libc.
- [ ] Exit Code Fix: Remove the explicit i32 return requirement for main; the runtime will handle exit(0).

##### Tooling

- [ ] Basic syntax highlighting and file icons (.zr with emoji support ⚡).
- [ ] LSP Initial: Basic Language Server Protocol scaffolding.

## Version 0.3.0: "The Abstraction Layer"

Goal: Introduce high-level abstractions with zero runtime cost, moving closer to Rust/C++ capabilities.

##### Generics (Templates)

- [ ] Generic Structs: Support struct Box<T> { value: T }.
- [ ] Generic Functions: Support fn id<T>(x: T) T.
- [ ] Monomorphization: Compiler generates specialized code for each type used (like C++ templates).

##### Memory & Safety

- [ ] Consider possible solutions for easy/secure/fast language (debug|release-safe|release-fast)
- [ ] Generational References / Borrow Checker / Arenas ??

##### Standard Library (STL) - Phase 1

- [ ] Option & Result: Implement tagged unions (enum with data) for error handling.
- [ ] Vec<T>: Implement dynamic arrays using malloc/realloc.

## Version 1.0.0: "Production Ready"

Goal: A safe, fast language with a complete developer ecosystem.

##### Ecosytem

- [ ] Module System: robust import system for external libraries and multi-file projects.
- [ ] Full LSP: Autocomplete, "Go to Definition", and error diagnostics in the editor.
- [ ] Complete STL: Functional iterators (map, filter, reduce) and advanced math libraries.
