# Snapshots that pin known bugs

A change that fixes one of these changes its snapshot; bless it and drop the line.

## Wrong output

- casts/int_to_float: `u64` max as f64 prints 18446744073709552640.0, not 18446744073709551616
- strings_printing/float_printing: floats print 6 fixed decimals with drift (1e-7 as 0.0, 1e300 wrong digits) instead of the shortest form that reads back
- constants/global_expressions, std/math: PI prints as 3.141593 (same float printing)

## Diagnostics

- arithmetic/divide_by_zero and every other panic snapshot, diagnostics/runtime_panic_location: the panic points at the start of the statement, not the failing expression
- generics/recursive_generic_method: eager checking of a generic struct's methods never ends on a method returning `Box<Box<T>>`; it is cut off with an error

## Pending a language decision

- optionals_results/ok_needs_context: `var r = Ok(1);` compiles, as an `i32!`
- control_flow/for_in_borrowed_vec: `for x in &v` is refused while `for x in &var v` works
- std/builtin_names_reserved: prelude helpers such as `syscall1` take names from user code
- parser/match_arm_without_comma, parser/trait_method_without_semicolon: separators are optional there
- lexer/non_ascii_identifier: non-ASCII identifiers are accepted while strings refuse non-ASCII
- lexer/nested_block_comment: block comments do not nest
