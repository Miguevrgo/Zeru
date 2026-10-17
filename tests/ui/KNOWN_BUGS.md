# Snapshots that pin known bugs

A change that fixes one of these changes its snapshot; bless it and drop the line.

## Diagnostics

- arithmetic/divide_by_zero and every other panic snapshot, diagnostics/runtime_panic_location: the panic points at the start of the statement, not the failing expression

## Pending a language decision

- optionals_results/ok_needs_context: `var r = Ok(1);` compiles, as an `i32!`
- control_flow/for_in_borrowed_vec: `for x in &v` is refused while `for x in &var v` works
- parser/match_arm_without_comma, parser/trait_method_without_semicolon: separators are optional there
- lexer/non_ascii_identifier: non-ASCII identifiers are accepted while strings refuse non-ASCII
- lexer/nested_block_comment: block comments do not nest
