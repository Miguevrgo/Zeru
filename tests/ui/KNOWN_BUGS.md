# Snapshots that pin known bugs

A change that fixes one of these changes its snapshot; bless it and drop the line.

## Diagnostics

- arithmetic/divide_by_zero and every other panic snapshot, diagnostics/runtime_panic_location: the panic points at the start of the statement, not the failing expression

## Pending a language decision

- lexer/nested_block_comment: block comments do not nest
