# Errors

Zeru has no exceptions. A function that may have nothing to give returns
an optional, `T?`; one that may fail returns a result, `T!`. Both are
ordinary values, so the type of a function tells you how it can go wrong,
and the compiler makes sure you take the value out before using it.

Mistakes in the program itself, like an index past the end of a list,
are not errors to handle: they stop the program with a panic.
