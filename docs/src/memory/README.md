# Memory

Zeru has no garbage collector, and you never free memory by hand. Every
value has one owner, a variable, a field or an element, and the value is
freed when its owner goes away. The compiler tracks owners and references
as it compiles, so a program cannot read memory that was already freed or
moved somewhere else.

Three rules cover it:

1. Values that own memory, like a `Vec` or a `String`, have one owner at a
   time: assigning them moves them. Plain data, like numbers, is copied.
2. A reference (`&x` or `&var x`) lets code use a value without owning it.
   While a reference is in use, the value it points into cannot be freed
   or moved.
3. When an owner goes out of scope, what it owns is freed, after its
   `drop` method runs if it has one.

The next pages go through each one.
