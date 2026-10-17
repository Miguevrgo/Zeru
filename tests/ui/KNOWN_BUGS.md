# Snapshots that pin known bugs

A refactor that fixes one of these changes its snapshot; bless it and drop the line.

## Crashes and wrong code

- generics/recursive_generic_method: the compiler overflows its stack on a method returning `Box<Box<T>>`
- structs/qualified_call_of_var_self_method: `S.bump(s)` passes sema and fails the LLVM verifier
- optionals_results/ok_needs_context: `var r = Ok(1);` compiles with no Result type to give it
- casts/int_to_float: `u64` max as f64 prints 18446744073709552640.0, not 18446744073709551616
- strings_printing/float_printing: floats print 6 fixed decimals with drift (1e-7 as 0.0, 1e300 wrong digits) instead of the shortest form that reads back
- constants/global_expressions, std/math: PI prints as 3.141593 (same float printing)

## Accepted or rejected wrongly

- modules/qualified_trait_bound: `T: lib::Go` does not parse
- generics/duplicate_type_parameters: `fn f<T, T>` and `struct B<T, T>` are accepted
- modules/private_trait: a private trait can be imported and used
- modules/unknown_selected_name: importing a name the module lacks is accepted
- generics/expected_type_does_not_infer: `var r: i64 = id(5)` is refused; the annotation does not bind T
- generics/optional_expected_generic_literal: `var o: Box<i64>? = Box { v: 3 }` is refused
- generics/none_before_binding_argument: `two(None, n)` fails while the argument after binds T
- generics/constant_expression_binds_first: only bare literals bind last; `g(1 + 1, x_i64)` is refused
- optionals_results/optional_of_tuple: a tuple literal ignores an optional expected type
- optionals_results/optional_array_with_none_second: `[x, None]` as `Array<i64?, 2>` is refused, `[None, x]` works
- control_flow/while_true_needs_dead_return: `while true` without `break` is not a terminating statement
- control_flow/for_in_borrowed_vec: `for x in &v` is refused while `for x in &var v` works
- traits/bound_return_type_not_checked: a bound checks method names, not return types; the error lands in the generic body
- constants/constant_division_by_zero, constants/constant_min_rem_minus_one: constant `5 / 0` and `-128 % -1` only fail at runtime

## Diagnostics

- arithmetic/divide_by_zero and every other panic snapshot, diagnostics/runtime_panic_location: the panic points at the start of the statement, not the failing expression
- diagnostics/argument_type, generics/int_literal_with_float, generics/constant_expression_binds_first: "Argument N type mismatch" names neither type
- diagnostics/unknown_inferred_cascade: two follow-up "Unknown type '<unknown>'" errors at the generic declaration
- generics/none_before_binding_argument: "Unknown type 'T'. Did you mean 'i8'?" three times at the declaration
- generics/method_parameter_reuses_struct_parameter: two follow-up errors after the real one
- generics/mangled_name_in_message, generics/optional_expected_generic_literal, generics/method_parameter_reuses_struct_parameter: mangled names such as `Box__i64_` in messages
- generics/unknown_trait_in_bound: the unknown trait is reported at the use, not at the bound
- std/math_abs_unsigned_rejected: the rejection points inside std/math.zr, not at the call
- std/builtin_struct_name_reserved: a user struct named like a prelude one cascades into errors inside std/builtin.zr
- std/builtin_names_reserved: prelude helpers such as `syscall1` take names from user code
- modules/same_last_name: `import a.util; import b.util;` is reported as a duplicate function inside b/util.zr
- types/tuple_length: one mismatch is reported twice
- types/array_length_not_a_number: a follow-up mismatch against `Array<i32, 0>`
- diagnostics/declaration_errors: errors are not reported in source order
- types/unknown_generic_type: "Unknown generic type or invalid args: Wrapper" breaks the "Unknown type 'X'" style
- arithmetic/logical_needs_bool: `i32 && bool` says "requires operands of same type" instead of naming bool
- structs/unknown_method: "Function 'Point::unknown_method' not defined" for a missing method
- constants/cannot_reassign, constants/parameter_is_constant, control_flow/for_in_element_is_constant, vec/element_constant_in_for: the message ends with a period, unlike the rest
- control_flow/while_condition_must_be_bool: "got: i32" where the if message says "got i32"

## Pending a language decision

- parser/match_arm_without_comma, parser/trait_method_without_semicolon: separators are optional there
- lexer/non_ascii_identifier: non-ASCII identifiers are accepted while strings refuse non-ASCII
- lexer/nested_block_comment: block comments do not nest
