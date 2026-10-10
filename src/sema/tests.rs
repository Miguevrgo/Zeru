use crate::errors::{Span, ZeruError};
use crate::parser::Parser;
use crate::{lexer::Lexer, sema::analyzer::SemanticAnalyzer};

fn analyze(input: &str) -> Vec<String> {
    analyze_errors(input)
        .into_iter()
        .map(|e| e.message)
        .collect()
}

fn analyze_errors(input: &str) -> Vec<ZeruError> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        panic!(
            "Parser errors in test: {:?}",
            parser.errors.iter().map(|e| &e.message).collect::<Vec<_>>()
        );
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&mut program);
    analyzer.errors
}

#[test]
fn test_declaration_errors_point_at_their_source() {
    for input in [
        "fn f(a: Strng) { } fn main() { }",
        "fn f() { } fn f() { } fn main() { }",
        "fn f(a: i32, a: i32) { } fn main() { }",
        "fn main(a: i32) { }",
        "fn main() i32 { return 0; }",
        "fn f(a: void) { } fn main() { }",
        "fn f() i32 { } fn main() { }",
        "struct S { a: Strng } fn main() { }",
        "fn main() { var a: Array<i32, i32> = [1]; }",
        "fn main() { var a: Wrapper<i32> = 1; }",
    ] {
        let errors = analyze_errors(input);
        assert!(!errors.is_empty(), "{input} was accepted");
        for error in errors {
            assert_ne!(error.span, Span::default(), "{input}: {}", error.message);
        }
    }
}

#[test]
fn test_type_mismatch() {
    let input = "
            fn main() {
                var x: i32 = true;
            }
        ";
    let errors = analyze(input);
    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_undeclared_variable() {
    let input = "
            fn main() {
                var x = y;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Undeclared variable"));
}

#[test]
fn test_constants_cannot_change() {
    for (input, message) in [
        (
            "fn main() { const x = 10; x = 20; }",
            "Cannot reassign constant variable 'x'",
        ),
        (
            "const G: i32 = 100; fn main() { G = 200; }",
            "Cannot reassign constant variable 'G'",
        ),
        (
            "fn f(x: i32) i32 { x += 1; return x; } fn main() { }",
            "Cannot reassign constant variable 'x'",
        ),
        (
            "fn main() { var a: Array<i32, 2> = [1, 2]; for item in a { item = 9; } }",
            "Cannot reassign constant variable 'item'",
        ),
        (
            "struct C { v: i32, fn t(self) { self.v = self.v + 1; } } fn main() { }",
            "Cannot modify 'self', which is not declared 'var'",
        ),
        (
            "struct C { v: i32, fn t(self) { self.v += 1; } } fn main() { }",
            "Cannot modify 'self', which is not declared 'var'",
        ),
    ] {
        let errors = analyze(input);
        assert_eq!(errors.len(), 1, "{input}: {errors:?}");
        assert!(errors[0].contains(message), "{input}: {errors:?}");
    }
}

#[test]
fn test_integer_type_assignment_mismatch() {
    let input = "
            fn main() {
                var x: i32 = 10;
                var y: i64 = x;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].to_lowercase().contains("type mismatch"));
}

#[test]
fn test_float_to_int_mismatch() {
    let input = "
            fn main() {
                var x: i32 = 3.14;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_int_literal_makes_a_float_only_when_exact() {
    assert!(analyze("fn main() { var x: f32 = 10; var y: f64 = -3; var z = 2.5 * 2; }").is_empty());
    let errors = analyze("fn main() { var x: f32 = 16777217; }");
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("has no exact f32 value"));
    let errors = analyze("fn main() { var i: i32 = 10; var x: f32 = i; }");
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_boolean_type_mismatch() {
    let input = "
            fn main() {
                var x: bool = 42;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_string_type_mismatch() {
    let input = "
            fn main() {
                var s: *u8 = 42;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_mixed_type_arithmetic_error() {
    let input = "
            fn main() {
                var a: i32 = 10;
                var b: f32 = 2.5;
                var c = a + b;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("same type"));
}

#[test]
fn test_logical_with_non_bool_error() {
    let input = "
            fn main() {
                var a: i32 = 10;
                var b = true;
                var c = a && b;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_valid_numeric_base_literals() {
    let input = "
            fn main() {
                var octal: i32 = 0o1047;
                var binary: i32 = 0b010110;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_compound_assignment_operators() {
    let input = "
            fn main() {
                var x: i32 = 10;
                x += 5;
                x -= 3;
                x *= 2;
                x /= 4;
                x %= 3;
                x &= 7;
                x |= 1;
                x ^= 2;
                x <<= 1;
                x >>= 1;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_compound_assignment_type_mismatch() {
    let input = "
            fn main() {
                var x: i32 = 10;
                x += 3.14;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_struct_field_access_error() {
    let input = "
            struct Point { x: f32 }
            fn main() {
                var p = Point { x: 1.0 };
                var val = p.z;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("has no field"));
}

#[test]
fn test_struct_missing_field() {
    let input = "
            struct Point { x: f32, y: f32 }
            fn main() {
                var p = Point { x: 1.0 };
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Missing field"));
}

#[test]
fn test_struct_extra_field() {
    let input = "
            struct Point { x: f32 }
            fn main() {
                var p = Point { x: 1.0, y: 2.0 };
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Unknown field"));
}

#[test]
fn test_struct_field_type_mismatch() {
    let input = "
            struct Point { x: f32, y: f32 }
            fn main() {
                var p = Point { x: true, y: 2.0 };
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_struct_field_assignment_type_mismatch() {
    let input = "
            struct Point { x: f32, y: f32 }
            fn main() {
                var p = Point { x: 1.0, y: 2.0 };
                p.x = false;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_undeclared_struct() {
    let input = "
            fn main() {
                var p = UnknownStruct { x: 1.0 };
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_writes_need_a_var_root() {
    for body in [
        "const p = P { x: 1 }; p.x = 2;",
        "const a: Array<i32, 2> = [1, 2]; a[0] = 3;",
        "const v: Vec<i32> = Vec.new(); v.push(1);",
        "const p = P { x: 1 }; p.bump();",
        "const p = P { x: 1 }; var r = &var p.x;",
    ] {
        let input = format!(
            "struct P {{ x: i32, fn bump(var self) {{ self.x += 1; }} }}
             fn main() {{ {body} }}"
        );
        let errors = analyze(&input);
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(
            errors[0].contains("not declared 'var'"),
            "{body}: {errors:?}"
        );
    }
}

#[test]
fn test_writes_through_a_pointer_are_the_pointees() {
    let input = "
            struct P { x: i32, fn bump(var self) { self.x += 1; } }
            fn f(p: *P) { p.x = 2; p.bump(); }
            fn main() { var p = P { x: 1 }; f(&p); }
        ";
    assert!(analyze(input).is_empty());
}

#[test]
fn test_borrowed_values_cannot_be_moved() {
    for (input, name) in [
        (
            "struct S { v: Vec<i32>, fn take(self) S { return self; } } fn main() { }",
            "self",
        ),
        (
            "fn eat(v: Vec<i32>) { }
             fn main() { var vv: Vec<Vec<i32>> = Vec.new(); for v in vv { eat(v); } }",
            "v",
        ),
    ] {
        let errors = analyze(input);
        assert_eq!(errors.len(), 1, "{errors:?}");
        assert!(errors[0].contains(&format!("Cannot move '{name}', which is borrowed")));
    }
}

#[test]
fn test_functions_see_constants_declared_below() {
    let input =
        "fn f() i32 { return N; } const N: i32 = M + 1; const M: i32 = 3; fn main() { f(); }";
    assert!(analyze(input).is_empty(), "{:?}", analyze(input));
}

#[test]
fn test_f32_does_not_initialise_f64() {
    let errors = analyze("fn main() { var a: f32 = 1.5; var b: f64 = a; }");
    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("Annotated as f64 but got f32"));
}

#[test]
fn test_an_error_is_not_echoed_by_what_uses_it() {
    for input in [
        "fn main() { var x = nope(3); }",
        "fn main() { var x = 1; var y = x.foo(); }",
        "fn main() { var v: Vec<i32> = Vec.new(); var w = v; v.push(1); }",
    ] {
        let errors = analyze(input);
        assert_eq!(errors.len(), 1, "{input}: {errors:?}");
    }
}

#[test]
fn test_method_not_found() {
    let input = "
            struct Point { x: f32 }
            fn main() {
                var p = Point { x: 1.0 };
                p.unknown_method();
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_call_takes_as_many_arguments_as_declared() {
    for (body, message) in [
        ("add(10);", "Function 'add' expects 2 arguments, got 1"),
        ("add(1, 2, 3);", "Function 'add' expects 2 arguments, got 3"),
        (
            "var x: i32 = add(1);",
            "Function 'add' expects 2 arguments, got 1",
        ),
    ] {
        let errors = analyze(&format!(
            "fn add(a: i32, b: i32) i32 {{ return a + b; }} fn main() {{ {body} }}"
        ));
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(errors[0].contains(message), "{body}: {errors:?}");
    }
}

#[test]
fn test_function_call_arg_type_mismatch() {
    let input = "
            fn process(x: i32) { }
            fn main() {
                process(3.14);
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("type mismatch"));
}

#[test]
fn test_undefined_function_call() {
    let input = "
            fn main() {
                undefined_function();
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("not defined"));
}

#[test]
fn test_condition_must_be_bool() {
    for (body, message) in [
        ("if i { }", "If condition must be boolean, got i32"),
        ("while i { }", "While condition must be boolean, got: i32"),
    ] {
        let errors = analyze(&format!("fn main() {{ var i: i32 = 1; {body} }}"));
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(errors[0].contains(message), "{body}: {errors:?}");
    }
}

#[test]
fn test_break_and_continue_need_a_loop() {
    for body in ["break;", "continue;"] {
        let errors = analyze(&format!("fn main() {{ {body} }}"));
        assert_eq!(
            errors,
            ["Break/Continue can only be used inside loops"],
            "{body}"
        );
    }
}

#[test]
fn test_array_element_type_mismatch() {
    let input = "
            fn main() {
                var arr: Array<i32, 3> = [1.0, 2.0, 3.0];
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_array_length_mismatch() {
    let input = "
            fn main() {
                var arr: Array<i32, 5> = [1, 2, 3];
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_match_with_different_arm_types() {
    let input = "
            fn main() {
                var x: i32 = 1;
                var result = match x {
                    0 => 100,
                    1 => true,
                    default => 0
                };
            }
        ";
    let errors = analyze(input);

    assert!(!errors.is_empty());
}

#[test]
fn test_not_operator_on_numbers() {
    assert!(analyze("fn main() { var x: u8 = 10; var y: u8 = !x; var b = !true; }").is_empty());
    let errors = analyze("fn main() { var f = !1.5; }");
    assert!(errors[0].contains("'!' applies to a bool or an integer, not f64"));
}

#[test]
fn test_variable_out_of_scope() {
    let input = "
            fn main() {
                {
                    var x: i32 = 10;
                }
                var y = x;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Undeclared variable"));
}

#[test]
fn test_empty_struct() {
    let input = "
            struct Empty { }
            fn main() {
                var e = Empty { };
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_similar_variable_name_suggestion() {
    let input = "
            fn main() {
                var counter: i32 = 0;
                var x = countr;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());

    assert!(errors[0].contains("counter") || errors[0].contains("Did you mean"));
}

#[test]
fn test_nested_negation() {
    let input = "
            fn main() {
                var x: i32 = 10;
                var y: i32 = --x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_dereference_non_pointer_error() {
    let input = "
            fn main() {
                var x: i32 = 42;
                var y: i32 = *x;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Cannot dereference type"));
}

#[test]
fn test_address_of_temporary_error() {
    let input = "
            fn main() {
                var ptr: *i32 = &42;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Cannot create reference to a temporary value"));
}

#[test]
fn test_tuple_type_mismatch() {
    let input = "
            fn main() {
                var t: (i32, bool) = (true, 42);
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_tuple_length_mismatch() {
    let input = "
            fn main() {
                var t: (i32, bool, f64) = (42, true);
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_pointer_steps_by_an_integer_literal() {
    let input =
        "fn main() { var x: u8 = 0; var p: *u8 = &x; var n: *u8 = p + 1; var b: *u8 = p - 1; }";
    assert!(analyze(input).is_empty(), "{:?}", analyze(input));
}

#[test]
fn test_pointer_arithmetic_invalid_mul() {
    let input = "
        fn main() {
            var ptr: *u8 = \"hello\";
            var bad: *u8 = ptr * 2;
        }
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty(), "ptr * int should be invalid");
}

#[test]
fn test_str_type_is_distinct_from_pointer_u8() {
    let input = "
        fn main() {
            var s1: *u8 = \"hello\";
            var s2: str = s1;
        }
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty(), "str and *u8 should be distinct types");
}

#[test]
fn test_main_can_return_early() {
    assert!(analyze("fn main() { return; }").is_empty());
}

#[test]
fn test_optional_none_requires_context() {
    let input = "
        fn main() {
            var x = None;
        }
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_main_returns_void() {
    for (ret, value) in [("i32", "0"), ("f32", "3.14")] {
        let errors = analyze(&format!("fn main() {ret} {{ return {value}; }}"));
        assert_eq!(
            errors,
            [format!("Function 'main' must return void, not {ret}")]
        );
    }
}

#[test]
fn test_type_suggestion_typo() {
    let input = "
        fn main() {
            var x: i3 = 10;
        }
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Did you mean") || errors[0].contains("Unknown type"));
}

#[test]
fn test_suggestion_does_not_skip_a_prefix_for_free() {
    let input = "
        struct HashMapEntry { key: i32 }
        fn main() {
            var x: Entry = 10;
        }
    ";
    let errors = analyze(input);
    assert_eq!(errors, ["Unknown type 'Entry'"]);
}
#[test]
fn test_generic_function_with_bound() {
    let input = "
        trait Printable {
            fn print(self);
        }
        fn show<T: Printable>(x: T) T {
            return x;
        }
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "Generic function with trait bound should be valid: {:?}",
        errors
    );
}

#[test]
fn test_generic_type_param_in_body() {
    let input = "
        fn first<T>(x: T, y: T) T {
            var result: T = x;
            return result;
        }
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "Using T inside function body should work: {:?}",
        errors
    );
}

#[test]
fn test_return_gives_the_declared_type() {
    for (function, message) in [
        (
            "fn f() i32 { }",
            "Function 'f' can finish without returning a value",
        ),
        (
            "fn f(n: i32) bool { if n > 0 { return true; } }",
            "Function 'f' can finish without returning a value",
        ),
        (
            "fn f() i32 { return; }",
            "Function expects i32, returning void",
        ),
        (
            "fn f() i32 { return true; }",
            "Function expects i32, returning bool",
        ),
    ] {
        let errors = analyze(&format!("{function} fn main() {{ }}"));
        assert_eq!(errors.len(), 1, "{function}: {errors:?}");
        assert!(errors[0].contains(message), "{function}: {errors:?}");
    }
}

#[test]
fn test_return_on_every_branch_is_accepted() {
    let cases = [
        "fn f(n: i32) i32 { if n > 0 { return 1; } else { return 2; } }",
        "fn f(n: i32) i32 { if n > 0 { return 1; } return 2; }",
        "fn f(n: i32) i32 { while n > 0 { return 1; } return 0; }",
        "fn f() { }",
    ];
    for case in cases {
        let errors = analyze(&format!("{case} fn main() {{ }}"));
        assert!(errors.is_empty(), "{case} -> {errors:?}");
    }
}

#[test]
fn test_self_referential_struct_is_rejected() {
    for input in [
        "struct S { next: S } fn main() { }",
        "struct A { b: B } struct B { a: A } fn main() { }",
        "struct S { kids: Array<S, 2> } fn main() { }",
    ] {
        let errors = analyze(input);
        assert!(
            errors
                .iter()
                .any(|e| e.contains("stores itself, so it has no finite size")),
            "{input} -> {errors:?}"
        );
    }
}

#[test]
fn test_duplicate_declarations_are_rejected() {
    for (declarations, message) in [
        ("struct S { x: i32, x: i32 }", "declares field 'x' twice"),
        ("fn f(a: i32, a: i32) { }", "declares parameter 'a' twice"),
        ("enum E { A, A }", "declares variant 'A' twice"),
        (
            "struct Foo { x: i32 } struct Foo { y: i32 }",
            "Type Foo is already defined",
        ),
        (
            "enum Color { Red } enum Color { Blue }",
            "Type 'Color' is already defined",
        ),
        (
            "fn foo() { } fn foo() { }",
            "Function 'foo' is already defined",
        ),
        (
            "trait Foo { } trait Foo { }",
            "Trait 'Foo' is already defined",
        ),
        (
            "const LIMIT: i32 = 1; const LIMIT: i32 = 2;",
            "'LIMIT' is already defined",
        ),
        ("fn A() { } const A: i32 = 2;", "'A' is already defined"),
    ] {
        let errors = analyze(&format!("{declarations} fn main() {{ }}"));
        assert_eq!(errors.len(), 1, "{declarations}: {errors:?}");
        assert!(errors[0].contains(message), "{declarations}: {errors:?}");
    }
}

#[test]
fn test_indexing_is_checked() {
    for (input, message) in [
        (
            "fn main() { var a: Array<i32, 2> = [1, 2]; var v = a[true]; }",
            "Index must be an integer",
        ),
        (
            "fn main() { var a: Array<i32, 2> = [1, 2]; var v = a[5]; }",
            "Index 5 is outside the array's 0..2",
        ),
        (
            "fn f(p: *Array<i32, 4>) i32 { return p[0]; } fn main() { }",
            "Cannot index *Array<i32, 4> directly, write (*p)[i]",
        ),
        (
            "fn main() { var text: str = \"abc\"; text[0] = 65; }",
            "Cannot write through a slice",
        ),
    ] {
        let errors = analyze(input);
        assert!(
            errors.iter().any(|e| e.contains(message)),
            "{input}: {errors:?}"
        );
    }
}

#[test]
fn test_assignment_is_right_associative() {
    let errors = analyze("fn main() { var a: i32 = 0; var b: i32 = 0; a = b = 5; }");
    assert_eq!(
        errors,
        ["Type mismatch in assignment. Expected i32, got void."]
    );
}

#[test]
fn test_static_method_takes_a_dot() {
    let errors = analyze(
        "struct Box { n: i32, fn empty() Box { return Box { n: 0 }; } } fn main() { var b = Box::empty(); }",
    );
    assert_eq!(errors, ["Call a struct's function with '.': Box.empty()"]);
}

#[test]
fn test_assigning_to_a_temporary_is_rejected() {
    let cases = [
        "fn make() Array<i32,2> { return [1,2]; } fn main() { make()[0] = 5; }",
        "struct S { a: i32 } fn make() S { return S{a:1}; } fn main() { make().a = 5; }",
        "struct S { a: i32 } fn main() { S{a:1}.a = 5; }",
        "fn main() { 1 + 2 = 3; }",
    ];
    for input in cases {
        let errors = analyze(input);
        assert!(
            errors
                .iter()
                .any(|e| e.contains("Cannot assign to a temporary value")),
            "{input} -> {errors:?}"
        );
    }
}

#[test]
fn test_every_real_place_stays_assignable() {
    let cases = [
        "fn main() { var x: i32 = 0; x = 5; }",
        "struct S { a: i32 } fn main() { var s = S{a:1}; s.a = 5; }",
        "fn main() { var a: Array<i32,2> = [1,2]; a[0] = 5; }",
        "fn main() { var x: i32 = 1; var p: *i32 = &x; *p = 5; }",
        "struct S { a: i32 } fn f(p: *S) { p.a = 5; }",
        "struct S { d: Array<i32,2> } fn main() { var s = S{d:[1,2]}; s.d[1] = 5; }",
        "struct S { a: i32, fn m(var self) { self.a = 5; } }",
    ];
    for input in cases {
        let errors = analyze(&format!("{input} fn main2() {{ }}"));
        assert!(
            !errors.iter().any(|e| e.contains("temporary")),
            "{input} -> {errors:?}"
        );
    }
}

#[test]
fn test_bound_rejects_an_argument_that_does_not_meet_it() {
    let errors = analyze(
        "
        struct Pair<T: Eq> { first: T, second: T }
        struct Point { x: i32 }
        fn main() {
            var p = Point { x: 1 };
            var bad = Pair { first: p, second: p.copy() };
        }
    ",
    );
    assert!(
        errors.iter().any(|e| e.contains("asks for T: Eq")),
        "expected the bound to be reported: {errors:?}"
    );
}

#[test]
fn test_unknown_trait_in_a_bound_is_reported() {
    let errors = analyze(
        "
        struct Pair<T: Nonesuch> { first: T, second: T }
        fn main() { var p = Pair { first: 1, second: 2 }; }
    ",
    );
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Unknown trait 'Nonesuch'")),
        "expected the trait to be reported: {errors:?}"
    );
}

#[test]
fn test_cast_to_a_pointer_has_a_type() {
    let inferred = "fn main() { var x: i64 = 0; var p = x as *u8; var q: *u8 = p; }";
    assert!(analyze(inferred).is_empty(), "{:?}", analyze(inferred));

    let wrong = "fn main() { var x: i64 = 0; var q: bool = x as *u8; }";
    let errors = analyze(wrong);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("Type mismatch"), "{errors:?}");
}

#[test]
fn test_literal_that_does_not_fit_is_an_error() {
    for (declaration, message) in [
        ("var x: u64 = -1;", "Literal -1 does not fit in u64"),
        ("var x: usize = -5;", "Literal -5 does not fit in usize"),
        ("var x: i8 = -129;", "Literal -129 does not fit in i8"),
        ("var x: i8 = 128;", "Literal 128 does not fit in i8"),
        ("var x: u8 = 256;", "Literal 256 does not fit in u8"),
        ("var x: u8 = -1;", "Literal -1 does not fit in u8"),
        ("var x: i16 = 32768;", "Literal 32768 does not fit in i16"),
        ("var x: u16 = 65536;", "Literal 65536 does not fit in u16"),
        (
            "var x: i64 = 18446744073709551615;",
            "Literal 18446744073709551615 does not fit in i64",
        ),
        (
            "var x = 3000000000;",
            "Literal 3000000000 does not fit in i32, the type a literal takes",
        ),
    ] {
        let errors = analyze(&format!("fn main() {{ {declaration} }}"));
        assert_eq!(errors.len(), 1, "{declaration}: {errors:?}");
        assert!(errors[0].starts_with(message), "{declaration}: {errors:?}");
    }
}

#[test]
fn test_literal_at_the_edge_of_its_type_fits() {
    let input = "
        fn main() {
            var a: u64 = 18446744073709551615;
            var b: i64 = -9223372036854775808;
            var c: i8 = -128;
            var d: u8 = 255;
            var e = -2147483648;
            var f: i32? = -5;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_literal_on_the_left_takes_the_type_on_the_right() {
    let input = "
        fn main() {
            var x: u32 = 5;
            var below = 3 < x;
            var f: f32 = 1.5;
            var doubled: f32 = 2.0 * f;
            var neg: i64 = 7;
            var diff = -1 - neg;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_operator_needs_operands_it_applies_to() {
    for (expression, operand) in [
        ("true + true", "bool"),
        ("1 && 2", "i32"),
        ("p == q", "Point"),
        ("\"a\" == \"b\"", "str"),
        ("true < false", "bool"),
        ("1.5 & 2.5", "f64"),
        ("1.5 << 2.5", "f64"),
    ] {
        let input = format!(
            "struct Point {{ x: i32 }} fn main() {{ var p = Point {{ x: 1 }}; var q = Point {{ x: 1 }}; var r = {expression}; }}"
        );
        let errors = analyze(&input);
        let expected = format!("cannot be applied to {operand}");
        assert!(errors[0].contains(&expected), "{expression}: {errors:?}");
    }
}

#[test]
fn test_cast_needs_a_conversion_that_exists() {
    for (cast, message) in [
        ("p as i32", "Cannot cast P to i32"),
        ("p as *u8", "Cannot cast P to *u8"),
        ("1 as Color", "Cannot cast i32 to Color"),
        ("2.5 as Color", "Cannot cast f64 to Color"),
    ] {
        let input = format!(
            "struct P {{ x: i32 }} enum Color {{ Red }} fn main() {{ var p = P {{ x: 1 }}; var r = {cast}; }}"
        );
        let errors = analyze(&input);
        assert!(errors[0].contains(message), "{cast}: {errors:?}");
    }

    let fine = "
        enum Color { Red }
        fn main() {
            var a: u8 = 7;
            var b = a as f32;
            var c = Color::Red as i32;
            var d = true as i64;
            var e = 0 as *u8;
            var f = e as i64;
            var g = \"text\" as *u8;
        }
    ";
    let errors = analyze(fine);
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_match_patterns_are_checked() {
    for (arms, message) in [
        ("0 => 1, 1 => 2", "needs a 'default' arm"),
        (
            "1 => 1, 1 => 2, default => 3",
            "already matched by an earlier arm",
        ),
        (
            "y => 1, default => 2",
            "must be a literal, a constant or an enum variant",
        ),
        (
            "true => 1, default => 2",
            "type bool cannot match a value of type i32",
        ),
        ("default => 1, default => 2", "one 'default' arm"),
    ] {
        let input = format!(
            "fn main() {{ var x: i32 = 1; var y: i32 = 1; var r = match x {{ {arms} }}; }}"
        );
        let errors = analyze(&input);
        assert_eq!(errors.len(), 1, "{arms}: {errors:?}");
        assert!(errors[0].contains(message), "{arms}: {errors:?}");
    }
}

#[test]
fn test_match_covers_every_variant_or_has_a_default() {
    let missing = "
        enum Color { Red, Green, Blue }
        fn main() { var c = Color::Red; var r = match c { Color::Red => 1 }; }
    ";
    let errors = analyze(missing);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(
        errors[0].contains("does not cover Color::Green, Color::Blue"),
        "{errors:?}"
    );

    let covered = "
        enum Color { Red, Green }
        fn main() {
            var c = Color::Red;
            var r = match c { Color::Red => 1, Color::Green => 2 };
            var b = true;
            var s = match b { true => 1, false => 0 };
        }
    ";
    let errors = analyze(covered);
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_global_constant_is_made_of_constants() {
    let fine = "
        enum Mode { Fast }
        const BASE: i32 = 4;
        const SPAN: i64 = (BASE * 2 + 1) as i64;
        const NAME: str = \"zeru\";
        const DEFAULT_MODE: Mode = Mode::Fast;
        fn main() { }
    ";
    let errors = analyze(fine);
    assert!(errors.is_empty(), "{errors:?}");

    let called = "fn base() i32 { return 4; } const BASE: i32 = base(); fn main() { }";
    let errors = analyze(called);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(
        errors[0].starts_with("A global constant must be made of"),
        "{errors:?}"
    );
}

#[test]
fn test_generic_function_is_checked_at_its_types() {
    let input = "
        struct P { x: i32 }
        fn bigger<T>(a: T, b: T) T {
            if a > b { return a; }
            return b;
        }
        fn main() {
            var p = P { x: 1 };
            var q = P { x: 2 };
            var r = bigger(p, q);
        }
    ";
    let errors = analyze(input);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("cannot be applied to P"), "{errors:?}");
}

#[test]
fn test_every_way_of_giving_a_value_away_moves_it() {
    for given_away in [
        "var s = S { v: a };",
        "var row = [a];",
        "var pair = (a, 1);",
        "var outer: Vec<Vec<i64>> = Vec.new(); outer.push(a);",
        "var b: Vec<i64> = Vec.new(); b = a;",
    ] {
        let input = format!(
            "struct S {{ v: Vec<i64> }} fn take(v: Vec<i64>) {{ }} fn main() {{ var a: Vec<i64> = Vec.new(); {given_away} take(a); }}"
        );
        let errors = analyze(&input);
        assert_eq!(errors.len(), 1, "{given_away}: {errors:?}");
        assert!(
            errors[0].contains("Use of moved value 'a'"),
            "{given_away}: {errors:?}"
        );
    }

    let ok = "
        struct S { v: Vec<i64> }
        fn keep(s: S) { }
        fn main() { var s = S { v: Vec.new() }; var r: S! = Ok(s); keep(s); }
    ";
    let errors = analyze(ok);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("Use of moved value 's'"), "{errors:?}");
}

#[test]
fn test_moving_out_of_a_place_or_a_loop_is_refused() {
    for (body, message) in [
        (
            "var s = S { v: Vec.new() }; var v = s.v;",
            "Cannot move a value out of a field, an element or a pointer",
        ),
        (
            "var vs: Vec<S> = Vec.new(); var s = vs[0];",
            "out of a field, an element or a pointer",
        ),
        (
            "var a: Vec<i64> = Vec.new(); while true { var b = a; }",
            "Cannot move 'a' inside a loop",
        ),
    ] {
        let input = format!("struct S {{ v: Vec<i64> }} fn main() {{ {body} }}");
        let errors = analyze(&input);
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(errors[0].contains(message), "{body}: {errors:?}");
    }
}

#[test]
fn test_moves_that_are_fine_stay_fine() {
    let input = "
        struct S { v: Vec<i64> }
        const ROW: Array<i32, 2> = [1, 2];
        fn take(row: Array<i32, 2>) { }
        fn main() {
            var s = S { v: Vec.new() };
            var v = s.v.copy();
            while true {
                var fresh: Vec<i64> = Vec.new();
                var kept = fresh;
                break;
            }
            take(ROW);
            take(ROW);
            var a: Vec<i64> = Vec.new();
            var c = 1;
            var picked = match c { 1 => a, default => a };
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty(), "{errors:?}");
}

#[test]
fn test_drop_takes_only_var_self() {
    for method in [
        "fn drop(self) { }",
        "fn drop(var self, x: i32) { }",
        "fn drop(var self) i32 { return 0; }",
    ] {
        let errors = analyze(&format!("struct S {{ x: i32, {method} }} fn main() {{ }}"));
        assert_eq!(errors.len(), 1, "{method}: {errors:?}");
        assert!(errors[0].contains("A 'drop' method takes only 'var self'"));
    }
    assert!(analyze("struct S { x: i32, fn drop(var self) { } } fn main() { }").is_empty());
}

#[test]
fn test_get_cannot_copy_out_what_owns_memory() {
    let errors = analyze("fn main() { var v: Vec<Vec<i32>> = Vec.new(); var x = v.get(0); }");
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("Vec::get() cannot copy out a Vec<i32>"));
    assert!(analyze("fn main() { var v: Vec<i32> = Vec.new(); var x = v.get(0); }").is_empty());
}

#[test]
fn test_unwrap_moves_what_owns_memory() {
    let errors =
        analyze("fn main() { var o: Vec<i32>? = None; var a = o.unwrap(); var b = o.is_some(); }");
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("Use of moved value 'o'"));
    assert!(
        analyze("fn main() { var o: i32? = 1; var a = o.unwrap(); var b = o.unwrap(); }")
            .is_empty()
    );
}

#[test]
fn test_literal_fills_a_vec() {
    assert!(
        analyze(
            "fn main() { var v: Vec<i64> = [1, 2]; var z: Vec<u8> = [0; 9]; var e: Vec<i32> = []; }"
        )
        .is_empty()
    );
    let errors = analyze("fn main() { var v: Vec<i64> = [true]; }");
    assert_eq!(errors.len(), 1, "{errors:?}");
}

#[test]
fn test_new_vec_methods() {
    let input = "
        fn main() {
            var v: Vec<i64> = Vec.new();
            v.insert(0, 1);
            var x: i64 = v.remove(0);
            v.reserve(8);
            v.shrink_to_fit();
        }
    ";
    assert!(analyze(input).is_empty(), "{:?}", analyze(input));
    let errors = analyze("fn main() { var v: Vec<i64> = Vec.new(); v.insert(true, 1); }");
    assert_eq!(errors.len(), 1, "{errors:?}");
}

#[test]
fn test_moves_follow_the_control_flow() {
    let ok = [
        "var v: Vec<i32> = Vec.new(); while true { eat(v); break; }",
        "var v: Vec<i32> = Vec.new(); var i = 0; while i < 3 { eat(v); v = Vec.new(); i += 1; }",
        "var v: Vec<i32> = Vec.new(); var c = true; if c { eat(v); return; } eat(v);",
        "var p = P { x: 1 }; var q = p; var r = p;",
    ];
    for body in ok {
        let input =
            format!("struct P {{ x: i32 }} fn eat(v: Vec<i32>) {{ }} fn main() {{ {body} }}");
        assert!(analyze(&input).is_empty(), "{body}: {:?}", analyze(&input));
    }

    let rejected = [
        (
            "var v: Vec<i32> = Vec.new(); while true { eat(v); }",
            "Cannot move 'v' inside a loop",
        ),
        (
            "var v: Vec<i32> = Vec.new(); var c = true; while c { eat(v); continue; }",
            "Cannot move 'v' inside a loop",
        ),
        (
            "var v: Vec<i32> = Vec.new(); while true { eat(v); break; } eat(v);",
            "Use of moved value 'v'",
        ),
        (
            "var v: Vec<i32> = Vec.new(); var c = true; if c { eat(v); } eat(v);",
            "Use of moved value 'v'",
        ),
        (
            "var p = P { x: 1 }; var q = p.copy();",
            "P holds no memory of its own",
        ),
    ];
    for (body, message) in rejected {
        let input =
            format!("struct P {{ x: i32 }} fn eat(v: Vec<i32>) {{ }} fn main() {{ {body} }}");
        let errors = analyze(&input);
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(errors[0].contains(message), "{body}: {errors:?}");
    }
}

#[test]
fn test_loops_count_read_and_write() {
    let ok = "
        struct S {
            items: Vec<i64>,
            n: i32,
            fn go(var self) { for x in &var self.items { self.n += 1; x += 1; } }
        }
        fn main() {
            var total: u64 = 0;
            var n: u64 = 10;
            for i in 0..n { total += i; }
            var v: Vec<i64> = [1];
            for x in &var v { x = 2; }
            for x in v { var y = x + 1; }
            var h: u8 = 255;
            h +%= 1;
        }
    ";
    assert!(analyze(ok).is_empty(), "{:?}", analyze(ok));

    for (body, message) in [
        (
            "var v: Vec<i64> = [1]; for x in &var v { v.push(2); }",
            "Cannot change 'v' while a loop or match around it refers into 'v'",
        ),
        (
            "var v: Vec<i64> = [1]; for x in v { x = 2; }",
            "Cannot reassign constant variable 'x'",
        ),
        (
            "for i in 0..2.5 { }",
            "A range counts between two integers of one type",
        ),
        (
            "var f = 1.5 +% 2.0;",
            "Operator '+%' cannot be applied to f64",
        ),
    ] {
        let errors = analyze(&format!("fn main() {{ {body} }}"));
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(errors[0].contains(message), "{body}: {errors:?}");
    }
}

#[test]
fn test_print_formats() {
    assert!(analyze("fn main() { var x: u8 = 1; println(\"{} {} {} {{}}\", x, 2.5, true); print(\"plain\"); }").is_empty());
    for (body, message) in [
        (
            "var s = \"x\"; println(s);",
            "The format must be a string literal",
        ),
        (
            "println(\"{} {}\", 1);",
            "The format has 2 '{}' but 1 value(s) follow it",
        ),
        ("println(\"{\", 1);", "A brace in a format is"),
        (
            "var v: Vec<i32> = Vec.new(); println(\"{}\", v);",
            "Cannot print Vec<i32>",
        ),
    ] {
        let errors = analyze(&format!("fn main() {{ {body} }}"));
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(errors[0].contains(message), "{body}: {errors:?}");
    }
}

#[test]
fn test_errors_propagate_and_fall_back() {
    let ok = "
        enum E { A, B }
        fn f() i32!E { return Err(E::A); }
        fn g() i32!E { var x = try f(); return Ok(x + 1); }
        fn o() i32? { return None; }
        fn main() {
            var a = g() catch 0;
            var b = o() orelse 1;
            var e: E = f().unwrap_err();
        }
    ";
    assert!(analyze(ok).is_empty(), "{:?}", analyze(ok));

    for (input, message) in [
        (
            "fn f() i32! { return Ok(1); } fn main() { var x = try f(); }",
            "'try' passes its i32 error on",
        ),
        (
            "enum E { A } fn f() i32!E { return Err(E::A); } fn g() i32! { var x = try f(); return Ok(x); } fn main() { }",
            "'try' passes its E error on",
        ),
        (
            "fn f() i32? { return 1; } fn main() { var x = f() catch 0; }",
            "'catch' takes a T! value, not i32?",
        ),
        (
            "fn f() i32! { return Err(true); } fn main() { }",
            "Err() takes i32, got bool",
        ),
        (
            "fn f() i32!u8 { return Ok(1); } fn main() { }",
            "An error type is an enum, not u8",
        ),
    ] {
        let errors = analyze(input);
        assert_eq!(errors.len(), 1, "{input}: {errors:?}");
        assert!(errors[0].contains(message), "{input}: {errors:?}");
    }
}

#[test]
fn test_match_payloads_variants_blocks_and_constants() {
    let ok = "
        enum Shape { Circle(f64), Rect(f64, f64), Empty }
        const LIMIT: i32 = 10;
        fn area(s: Shape) f64 {
            return match s { Shape::Circle(r) => r * r, Shape::Rect(w, _) => w, Shape::Empty => 0.0 };
        }
        fn main() {
            var o: i64? = 5;
            var total: i64 = 0;
            match o {
                Some(v) => { total += v; }
                None => { }
            }
            var r: i32! = Err(4);
            var code = match r { Ok(v) => v, Err(e) => e };
            var n = match code { LIMIT => 1, default => 0 };
            var a = area(Shape::Circle(2.0));
        }
    ";
    assert!(analyze(ok).is_empty(), "{:?}", analyze(ok));

    for (body, message) in [
        (
            "var s = S::A(1); if s == S::B { }",
            "'==' does not apply to S",
        ),
        ("var s = S::A;", "'S::A' carries 1 value(s)"),
        ("var s = S::A(1, 2);", "'S::A' carries 1 value(s), got 2"),
        (
            "var s = S::B; var x = match s { S::A(v) => v };",
            "Match does not cover S::B",
        ),
        (
            "var o: i32? = 1; match o { Some(v) => { o = None; } None => { } }",
            "Cannot change 'o'",
        ),
        (
            "var o: i32? = 1; var x = match o { Some(1) => 1, default => 0 };",
            "A pattern binds names",
        ),
        (
            "var o: i32? = 1; var x = match o { Some(v) => 1 };",
            "Match does not cover None",
        ),
    ] {
        let errors = analyze(&format!("enum S {{ A(i32), B }} fn main() {{ {body} }}"));
        assert_eq!(errors.len(), 1, "{body}: {errors:?}");
        assert!(errors[0].contains(message), "{body}: {errors:?}");
    }
}
