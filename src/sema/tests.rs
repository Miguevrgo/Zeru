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
    // Each of these used to come out with no file or line at all.
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
fn test_variable_declaration() {
    let input = "
            fn main() {
                var x: i32 = 10;
                var y = x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_variable_shadowing() {
    let input = "
            fn main() {
                var x: i32 = 10;
                var x: bool = true;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_const_reassignment() {
    let input = "
            fn main() {
                const x = 10;
                x = 20;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Cannot reassign constant"));
}

#[test]
fn test_var_reassignment_allowed() {
    let input = "
            fn main() {
                var x = 10;
                x = 20;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_all_signed_integer_types() {
    let input = "
            fn main() {
                var a: i8 = 127;
                var b: i16 = 32767;
                var c: i32 = 2147483647;
                var d: i64 = 100;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_all_unsigned_integer_types() {
    let input = "
            fn main() {
                var a: u8 = 255;
                var b: u16 = 65535;
                var c: u32 = 100;
                var d: u64 = 100;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_i8_overflow() {
    let input = "
            fn main() {
                var x: i8 = 128;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("does not fit"));
}

#[test]
fn test_i8_underflow() {
    let input = "
            fn main() {
                var x: i8 = -129;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("does not fit"));
}

#[test]
fn test_u8_overflow() {
    let input = "
            fn main() {
                var x: u8 = 256;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("does not fit"));
}

#[test]
fn test_u8_negative_value() {
    let input = "
            fn main() {
                var x: u8 = -1;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("does not fit"));
}

#[test]
fn test_i16_overflow() {
    let input = "
            fn main() {
                var x: i16 = 32768;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
}

#[test]
fn test_u16_overflow() {
    let input = "
            fn main() {
                var x: u16 = 65536;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
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
fn test_float_types() {
    let input = "
            fn main() {
                var a: f32 = 3.14;
                var b: f64 = 2.718281828;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
    // A variable is not a literal: its type stays its own.
    let errors = analyze("fn main() { var i: i32 = 10; var x: f32 = i; }");
    assert!(errors[0].contains("Type mismatch"));
}

#[test]
fn test_boolean_type() {
    let input = "
            fn main() {
                var a: bool = true;
                var b: bool = false;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_string_type() {
    let input = "
            fn main() {
                var s: str = \"hello\";
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_integer_arithmetic() {
    let input = "
            fn main() {
                var a: i32 = 10;
                var b: i32 = 20;
                var sum = a + b;
                var diff = a - b;
                var prod = a * b;
                var quot = a / b;
                var rem = a % b;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_float_arithmetic() {
    let input = "
            fn main() {
                var a: f32 = 1.5;
                var b: f32 = 2.5;
                var sum = a + b;
                var diff = a - b;
                var prod = a * b;
                var quot = a / b;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_integer_comparisons() {
    let input = "
            fn main() {
                var a: i32 = 10;
                var b: i32 = 20;
                var eq = a == b;
                var neq = a != b;
                var lt = a < b;
                var gt = a > b;
                var leq = a <= b;
                var geq = a >= b;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_float_comparisons() {
    let input = "
            fn main() {
                var a: f32 = 1.0;
                var b: f32 = 2.0;
                var lt = a < b;
                var eq = a == b;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_boolean_equality() {
    let input = "
            fn main() {
                var a = true;
                var b = false;
                var eq = a == b;
                var neq = a != b;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_logical_operations() {
    let input = "
            fn main() {
                var a = true;
                var b = false;
                var and_res = a && b;
                var or_res = a || b;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_bitwise_operations() {
    let input = "
            fn main() {
                var a: i32 = 0xFF;
                var b: i32 = 0x0F;
                var and_res = a & b;
                var or_res = a | b;
                var xor_res = a ^ b;
                var shl = a << 2;
                var shr = a >> 2;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_bitwise_unsigned() {
    let input = "
            fn main() {
                var a: u32 = 255;
                var b: u32 = 15;
                var result = a & b;
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
fn test_struct_definition_and_usage() {
    let input = "
            struct Point { x: f32, y: f32 }
            fn main() {
                var p = Point { x: 1.0, y: 2.0 };
                var val = p.x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_struct_field_assignment() {
    let input = "
            struct Point { x: f32, y: f32 }
            fn main() {
                var p = Point { x: 1.0, y: 2.0 };
                p.x = 5.0;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_nested_struct() {
    let input = "
            struct Inner { val: i32 }
            struct Outer { inner: Inner }
            fn main() {
                var i = Inner { val: 42 };
                var o = Outer { inner: i };
                var x = o.inner.val;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_method_call() {
    let input = "
            struct Counter {
                val: i32,
                fn increment(var self) { self.val = self.val + 1; }
            }
            fn main() {
                var c = Counter { val: 0 };
                c.increment();
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_method_with_return_type() {
    let input = "
            struct Counter {
                val: i32,
                fn get_val(self) i32 { return self.val; }
            }
            fn main() {
                var c = Counter { val: 42 };
                var v: i32 = c.get_val();
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_method_with_parameters() {
    let input = "
            struct Calculator {
                result: i32,
                fn add(var self, x: i32) { self.result = self.result + x; }
            }
            fn main() {
                var calc = Calculator { result: 0 };
                calc.add(10);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_immutable_self_field_modification() {
    let input = "
            struct Counter {
                val: i32,
                fn try_increment(self) { self.val = self.val + 1; }
            }
            fn main() {
                var c = Counter { val: 0 };
                c.try_increment();
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Cannot modify 'self'"));
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
    let input = "fn f() i32 { return N; } const N: i32 = 3; fn main() { f(); }";
    assert!(analyze(input).is_empty());
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
fn test_function_definition_and_call() {
    let input = "
            fn add(a: i32, b: i32) i32 {
                return a + b;
            }
            fn main() {
                var result = add(1, 2);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_function_call_args_mismatch() {
    let input = "
            fn add(a: i32, b: i32) i32 { return a + b; }
            fn main() {
                add(10);
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("expects 2 arguments"));
}

#[test]
fn test_function_call_too_many_args() {
    let input = "
            fn add(a: i32, b: i32) i32 { return a + b; }
            fn main() {
                add(1, 2, 3);
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("expects 2 arguments"));
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
fn test_function_return_type_mismatch() {
    let input = "
            fn get_number() i32 {
                return true;
            }
            fn main() { }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Type mismatch") || errors[0].contains("return"));
}

#[test]
fn test_function_void_return() {
    let input = "
            fn do_nothing() {
                return;
            }
            fn main() {
                do_nothing();
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_function_missing_return_value() {
    let input = "
            fn get_number() i32 {
                return;
            }
            fn main() { }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
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
fn test_multiple_function_calls() {
    let input = "
            fn foo() u32 {
                return 65536;
            }

            fn fizz() {
                const unused: i32 = 5;
            }

            fn main() {
                const a: u32 = foo();
                fizz();
                var returned_val = a % 2;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_recursive_function() {
    let input = "
            fn factorial(n: i32) i32 {
                if n <= 1 {
                    return 1;
                }
                return n * factorial(n - 1);
            }
            fn main() {
                var result = factorial(5);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_if_statement() {
    let input = "
            fn main() {
                var x: i32 = 10;
                if x > 5 {
                    var y = x + 1;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_if_else_statement() {
    let input = "
            fn main() {
                var x: i32 = 10;
                if x > 5 {
                    var y = 1;
                } else {
                    var y = 2;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_if_condition_not_bool() {
    let input = "
            fn main() {
                var x: i32 = 10;
                if x {
                    var y = 1;
                }
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("bool"));
}

#[test]
fn test_while_loop() {
    let input = "
            fn main() {
                var i: i32 = 0;
                while i < 10 {
                    i = i + 1;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_while_condition_not_bool() {
    let input = "
            fn main() {
                var i: i32 = 10;
                while i {
                    i = i - 1;
                }
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("bool"));
}

#[test]
fn test_for_in_loop() {
    let input = "
            fn main() {
                var arr: Array<i32, 3> = [1, 2, 3];
                for item in arr {
                    var x = item;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_break_in_loop() {
    let input = "
            fn main() {
                while true {
                    break;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_continue_in_loop() {
    let input = "
            fn main() {
                var i: i32 = 0;
                while i < 10 {
                    i = i + 1;
                    continue;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_array_declaration() {
    let input = "
            fn main() {
                var arr: Array<i32, 5> = [1, 2, 3, 4, 5];
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_array_index_access() {
    let input = "
            fn main() {
                var arr: Array<i32, 3> = [10, 20, 30];
                var first = arr[0];
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_array_index_assignment() {
    let input = "
            fn main() {
                var arr: Array<i32, 3> = [1, 2, 3];
                arr[0] = 100;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_array_repeat_syntax() {
    let input = "
            fn main() {
                var arr = [0; 10];
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_nested_array() {
    let input = "
            fn main() {
                var matrix: Array<Array<i32, 2>, 2> = [[1, 2], [3, 4]];
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_enum_definition() {
    let input = "
            enum Color { Red, Green, Blue }
            fn main() { }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_enum_usage() {
    let input = "
            enum Status { Active, Inactive }
            fn main() {
                var s = Status::Active;
            }
        ";
    let errors = analyze(input);

    assert!(errors.is_empty() || errors[0].contains("not implemented"));
}

#[test]
fn test_match_expression() {
    let input = "
            fn main() {
                var x: i32 = 1;
                var result = match x {
                    0 => 100,
                    1 => 200,
                    default => 0
                };
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_cast_int_to_float() {
    let input = "
            fn main() {
                var x: i32 = 10;
                var y = x as f32;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_cast_float_to_int() {
    let input = "
            fn main() {
                var x: f32 = 3.14;
                var y = x as i32;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_cast_between_int_sizes() {
    let input = "
            fn main() {
                var x: i32 = 100;
                var y = x as i64;
                var z = x as i8;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_negation_operator() {
    let input = "
            fn main() {
                var x: i32 = 10;
                var neg = -x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_not_operator() {
    let input = "
            fn main() {
                var x = true;
                var not_x = !x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_not_operator_on_numbers() {
    // Logical on a bool, bitwise on an integer, nothing else.
    assert!(analyze("fn main() { var x: u8 = 10; var y: u8 = !x; var b = !true; }").is_empty());
    let errors = analyze("fn main() { var f = !1.5; }");
    assert!(errors[0].contains("'!' applies to a bool or an integer, not f64"));
}

#[test]
fn test_block_scope() {
    let input = "
            fn main() {
                var x: i32 = 10;
                {
                    var y: i32 = 20;
                    var z = x + y;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_nested_block_scope() {
    let input = "
            fn main() {
                var x: i32 = 1;
                {
                    var x: i32 = 2;
                    {
                        var x: i32 = 3;
                    }
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_global_constant() {
    let input = "
            const GLOBAL: i32 = 100;
            fn main() {
                var x = GLOBAL;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_global_constant_reassignment_error() {
    let input = "
            const GLOBAL: i32 = 100;
            fn main() {
                GLOBAL = 200;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("Cannot reassign constant"));
}

#[test]
fn test_type_inference_from_literal() {
    let input = "
            fn main() {
                var x = 42;
                var y = 3.14;
                var z = true;
                var s = \"hello\";
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_type_inference_from_expression() {
    let input = "
            fn main() {
                var a: i32 = 10;
                var b: i32 = 20;
                var sum = a + b;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_type_inference_from_function_call() {
    let input = "
            fn get_value() i32 { return 42; }
            fn main() {
                var x = get_value();
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_empty_function() {
    let input = "
            fn empty() { }
            fn main() {
                empty();
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_multiple_statements_in_block() {
    let input = "
            fn main() {
                var a: i32 = 1;
                var b: i32 = 2;
                var c: i32 = 3;
                var sum = a + b + c;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_chained_field_access() {
    let input = "
            struct A { b: B }
            struct B { c: C }
            struct C { val: i32 }
            fn main() {
                var c = C { val: 42 };
                var b = B { c: c };
                var a = A { b: b };
                var x = a.b.c.val;
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
fn test_complex_program() {
    let input = "
            struct Point {
                x: f32,
                y: f32,

                fn distance_from_origin(self) f32 {
                    return self.x * self.x + self.y * self.y;
                }
            }

            fn create_point(x: f32, y: f32) Point {
                return Point { x: x, y: y };
            }

            fn main() {
                var p = create_point(3.0, 4.0);
                var dist = p.distance_from_origin();

                if dist > 10.0 {
                    var msg = \"far\";
                } else {
                    var msg = \"near\";
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_fibonacci_program() {
    let input = "
            fn fib(n: i32) i32 {
                if n <= 1 {
                    return n;
                }
                return fib(n - 1) + fib(n - 2);
            }

            fn main() {
                var i: i32 = 0;
                while i < 10 {
                    var result = fib(i);
                    i = i + 1;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_usize_type() {
    let input = "
            fn main() {
                var idx: usize = 0;
                var other: usize = 100;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_array_index_with_usize() {
    let input = "
            fn main() {
                var arr: Array<i32, 5> = [1, 2, 3, 4, 5];
                var idx: usize = 2;
                var elem = arr[idx];
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_array_index_assignment_with_usize() {
    let input = "
            fn main() {
                var arr: Array<i32, 3> = [10, 20, 30];
                var idx: usize = 1;
                arr[idx] = 100;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_prefix_negation_int() {
    let input = "
            fn main() {
                var x: i32 = 42;
                var neg: i32 = -x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_prefix_negation_float() {
    let input = "
            fn main() {
                var x: f32 = 3.14;
                var neg: f32 = -x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_prefix_not_bool() {
    let input = "
            fn main() {
                var flag: bool = true;
                var negated: bool = !flag;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_boolean_literal_true() {
    let input = "
            fn main() {
                var t: bool = true;
                var f: bool = false;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_cast_i32_to_i64() {
    let input = "
            fn main() {
                var small: i32 = 100;
                var large: i64 = small as i64;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_cast_i64_to_i32_truncate() {
    let input = "
            fn main() {
                var large: i64 = 1000;
                var small: i32 = large as i32;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_cast_f32_to_f64() {
    let input = "
            fn main() {
                var f: f32 = 3.14;
                var d: f64 = f as f64;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_cast_f64_to_f32() {
    let input = "
            fn main() {
                var d: f64 = 3.141592653589793;
                var f: f32 = d as f32;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_array_as_function_param() {
    let input = "
            fn sum_first_two(arr: Array<i32, 3>) i32 {
                return arr[0] + arr[1];
            }
            fn main() {
                var nums: Array<i32, 3> = [10, 20, 30];
                var result: i32 = sum_first_two(nums);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_array_element_modification_in_loop() {
    let input = "
            fn main() {
                var arr: Array<i32, 5> = [1, 2, 3, 4, 5];
                var i: usize = 0;
                while i < 5 {
                    arr[i] = arr[i] * 2;
                    i = i + 1;
                }
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_combined_array_and_cast() {
    let input = "
            fn main() {
                var arr: Array<i32, 3> = [1, 2, 3];
                var idx: i32 = 1;
                var elem: i32 = arr[idx as usize];
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_pointer_declaration() {
    let input = "
            fn main() {
                var x: i32 = 42;
                var ptr: *i32 = &x;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_pointer_dereference() {
    let input = "
            fn main() {
                var x: i32 = 42;
                var ptr: *i32 = &x;
                var y: i32 = *ptr;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_pointer_dereference_assignment() {
    let input = "
            fn main() {
                var x: i32 = 42;
                var ptr: *i32 = &x;
                *ptr = 100;
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
fn test_pointer_to_struct() {
    let input = "
            struct Point { x: i32, y: i32 }
            fn main() {
                var p: Point = Point { x: 10, y: 20 };
                var ptr: *Point = &p;
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_tuple_basic() {
    let input = "
            fn main() {
                var t: (i32, bool) = (42, true);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_tuple_type_inference() {
    let input = "
            fn main() {
                var t = (42, true, 3.14);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
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
fn test_tuple_nested() {
    let input = "
            fn main() {
                var t: ((i32, i32), bool) = ((1, 2), true);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_empty_tuple() {
    let input = "
            fn main() {
                var t: () = ();
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_mut_parameter_can_be_modified() {
    let input = "
            fn increment(var x: i32) i32 {
                x += 1;
                return x;
            }
            fn main() {
                var result = increment(5);
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_immutable_parameter_cannot_be_modified() {
    let input = "
            fn increment(x: i32) i32 {
                x += 1;
                return x;
            }
            fn main() {}
        ";
    let errors = analyze(input);
    assert!(
        !errors.is_empty(),
        "Expected error for mutating immutable parameter"
    );
    assert!(errors[0].contains("Cannot reassign constant"));
}

#[test]
fn test_mut_self_in_method() {
    let input = "
            struct Counter {
                value: i32,

                fn increment(var self) {
                    self.value += 1;
                }
            }
            fn main() {}
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_immutable_self_cannot_modify_fields() {
    let input = "
            struct Counter {
                value: i32,

                fn try_increment(self) {
                    self.value += 1;
                }
            }
            fn main() {}
        ";
    let errors = analyze(input);
    println!("Errors for immutable self: {:?}", errors);
}

#[test]
fn test_pointer_arithmetic_add() {
    let input = "
        fn get_ptr() *u8 {
            var x: u8 = 0;
            return &x;
        }
        fn main() {
            var ptr: *u8 = get_ptr();
            var next: *u8 = ptr + 1;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty(), "ptr + int should be valid: {:?}", errors);
}

#[test]
fn test_pointer_arithmetic_sub() {
    let input = "
        fn get_ptr() *u8 {
            var x: u8 = 0;
            return &x;
        }
        fn main() {
            var ptr: *u8 = get_ptr();
            var prev: *u8 = ptr - 1;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty(), "ptr - int should be valid: {:?}", errors);
}

#[test]
fn test_pointer_arithmetic_with_usize() {
    let input = "
        fn get_ptr() *u8 {
            var x: u8 = 0;
            return &x;
        }
        fn main() {
            var ptr: *u8 = get_ptr();
            var offset: usize = 2;
            var next: *u8 = ptr + offset;
        }
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "ptr + usize should be valid: {:?}",
        errors
    );
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
fn test_str_type_alias() {
    // NOTE: str is now a proper fat pointer type (ptr + len), not an alias for *u8
    // String literals currently still produce *u8 until codegen is updated
    // This test verifies str type is recognized
    let input = "
        fn takes_str(s: str) {
        }
        fn main() {
            // For now, str variables need explicit type annotation
            // String literals will be updated to produce str in the future
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty(), "str type should be valid: {:?}", errors);
}

#[test]
fn test_str_type_is_distinct_from_pointer_u8() {
    // str is now a distinct type from *u8 (fat pointer vs raw pointer)
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
fn test_str_function_parameter() {
    let input = "
        fn print_str(s: str) {
            // Just type check - str is a valid parameter type
        }
        fn main() {
            // Note: string literals still produce *u8, full str support pending
        }
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "str as function param should work: {:?}",
        errors
    );
}

#[test]
fn test_str_return_type() {
    // Test that str can be used as a return type
    // Full str support is pending codegen updates
    let input = "
        fn takes_and_returns_str(s: str) str {
            return s;
        }
        fn main() {
        }
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "str as return type should work: {:?}",
        errors
    );
}

#[test]
fn test_logical_and_short_circuit() {
    let input = "
        fn main() {
            var a = true;
            var b = false;
            var result = a && b;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_logical_or_short_circuit() {
    let input = "
        fn main() {
            var a = true;
            var b = false;
            var result = a || b;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_deeply_nested_expressions() {
    let input = "
        fn main() {
            var x: i32 = ((((1 + 2) * 3) - 4) / 5);
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_break_outside_loop_error() {
    let input = "
        fn main() {
            break;
        }
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("loop"));
}

#[test]
fn test_continue_outside_loop_error() {
    let input = "
        fn main() {
            continue;
        }
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("loop"));
}

#[test]
fn test_return_outside_function_error() {
    let input = "
        fn main() {
            // Valid return inside function
            return;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_duplicate_struct_definition() {
    let input = "
        struct Foo { x: i32 }
        struct Foo { y: i32 }
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("already defined"));
}

#[test]
fn test_duplicate_enum_definition() {
    let input = "
        enum Color { Red, Green }
        enum Color { Blue, Yellow }
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("already defined"));
}

#[test]
fn test_duplicate_function_definition() {
    let input = "
        fn foo() {}
        fn foo() {}
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
    assert!(errors[0].contains("already defined"));
}

#[test]
fn test_optional_type_basic() {
    let input = "
        fn main() {
            var x: i32? = 42;
            var y: i32? = None;
        }
    ";
    let errors = analyze(input);
    assert!(errors.is_empty(), "Optional type should work: {:?}", errors);
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
fn test_empty_array() {
    let input = "
        fn main() {
            var arr: Array<i32, 3> = [0; 3];
        }
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "Array initialization should be valid: {:?}",
        errors
    );
}

#[test]
fn test_main_with_invalid_return_type() {
    let input = "
        fn main() f32 {
            return 3.14;
        }
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty(), "main should only return void or i32");
}

#[test]
fn test_main_with_i32_return_rejected() {
    let input = "
        fn main() i32 {
            return 0;
        }
    ";
    let errors = analyze(input);
    assert!(
        !errors.is_empty(),
        "main with i32 return should be rejected"
    );
    assert!(errors[0].contains("must return void"));
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
    // 'Entry' is seven edits from 'HashMapEntry'. Leaving the first row of the
    // distance table at zero made the leading 'HashMap' cost nothing.
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
fn test_generic_function_identity() {
    let input = "
        fn identity<T>(x: T) T {
            return x;
        }
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "Generic identity function should be valid: {:?}",
        errors
    );
}

#[test]
fn test_generic_function_multiple_params() {
    let input = "
        fn swap<T, U>(a: T, b: U) T {
            return a;
        }
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "Generic function with multiple type params should be valid: {:?}",
        errors
    );
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
fn test_trait_definition() {
    let input = "
        trait Drawable {
            fn draw(self);
            fn area(self) f64;
        }
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(
        errors.is_empty(),
        "Trait definition should be valid: {:?}",
        errors
    );
}

#[test]
fn test_duplicate_trait_error() {
    let input = "
        trait Foo {}
        trait Foo {}
        fn main() {}
    ";
    let errors = analyze(input);
    assert!(!errors.is_empty(), "Duplicate trait should error");
    assert!(errors[0].contains("already defined"));
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
fn test_missing_return_is_rejected() {
    // The body used to compile to a bare `unreachable`, so the caller read
    // whatever happened to be in the return register.
    let errors = analyze("fn f() i32 { } fn main() { }");
    assert!(
        errors
            .iter()
            .any(|e| e.contains("without returning a value")),
        "got: {errors:?}"
    );
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
    // Laying this out sent the compiler into a stack overflow.
    for input in [
        "struct S { next: S } fn main() { }",
        "struct A { b: B } struct B { a: A } fn main() { }",
        "struct S { kids: Array<S, 2> } fn main() { }",
    ] {
        let errors = analyze(input);
        assert!(
            errors.iter().any(|e| e.contains("no finite size")),
            "{input} -> {errors:?}"
        );
    }
}

#[test]
fn test_indirection_breaks_struct_recursion() {
    let errors = analyze("struct N { next: *N, v: i32 } fn main() { }");
    assert!(errors.is_empty(), "got: {errors:?}");
}

#[test]
fn test_generic_return_type_is_concrete_at_the_call_site() {
    // The return type stayed as the type parameter, so anything that had to
    // know the real type -- a field, a method -- was rejected.
    let input = "
        struct P { x: i32, fn get(self) i32 { return self.x; } }
        fn pick<T>(a: T, b: T) T { return a; }
        fn main() {
            var p1 = P { x: 1 };
            var p2 = P { x: 2 };
            var field: i32 = pick(p1, p2).x;
        }
    ";
    assert!(analyze(input).is_empty(), "{:?}", analyze(input));
}

#[test]
fn test_duplicate_declarations_are_rejected() {
    let cases = [
        (
            "struct S { a: i32, a: i32 } fn main() { }",
            "field 'a' twice",
        ),
        (
            "fn f(a: i32, a: i32) { } fn main() { }",
            "parameter 'a' twice",
        ),
        ("enum E { A, A } fn main() { }", "variant 'A' twice"),
    ];
    for (input, expected) in cases {
        let errors = analyze(input);
        assert!(
            errors.iter().any(|e| e.contains(expected)),
            "{input} -> {errors:?}"
        );
    }
}

#[test]
fn test_index_must_be_an_integer() {
    let errors = analyze("fn main() { var a: Array<i32,2> = [1,2]; var v = a[true]; }");
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Index must be an integer")),
        "got: {errors:?}"
    );
}

#[test]
fn test_constant_index_out_of_range_is_rejected() {
    // Known at compile time, so it should not wait for the runtime check.
    let errors = analyze("fn main() { var a: Array<i32,2> = [1,2]; var v = a[5]; }");
    assert!(
        errors
            .iter()
            .any(|e| e.contains("outside the array's 0..2")),
        "got: {errors:?}"
    );
}

#[test]
fn test_assigning_to_a_temporary_is_rejected() {
    // Nothing reads a temporary again, so the write would go nowhere. Codegen
    // used to catch this only where it happened to lack a pointer.
    let cases = [
        "fn make() Array<i32,2> { return [1,2]; } fn main() { make()[0] = 5; }",
        "struct S { a: i32 } fn make() S { return S{a:1}; } fn main() { make().a = 5; }",
        "struct S { a: i32 } fn main() { S{a:1}.a = 5; }",
        "fn main() { 1 + 2 = 3; }",
    ];
    for input in cases {
        let errors = analyze(input);
        assert!(
            errors.iter().any(|e| e.contains("temporary")),
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
fn test_writing_through_a_slice_is_rejected() {
    let errors = analyze(
        "
        fn main() {
            var text: str = \"abc\";
            text[0] = 65;
        }
    ",
    );
    assert!(
        errors
            .iter()
            .any(|e| e.contains("Cannot write through a slice")),
        "expected the write to be refused: {errors:?}"
    );
}

#[test]
fn test_cast_to_a_pointer_has_a_type() {
    // The target was only understood when it was a bare name, so a pointer
    // cast came out untyped and fitted any annotation.
    let inferred = "fn main() { var x: i64 = 0; var p = x as *u8; var q: *u8 = p; }";
    assert!(analyze(inferred).is_empty(), "{:?}", analyze(inferred));

    let wrong = "fn main() { var x: i64 = 0; var q: bool = x as *u8; }";
    let errors = analyze(wrong);
    assert_eq!(errors.len(), 1, "{errors:?}");
    assert!(errors[0].contains("Type mismatch"), "{errors:?}");
}

#[test]
fn test_literal_that_does_not_fit_is_an_error() {
    // Each of these used to be accepted, and the last two silently wrapped.
    for (declaration, message) in [
        ("var x: u64 = -1;", "Literal -1 does not fit in u64"),
        ("var x: usize = -5;", "Literal -5 does not fit in usize"),
        ("var x: i8 = -129;", "Literal -129 does not fit in i8"),
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
    // Only a literal on the right used to adapt, so `3 < x` compared an i32
    // with a u32 and was refused, while `x > 3` was fine.
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
    // Each of these reached codegen, which crashed on some and miscompiled
    // the rest: `true < false` compared as signed one-bit numbers.
    for (expression, operand) in [
        ("true + true", "bool"),
        ("1 && 2", "i32"),
        ("p == q", "P"),
        ("\"a\" == \"b\"", "str"),
        ("true < false", "bool"),
        ("1.5 & 2.5", "f64"),
        ("1.5 << 2.5", "f64"),
    ] {
        let input = format!(
            "struct P {{ x: i32 }} fn main() {{ var p = P {{ x: 1 }}; var q = P {{ x: 1 }}; var r = {expression}; }}"
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
    // Patterns were never looked at: these reached LLVM, which refused them
    // with its own messages, or, for the first, compiled to undefined
    // behaviour for any value no arm named.
    for (arms, message) in [
        ("0 => 1, 1 => 2", "needs a 'default' arm"),
        (
            "1 => 1, 1 => 2, default => 3",
            "already matched by an earlier arm",
        ),
        (
            "y => 1, default => 2",
            "must be a literal or an enum variant",
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
fn test_global_constant_is_defined_once() {
    for input in [
        "const A: i32 = 1; const A: i32 = 2; fn main() { }",
        "fn A() { } const A: i32 = 2; fn main() { }",
    ] {
        let errors = analyze(input);
        assert_eq!(errors, ["'A' is already defined"], "{input}");
    }
}

#[test]
fn test_generic_function_is_checked_at_its_types() {
    // The body used to be checked once with T standing for anything, and an
    // instantiation was never checked at all.
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
    // Only a declaration, a return and a call argument used to move: each of
    // these left two owners of one Vec, and a push through either could
    // reallocate the buffer under the other.
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
            "out of a field, an element or a pointer",
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
            // A copy of a field is a value of its own.
            var s = S { v: Vec.new() };
            var v = s.v.copy();
            // Declared inside the loop, so a new one each turn.
            while true {
                var fresh: Vec<i64> = Vec.new();
                var kept = fresh;
                break;
            }
            // A constant is built anew wherever it is used.
            take(ROW);
            take(ROW);
            // Only one arm runs.
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
        // Moved, then out of the loop at once.
        "var v: Vec<i32> = Vec.new(); while true { eat(v); break; }",
        // Moved, then given a value again before the next turn.
        "var v: Vec<i32> = Vec.new(); var i = 0; while i < 3 { eat(v); v = Vec.new(); i += 1; }",
        // The branch that moves it returns, so past the `if` it is still there.
        "var v: Vec<i32> = Vec.new(); var c = true; if c { eat(v); return; } eat(v);",
        // Plain data is copied, not moved.
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
            "Cannot change 'v' while a loop walks 'v'",
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
