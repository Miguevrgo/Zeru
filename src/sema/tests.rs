use crate::parser::Parser;
use crate::{lexer::Lexer, sema::analyzer::SemanticAnalyzer};

fn analyze(input: &str) -> Vec<String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        panic!(
            "Parser errors in test: {:?}",
            parser.errors.iter().map(|e| &e.message).collect::<Vec<_>>()
        );
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program);
    analyzer.errors.iter().map(|e| e.message.clone()).collect()
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
fn test_int_to_float_mismatch() {
    let input = "
            fn main() {
                var x: f32 = 10;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
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
                var s: String = \"hello\";
            }
        ";
    let errors = analyze(input);
    assert!(errors.is_empty());
}

#[test]
fn test_string_type_mismatch() {
    let input = "
            fn main() {
                var s: String = 42;
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
                var p = Point { x: 1, y: 2 };
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
                p.x = 5;
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
                fn increment(self) { self.val = self.val + 1; }
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
                fn add(self, x: i32) { self.result = self.result + x; }
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
fn test_not_operator_on_non_bool() {
    let input = "
            fn main() {
                var x: i32 = 10;
                var not_x = !x;
            }
        ";
    let errors = analyze(input);
    assert!(!errors.is_empty());
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
