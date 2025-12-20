use crate::codegen::SafetyMode;
use crate::codegen::compiler::Compiler;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::sema::analyzer::SemanticAnalyzer;
use inkwell::context::Context;

fn compile_to_ir(input: &str) -> Result<String, String> {
    compile_to_ir_with_mode(input, SafetyMode::Debug)
}

fn compile_to_ir_with_mode(input: &str, safety_mode: SafetyMode) -> Result<String, String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        let msgs: Vec<_> = parser.errors.iter().map(|e| e.message.as_str()).collect();
        return Err(format!("Parser errors: {:?}", msgs));
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program);

    if !analyzer.errors.is_empty() {
        let msgs: Vec<_> = analyzer.errors.iter().map(|e| e.message.as_str()).collect();
        return Err(format!("Semantic errors: {:?}", msgs));
    }

    let context = Context::create();
    let module = context.create_module("test");
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode);
    compiler.compile_program(&program);

    Ok(module.print_to_string().to_string())
}

fn assert_compiles(input: &str) {
    match compile_to_ir(input) {
        Ok(_) => {}
        Err(e) => panic!("Compilation failed: {}", e),
    }
}

fn assert_ir_contains(input: &str, patterns: &[&str]) {
    let ir = compile_to_ir(input).expect("Compilation failed");
    for pattern in patterns {
        assert!(
            ir.contains(pattern),
            "IR does not contain expected pattern: '{}'\n\nFull IR:\n{}",
            pattern,
            ir
        );
    }
}

#[test]
fn test_empty_main() {
    assert_compiles("fn main() { }");
}

#[test]
fn test_main_returns_i32() {
    let input = "fn main() i32 { return 42; }";
    assert_ir_contains(input, &["define i32 @main()", "ret i32 42"]);
}

#[test]
fn test_function_with_params() {
    let input = "
            fn add(a: i32, b: i32) i32 {
                return a + b;
            }
            fn main() { }
        ";
    assert_ir_contains(input, &["define i32 @add(i32 %0, i32 %1)"]);
}

#[test]
fn test_void_function() {
    let input = "
            fn do_nothing() { }
            fn main() { do_nothing(); }
        ";
    assert_ir_contains(
        input,
        &["define void @do_nothing()", "call void @do_nothing()"],
    );
}

#[test]
fn test_variable_declaration() {
    let input = "fn main() { var x: i32 = 42; }";
    assert_ir_contains(input, &["alloca i32", "store i32 42"]);
}

#[test]
fn test_different_integer_types() {
    let input = "
            fn main() {
                var a: i8 = 1;
                var b: i16 = 2;
                var c: i32 = 3;
                var d: i64 = 4;
            }
        ";
    assert_ir_contains(
        input,
        &["alloca i8", "alloca i16", "alloca i32", "alloca i64"],
    );
}

#[test]
fn test_float_types() {
    let input = "
            fn main() {
                var f: f32 = 3.14;
                var d: f64 = 2.718;
            }
        ";
    assert_ir_contains(input, &["alloca float", "alloca double"]);
}

#[test]
fn test_bool_type() {
    let input = "fn main() { var b: bool = true; }";
    assert_ir_contains(input, &["alloca i1", "store i1 true"]);
}

#[test]
fn test_integer_arithmetic() {
    let input = "
            fn main() {
                var a: i32 = 10;
                var b: i32 = 3;
                var sum = a + b;
                var diff = a - b;
                var prod = a * b;
                var quot = a / b;
                var rem = a % b;
            }
        ";
    assert_ir_contains(
        input,
        &["add i32", "sub i32", "mul i32", "sdiv i32", "srem i32"],
    );
}

#[test]
fn test_float_arithmetic() {
    let input = "
            fn main() {
                var a: f32 = 10.0;
                var b: f32 = 3.0;
                var sum = a + b;
                var diff = a - b;
                var prod = a * b;
                var quot = a / b;
            }
        ";
    assert_ir_contains(
        input,
        &["fadd float", "fsub float", "fmul float", "fdiv float"],
    );
}

#[test]
fn test_integer_comparisons() {
    let input = "
            fn main() {
                var a: i32 = 5;
                var b: i32 = 10;
                var eq = a == b;
                var ne = a != b;
                var lt = a < b;
                var le = a <= b;
                var gt = a > b;
                var ge = a >= b;
            }
        ";
    assert_ir_contains(
        input,
        &[
            "icmp eq i32",
            "icmp ne i32",
            "icmp slt i32",
            "icmp sle i32",
            "icmp sgt i32",
            "icmp sge i32",
        ],
    );
}

#[test]
fn test_float_comparisons() {
    let input = "
            fn main() {
                var a: f32 = 5.0;
                var b: f32 = 10.0;
                var lt = a < b;
                var eq = a == b;
            }
        ";
    assert_ir_contains(input, &["fcmp olt float", "fcmp oeq float"]);
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
    assert_ir_contains(input, &["and i32", "or i32", "xor i32", "shl i32"]);
}

#[test]
fn test_if_statement() {
    let input = "
            fn main() {
                var x: i32 = 10;
                if x > 5 {
                    x = 1;
                }
            }
        ";
    assert_ir_contains(input, &["br i1", "then:", "merge:"]);
}

#[test]
fn test_if_else_statement() {
    let input = "
            fn main() {
                var x: i32 = 10;
                if x > 5 {
                    x = 1;
                } else {
                    x = 0;
                }
            }
        ";
    assert_ir_contains(input, &["br i1", "then:", "else:", "merge:"]);
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
    assert_ir_contains(input, &["loop_cond:", "loop_body:", "after_loop:", "br i1"]);
}

#[test]
fn test_struct_definition() {
    let input = "
            struct Point { x: i32, y: i32 }
            fn main() {
                var p = Point { x: 10, y: 20 };
            }
        ";
    assert_ir_contains(input, &["%Point = type { i32, i32 }"]);
}

#[test]
fn test_struct_field_access() {
    let input = "
            struct Point { x: i32, y: i32 }
            fn main() {
                var p = Point { x: 10, y: 20 };
                var val = p.x;
            }
        ";
    assert_ir_contains(input, &["getelementptr inbounds"]);
}

#[test]
fn test_struct_method() {
    let input = "
            struct Counter {
                val: i32,
                fn get(self) i32 { return self.val; }
            }
            fn main() {
                var c = Counter { val: 42 };
                var v = c.get();
            }
        ";
    assert_ir_contains(input, &["define i32 @\"Counter::get\"(%Counter %0)"]);
}

#[test]
fn test_function_call() {
    let input = "
            fn square(x: i32) i32 { return x * x; }
            fn main() {
                var result = square(5);
            }
        ";
    assert_ir_contains(input, &["call i32 @square(i32 5)"]);
}

#[test]
fn test_recursive_function() {
    let input = "
            fn factorial(n: i32) i32 {
                if n <= 1 { return 1; }
                return n * factorial(n - 1);
            }
            fn main() { var r = factorial(5); }
        ";
    assert_ir_contains(input, &["call i32 @factorial"]);
}

#[test]
fn test_compound_add_assign() {
    let input = "
        fn main() {
            var x: i32 = 10;
            x += 5;
        }
    ";
    assert_ir_contains(input, &["load i32", "add i32", "store i32"]);
}

#[test]
fn test_compound_sub_assign() {
    let input = "
        fn main() {
            var x: i32 = 10;
            x -= 3;
        }
    ";
    assert_ir_contains(input, &["load i32", "sub i32", "store i32"]);
}

#[test]
fn test_compound_mul_assign() {
    let input = "
        fn main() {
            var x: i32 = 10;
            x *= 2;
        }
    ";
    assert_ir_contains(input, &["load i32", "mul i32", "store i32"]);
}

#[test]
fn test_compound_div_assign() {
    let input = "
        fn main() {
            var x: i32 = 10;
            x /= 2;
        }
    ";
    assert_ir_contains(input, &["load i32", "sdiv i32", "store i32"]);
}

#[test]
fn test_compound_bitwise_and_assign() {
    let input = "
        fn main() {
            var x: i32 = 0xFF;
            x &= 0x0F;
        }
    ";
    assert_ir_contains(input, &["load i32", "and i32", "store i32"]);
}

#[test]
fn test_compound_bitwise_or_assign() {
    let input = "
        fn main() {
            var x: i32 = 0x0F;
            x |= 0xF0;
        }
    ";
    assert_ir_contains(input, &["load i32", "or i32", "store i32"]);
}

#[test]
fn test_compound_shift_left_assign() {
    let input = "
        fn main() {
            var x: i32 = 1;
            x <<= 4;
        }
    ";
    assert_ir_contains(input, &["load i32", "shl i32", "store i32"]);
}

#[test]
fn test_compound_shift_right_assign() {
    let input = "
        fn main() {
            var x: i32 = 16;
            x >>= 2;
        }
    ";
    assert_ir_contains(input, &["load i32", "ashr i32", "store i32"]);
}

#[test]
fn test_compound_float_add_assign() {
    let input = "
        fn main() {
            var f: f32 = 1.0;
            f += 2.5;
        }
    ";
    assert_ir_contains(input, &["load float", "fadd float", "store float"]);
}

#[test]
fn test_compound_in_loop() {
    let input = "
        fn main() {
            var sum: i32 = 0;
            var i: i32 = 0;
            while i < 10 {
                sum += i;
                i += 1;
            }
        }
    ";
    assert_ir_contains(input, &["loop_cond:", "loop_body:", "add i32"]);
}

#[test]
fn test_string_literal_basic() {
    let input = r#"
        fn main() {
            var msg = "Hello, World!";
        }
    "#;
    assert_ir_contains(input, &["@str", "Hello, World!"]);
}

#[test]
fn test_string_literal_empty() {
    let input = r#"
        fn main() {
            var empty = "";
        }
    "#;
    assert_compiles(input);
}

#[test]
fn test_string_literal_with_escapes() {
    let input = r#"
        fn main() {
            var escaped = "Line1\nLine2\tTabbed";
        }
    "#;
    assert_compiles(input);
}

#[test]
fn test_multiple_string_literals() {
    let input = r#"
        fn main() {
            var a = "first";
            var b = "second";
            var c = "third";
        }
    "#;
    assert_ir_contains(input, &["first", "second", "third"]);
}

#[test]
fn test_pointer_address_of() {
    let input = "
        fn main() {
            var x: i32 = 42;
            var ptr: *i32 = &x;
        }
    ";
    assert_ir_contains(input, &["alloca i32", "alloca ptr", "store ptr"]);
}

#[test]
fn test_pointer_dereference_read() {
    let input = "
        fn main() i32 {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            return *ptr;
        }
    ";
    assert_ir_contains(input, &["load ptr", "load i32"]);
}

#[test]
fn test_pointer_dereference_write() {
    let input = "
        fn main() i32 {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            *ptr = 100;
            return x;
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_debug_mode_emits_null_checks() {
    let input = "
        fn main() i32 {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            return *ptr;
        }
    ";
    let ir = compile_to_ir_with_mode(input, SafetyMode::Debug).unwrap();
    assert!(
        ir.contains("icmp eq ptr"),
        "Debug mode should emit null pointer check"
    );
    assert!(
        ir.contains("null_panic"),
        "Debug mode should have panic block"
    );
    assert!(
        ir.contains("@abort"),
        "Debug mode should call abort on null"
    );
}

#[test]
fn test_release_safe_emits_null_checks() {
    let input = "
        fn main() i32 {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            return *ptr;
        }
    ";
    let ir = compile_to_ir_with_mode(input, SafetyMode::ReleaseSafe).unwrap();
    assert!(
        ir.contains("icmp eq ptr"),
        "ReleaseSafe should emit null pointer check"
    );
    assert!(
        ir.contains("null_panic"),
        "ReleaseSafe should have panic block"
    );
}

#[test]
fn test_release_fast_no_null_checks() {
    let input = "
        fn main() i32 {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            return *ptr;
        }
    ";
    let ir = compile_to_ir_with_mode(input, SafetyMode::ReleaseFast).unwrap();
    assert!(
        !ir.contains("null_panic"),
        "ReleaseFast should NOT have null check blocks"
    );
    assert!(!ir.contains("@abort"), "ReleaseFast should NOT call abort");
}

#[test]
fn test_tuple_basic_codegen() {
    let input = "
        fn main() i32 {
            var t: (i32, i32) = (10, 20);
            return 0;
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(
        ir.contains("{ i32, i32 }"),
        "Tuple should be compiled to struct type"
    );
}

#[test]
fn test_tuple_with_different_types() {
    let input = "
        fn main() i32 {
            var t: (i32, bool, f64) = (42, true, 3.14);
            return 0;
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(
        ir.contains("{ i32, i1, double }"),
        "Mixed-type tuple should compile correctly"
    );
}

#[test]
fn test_pointer_arithmetic_add() {
    let input = "
        fn main() {
            var ptr: *u8 = \"test\";
            var next: *u8 = ptr + 1;
        }
    ";
    assert_ir_contains(input, &["getelementptr"]);
}

#[test]
fn test_pointer_arithmetic_sub() {
    let input = "
        fn main() {
            var ptr: *u8 = \"test\";
            var prev: *u8 = ptr - 1;
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(
        ir.contains("getelementptr"),
        "Should use GEP for pointer sub"
    );
}
