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
    let mut program = parser.parse_program();

    if !parser.errors.is_empty() {
        let msgs: Vec<_> = parser.errors.iter().map(|e| e.message.as_str()).collect();
        return Err(format!("Parser errors: {:?}", msgs));
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&mut program);

    if !analyzer.errors.is_empty() {
        let msgs: Vec<_> = analyzer.errors.iter().map(|e| e.message.as_str()).collect();
        return Err(format!("Semantic errors: {:?}", msgs));
    }

    let context = Context::create();
    let module = context.create_module("test");
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode);
    compiler.compile_program(&program);

    if !compiler.errors.is_empty() {
        let msgs: Vec<_> = compiler.errors.iter().map(|e| e.message.as_str()).collect();
        return Err(format!("Codegen errors: {:?}", msgs));
    }

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
    // Debug traps on overflow, so `+ - *` go through the intrinsic.
    assert_ir_contains(
        input,
        &[
            "llvm.sadd.with.overflow.i32",
            "llvm.ssub.with.overflow.i32",
            "llvm.smul.with.overflow.i32",
            "sdiv i32",
            "srem i32",
        ],
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
    assert_ir_contains(
        input,
        &["load i32", "llvm.sadd.with.overflow.i32", "store i32"],
    );
}

#[test]
fn test_compound_sub_assign() {
    let input = "
        fn main() {
            var x: i32 = 10;
            x -= 3;
        }
    ";
    assert_ir_contains(
        input,
        &["load i32", "llvm.ssub.with.overflow.i32", "store i32"],
    );
}

#[test]
fn test_compound_mul_assign() {
    let input = "
        fn main() {
            var x: i32 = 10;
            x *= 2;
        }
    ";
    assert_ir_contains(
        input,
        &["load i32", "llvm.smul.with.overflow.i32", "store i32"],
    );
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
    assert_ir_contains(
        input,
        &["loop_cond:", "loop_body:", "llvm.sadd.with.overflow.i32"],
    );
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
        fn main() {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            var val: i32 = *ptr;
        }
    ";
    assert_ir_contains(input, &["load ptr", "load i32"]);
}

#[test]
fn test_pointer_dereference_write() {
    let input = "
        fn main() {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            *ptr = 100;
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_debug_mode_emits_null_checks() {
    let input = "
        fn main() {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            var val: i32 = *ptr;
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
        fn main() {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            var val: i32 = *ptr;
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
        fn main() {
            var x: i32 = 42;
            var ptr: *i32 = &x;
            var val: i32 = *ptr;
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
        fn main() {
            var t: (i32, i32) = (10, 20);
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
        fn main() {
            var t: (i32, bool, f64) = (42, true, 3.14);
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
            var x: u8 = 0;
            var ptr: *u8 = &x;
            var next: *u8 = ptr + 1;
        }
    ";
    assert_ir_contains(input, &["getelementptr"]);
}

#[test]
fn test_pointer_arithmetic_sub() {
    let input = "
        fn main() {
            var x: u8 = 0;
            var ptr: *u8 = &x;
            var prev: *u8 = ptr - 1;
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(
        ir.contains("getelementptr"),
        "Should use GEP for pointer sub"
    );
}
#[test]
fn test_generic_function_identity() {
    let input = "
        fn identity<T>(x: T) T {
            return x;
        }
        fn main() {
            var a: i32 = identity(42);
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(
        ir.contains("identity__i32_"),
        "Should generate monomorphized i32 identity function"
    );
}

#[test]
fn test_generic_function_multiple_types() {
    let input = "
        fn identity<T>(x: T) T {
            return x;
        }
        fn main() {
            var a: i32 = identity(42);
            var b: f64 = identity(3.14);
            var c: bool = identity(true);
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(
        ir.contains("identity__i32_"),
        "Should have i32 specialization"
    );
    assert!(
        ir.contains("identity__f64_"),
        "Should have f64 specialization"
    );
    assert!(
        ir.contains("identity__bool_"),
        "Should have bool specialization"
    );
}

#[test]
fn test_generic_function_two_params() {
    let input = "
        fn first<T, U>(a: T, b: U) T {
            return a;
        }
        fn main() {
            var x: i32 = first(10, 3.14);
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(
        ir.contains("first__i32_f64_"),
        "Should generate monomorphized function with both types"
    );
}

#[test]
fn test_for_in_array() {
    let input = "
        fn main() {
            var arr: Array<i32, 3> = [1, 2, 3];
            var sum: i32 = 0;
            for item in arr {
                sum = sum + item;
            }
        }
    ";
    let ir = compile_to_ir(input).unwrap();
    assert!(ir.contains("for_cond"), "Should have loop condition block");
    assert!(ir.contains("for_body"), "Should have loop body block");
    assert!(ir.contains("for_incr"), "Should have loop increment block");
    assert!(ir.contains("after_for"), "Should have after-loop block");
    assert!(
        ir.contains("add i64") || ir.contains("add nsw i64"),
        "Should increment the index"
    );
}

#[test]
fn test_for_in_with_break() {
    let input = "
        fn main() {
            var arr: Array<i32, 5> = [1, 2, 3, 4, 5];
            for item in arr {
                if item == 3 {
                    break;
                }
            }
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_for_in_with_continue() {
    let input = "
        fn main() {
            var arr: Array<i32, 5> = [1, 2, 3, 4, 5];
            for item in arr {
                if item == 2 {
                    continue;
                }
            }
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_type_ok() {
    let input = "
        fn get_value() i32! {
            return Ok(42);
        }
        fn main() {
            var result: i32! = get_value();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_type_err() {
    let input = "
        fn get_value() i32! {
            return Err(1);
        }
        fn main() {
            var result: i32! = get_value();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_type_conditional() {
    let input = "
        fn divide(a: i32, b: i32) i32! {
            if b == 0 {
                return Err(1);
            }
            return Ok(a / b);
        }
        fn main() {
            var r1: i32! = divide(10, 2);
            var r2: i32! = divide(10, 0);
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_str_type() {
    let input = "
        fn main() {
            var s: str = \"hello\";
            println(s);
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_str_len() {
    let input = "
        fn main() {
            var s: str = \"hello\";
            var len: usize = s.len();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_is_ok() {
    let input = "
        fn try_it() i32! {
            return Ok(42);
        }
        fn main() {
            var r: i32! = try_it();
            var ok: bool = r.is_ok();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_is_err() {
    let input = "
        fn try_it() i32! {
            return Err(1);
        }
        fn main() {
            var r: i32! = try_it();
            var err: bool = r.is_err();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_unwrap() {
    let input = "
        fn try_it() i32! {
            return Ok(42);
        }
        fn main() {
            var r: i32! = try_it();
            var val: i32 = r.unwrap();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_unwrap_err() {
    let input = "
        fn try_it() i32! {
            return Err(5);
        }
        fn main() {
            var r: i32! = try_it();
            var code: i32 = r.unwrap_err();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_result_is_ok_branch() {
    let input = "
        fn divide(a: i32, b: i32) i32! {
            if b == 0 {
                return Err(1);
            }
            return Ok(a / b);
        }
        fn main() {
            var r: i32! = divide(10, 2);
            if r.is_ok() {
                var val: i32 = r.unwrap();
            }
        }
    ";
    assert_compiles(input);
}

// Regression tests: each pins a bug that emitted wrong code rather than an
// error, so the IR is the only witness.

fn assert_ir_lacks(input: &str, patterns: &[&str]) {
    let ir = compile_to_ir(input).expect("Compilation failed");
    for pattern in patterns {
        assert!(
            !ir.contains(pattern),
            "IR contains unexpected pattern: '{}'\n\nFull IR:\n{}",
            pattern,
            ir
        );
    }
}

#[test]
fn test_signed_widening_cast_sign_extends() {
    // Zero-extending turns `-1 as i64` into 4294967295.
    let input = "
        fn main() {
            var a: i32 = -1;
            var b: i64 = a as i64;
        }
    ";
    assert_ir_contains(input, &["sext i32"]);
    assert_ir_lacks(input, &["zext i32"]);
}

#[test]
fn test_unsigned_widening_cast_zero_extends() {
    let input = "
        fn main() {
            var a: u32 = 7;
            var b: u64 = a as u64;
        }
    ";
    assert_ir_contains(input, &["zext i32"]);
    assert_ir_lacks(input, &["sext i32"]);
}

#[test]
fn test_bool_widening_cast_zero_extends() {
    // Sign-extending an i1 makes `true as i32` equal -1.
    let input = "
        fn main() {
            var flag: bool = true;
            var n: i32 = flag as i32;
        }
    ";
    assert_ir_contains(input, &["zext i1"]);
    assert_ir_lacks(input, &["sext i1"]);
}

#[test]
fn test_float_to_unsigned_cast_is_unsigned() {
    let input = "
        fn main() {
            var f: f64 = 3.5;
            var n: u32 = f as u32;
        }
    ";
    assert_ir_contains(input, &["fptoui"]);
}

#[test]
fn test_three_field_struct_method_is_not_a_vec_method() {
    // A struct shaped like `Vec`'s header went through the Vec fast path, which
    // reads a garbage pointer out of the first field.
    let input = "
        struct Buf {
            a: i32,
            b: i32,
            c: i32,

            fn get(self, i: i32) i32 { return self.a; }
            fn len(self) i32 { return 3; }
        }
        fn main() {
            var buf = Buf { a: 1, b: 2, c: 3 };
            var x: i32 = buf.get(0);
            var n: i32 = buf.len();
        }
    ";
    assert_ir_contains(input, &["call i32 @\"Buf::get\"", "call i32 @\"Buf::len\""]);
}

#[test]
fn test_store_through_pointer_uses_pointee_width() {
    // Storing i64 through a *i32 overwrites the next four bytes.
    let input = "
        fn main() {
            var arr: Array<i32, 4> = [1, 2, 3, 4];
            var p: *i32 = &arr[0];
            *p = 77;
        }
    ";
    assert_ir_contains(input, &["store i32 77"]);
    assert_ir_lacks(input, &["store i64 77"]);
}

#[test]
fn test_enum_variant_path_resolves() {
    // Arrives as one qualified identifier; without a case for it every enum use
    // failed with "Unknown identifier".
    let input = "
        enum Status { Connected, Disconnected, Connecting }
        fn main() {
            var s: Status = Status::Connecting;
        }
    ";
    assert_ir_contains(input, &["store i32 2"]);
}

#[test]
fn test_enum_variant_path_in_match() {
    let input = "
        enum Color { Red, Green, Blue }
        fn main() {
            var c: Color = Color::Green;
            var n = match c {
                Color::Red => 0,
                Color::Green => 1,
                default => 2
            };
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_method_call_on_temporary_evaluates_receiver_once() {
    // The receiver was compiled three times, so a call in that position ran its
    // side effects three times.
    let input = "
        struct Counter {
            n: i32,
            fn value(self) i32 { return self.n; }
        }
        fn make() Counter { return Counter { n: 1 }; }
        fn main() {
            var v: i32 = make().value();
        }
    ";
    let ir = compile_to_ir(input).expect("Compilation failed");
    let calls = ir.matches("call %Counter @make()").count();
    assert_eq!(
        calls, 1,
        "expected exactly one call to @make\n\nFull IR:\n{ir}"
    );
}

#[test]
fn test_exhaustive_match_without_default_arm() {
    // The switch landed in the synthesised default block, after its
    // `unreachable`, leaving the entry block with no terminator.
    let input = "
        enum Color { Red, Green }
        fn main() {
            var c: Color = Color::Red;
            var n = match c {
                Color::Red => 1,
                Color::Green => 2
            };
        }
    ";
    assert_ir_contains(input, &["switch i32", "match_default"]);
}

#[test]
fn test_nested_for_allocates_in_entry_block() {
    // An alloca in the loop body grows the stack once per iteration.
    let input = "
        fn main() {
            var outer: Array<i32, 2> = [1, 2];
            var inner: Array<i32, 2> = [3, 4];
            var total: i32 = 0;
            for a in outer {
                for b in inner {
                    total += a + b;
                }
            }
        }
    ";
    let ir = compile_to_ir(input).expect("Compilation failed");
    let entry = ir
        .split("for_cond")
        .next()
        .expect("entry block should precede the loop");
    assert_eq!(
        ir.matches("alloca").count(),
        entry.matches("alloca").count(),
        "every alloca must sit in the entry block\n\nFull IR:\n{ir}"
    );
}

#[test]
fn test_block_scope_restores_shadowed_variable() {
    // The inner declaration used to overwrite the outer binding for good, so
    // the outer `x` read the inner slot after the block closed.
    let input = "
        fn main() {
            var x: i32 = 1;
            {
                var x: i32 = 2;
                var inner: i32 = x;
            }
            var outer: i32 = x;
        }
    ";
    // The shadowing slot is `%x1`, so a load from `%x` proves the outer
    // binding came back.
    assert_ir_contains(input, &["load i32, ptr %x,"]);
}

#[test]
fn test_optional_literal_takes_payload_type() {
    // `200` was typed as i32 against a `u8?` annotation and rejected.
    assert_compiles("fn main() { var c: u8? = 200; }");
}

#[test]
fn test_array_index_is_bounds_checked() {
    let input = "
        fn main() {
            var a: Array<i32, 2> = [1, 2];
            var i: i32 = 8;
            a[i] = 99;
        }
    ";
    assert_ir_contains(input, &["bounds_panic", "call void @abort()"]);
}

#[test]
fn test_release_fast_drops_bounds_check() {
    let input = "
        fn main() {
            var a: Array<i32, 2> = [1, 2];
            var i: i32 = 8;
            a[i] = 99;
        }
    ";
    let ir = compile_to_ir_with_mode(input, SafetyMode::ReleaseFast).expect("Compilation failed");
    assert!(!ir.contains("bounds_panic"), "Full IR:\n{ir}");
}

#[test]
fn test_integer_literal_beyond_i64_survives() {
    // The lexer used to fall back to 0 for anything `i64::from_str` rejected,
    // so `u64` literals above i64::MAX became zero with no diagnostic.
    let input = "fn main() { var a: u64 = 18446744073709551615; }";
    assert_ir_contains(input, &["store i64 -1"]);
}

#[test]
fn test_out_of_range_literal_is_rejected() {
    let err = compile_to_ir("fn main() { var a: u64 = 99999999999999999999999; }")
        .expect_err("literal does not fit any integer type");
    assert!(err.contains("out of range"), "got: {err}");
}

#[test]
fn test_vec_uses_its_element_type() {
    // Every element used to occupy a fixed 8-byte slot, so a `Vec<i32>` stored
    // four bytes and read eight back.
    let input = "
        fn main() {
            var v: Vec<i32> = Vec.new();
            v.push(11);
        }
    ";
    assert_ir_contains(input, &["getelementptr i32", "store i32 11"]);
    assert_ir_lacks(input, &["getelementptr i8"]);
}

#[test]
fn test_field_access_through_pointer() {
    // Only a receiver literally named `self` used to be followed, so any other
    // pointer failed to resolve its fields at all.
    let input = "
        struct S { v: i32, w: i32 }
        fn read(p: *S) i32 { return p.v; }
        fn write(p: *S) { p.w = 42; }
        fn main() { }
    ";
    assert_compiles(input);
}

#[test]
fn test_method_call_through_pointer() {
    let input = "
        struct S {
            v: i32,
            fn get(self) i32 { return self.v; }
            fn bump(var self) { self.v = self.v + 1; }
        }
        fn peek(p: *S) i32 { return p.get(); }
        fn poke(p: *S) { p.bump(); }
        fn main() { }
    ";
    assert_compiles(input);
}

#[test]
fn test_optional_methods() {
    // `T?` had no accessor at all, so `Vec::get` and `Vec::pop` returned values
    // nothing could read.
    let input = "
        fn main() {
            var some: i32? = 5;
            var none: i32? = None;
            var a: bool = some.is_some();
            var b: bool = none.is_none();
            var v: i32 = some.unwrap();
        }
    ";
    assert_ir_contains(input, &["opt_tag", "unwrap_none_panic"]);
}

#[test]
fn test_overflow_is_checked() {
    let input = "fn main() { var a: i32 = 1; var b: i32 = 2; var c = a + b; }";
    assert_ir_contains(input, &["llvm.sadd.with.overflow.i32", "overflow_panic"]);
}

#[test]
fn test_division_and_shift_are_checked() {
    let input = "
        fn main() {
            var a: i32 = 10;
            var b: i32 = 2;
            var q = a / b;
            var s = a << b;
        }
    ";
    assert_ir_contains(input, &["div_panic", "shift_panic", "div_overflow"]);
}

#[test]
fn test_release_fast_drops_arithmetic_checks() {
    let input = "
        fn main() {
            var a: i32 = 10;
            var b: i32 = 2;
            var c = a + b;
            var q = a / b;
            var s = a << b;
        }
    ";
    let ir = compile_to_ir_with_mode(input, SafetyMode::ReleaseFast).expect("Compilation failed");
    for pattern in ["overflow", "div_panic", "shift_panic"] {
        assert!(
            !ir.contains(pattern),
            "{pattern} survived\n\nFull IR:\n{ir}"
        );
    }
    assert!(ir.contains("add i32"), "Full IR:\n{ir}");
}

#[test]
fn test_same_scope_shadowing() {
    // The new binding is created after its initialiser runs, so the
    // initialiser still reads the old one, and the type may change.
    let input = "
        fn main() {
            var x: i32 = 1;
            var x: i32 = x + 1;
            var y: i32 = 5;
            var y: bool = y > 0;
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_constant_index_needs_no_bounds_check() {
    // The analyser settles a literal index, so the branch is dead weight.
    let input = "
        fn main() {
            var a: Array<i32, 4> = [1, 2, 3, 4];
            var v: i32 = a[2];
        }
    ";
    assert_ir_lacks(input, &["bounds_panic"]);
}

#[test]
fn test_reading_through_a_temporary() {
    // A call result has no storage, so it is spilled to a slot and read from
    // there, whatever the shape of the access.
    let input = "
        struct Inner { n: i32 }
        struct Holder { data: Array<i32, 3>, inner: Inner }
        fn make() Array<i32, 3> { return [1, 2, 3]; }
        fn wrap() Holder { return Holder { data: [1, 2, 3], inner: Inner { n: 7 } }; }
        fn main() {
            var first: i32 = make()[0];
            var i: usize = 1;
            var dynamic: i32 = make()[i];
            var field: i32 = wrap().inner.n;
            var nested: i32 = wrap().data[2];
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_deref_of_a_computed_pointer_reads_the_pointee_width() {
    // `p + 1` has no name to look up, so the width has to come from the type
    // the analyser resolved. Guessing reads the wrong number of bytes.
    let input = "
        fn main() {
            var bytes: Array<u8, 4> = [1, 2, 3, 4];
            var p: *u8 = &bytes[0];
            if *(p + 1) == 2 { return; }
        }
    ";
    // A comparison carries no expected type, so nothing else can supply the
    // width: an eight byte load here reads past a four byte array.
    assert_ir_lacks(input, &["load i64"]);
}

#[test]
fn test_pointer_arithmetic_steps_by_element() {
    // As in C: one step is one element, so a *i32 moves four bytes and lands on
    // the next element instead of inside the one it started on.
    let input = "
        fn main() {
            var words: Array<i32, 3> = [1, 2, 3];
            var p: *i32 = &words[0];
            *(p + 1) = 99;
        }
    ";
    assert_ir_contains(input, &["getelementptr i32"]);
}

#[test]
fn test_vec_mutation_reaches_any_place() {
    // A mutating method needs the storage, which is wherever the Vec lives:
    // a field, a field of a field, or an array slot, not only a variable.
    let input = "
        struct Bag { items: Vec<i64> }
        struct Nested { bag: Bag }
        fn main() {
            var b = Bag { items: Vec.new() };
            b.items.push(1);
            var n = Nested { bag: Bag { items: Vec.new() } };
            n.bag.items.push(2);
            var many: Array<Vec<i64>, 2> = [Vec.new(), Vec.new()];
            many[0].push(3);
            var p: *Bag = &b;
            p.items.push(4);
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_assignment_is_right_associative() {
    // `a = b = 5` groups as `a = (b = 5)`, so the inner assignment is the one
    // that has to typecheck against `a`, not the other way round.
    let input = "
        fn main() {
            var a: i32 = 0;
            var b: i32 = 0;
            a = b = 5;
        }
    ";
    let errors = compile_to_ir(input).expect_err("an assignment produces no value");
    assert!(
        errors
            .replace('\\', "")
            .contains(r#"Expected "i32", got "void""#),
        "unexpected error: {errors}"
    );
}

#[test]
fn test_generic_call_instantiates_for_the_argument_type() {
    // The argument is a field, not a literal or a plain variable. Guessing from
    // the shape of the expression reads i32 out of it and then passes an i64
    // into a function that takes an i32.
    let input = "
        struct Wide { first: i64, second: i64 }
        fn largest<T>(a: T, b: T) T {
            if a > b { return a; }
            return b;
        }
        fn main() {
            var w = Wide { first: 100, second: 200 };
            var big: i64 = largest(w.first, w.second);
        }
    ";
    assert_ir_contains(input, &["largest__i64_"]);
    assert_ir_lacks(input, &["largest__i32_"]);
}

#[test]
fn test_generic_struct_takes_its_arguments_from_the_values() {
    // Each set of arguments is a struct of its own, so the same declaration at
    // two widths lays out twice and the methods follow.
    let input = "
        struct Pair<T> {
            first: T,
            second: T,
            fn swap(self) Pair<T> { return Pair { first: self.second, second: self.first }; }
        }
        fn main() {
            var narrow = Pair { first: 1, second: 2 };
            var wide: Pair<i64> = Pair { first: 100, second: 200 };
            var swapped: i64 = wide.swap().first;
            var also: i32 = narrow.swap().second;
        }
    ";
    assert_ir_contains(input, &["%Pair__i32_ = type", "%Pair__i64_ = type"]);
}

#[test]
fn test_generic_struct_holds_a_parameterised_field() {
    // A parameter reached through another generic type: the field is a Vec of
    // whatever the struct was instantiated with.
    let input = "
        struct Holder<T> {
            items: Vec<T>,
            fallback: T,
            fn take(self, index: usize) T {
                var got = self.items.get(index);
                if got.is_none() { return self.fallback; }
                return got.unwrap();
            }
        }
        fn main() {
            var holder: Holder<i64> = Holder { items: Vec.new(), fallback: -1 };
            holder.items.push(3);
            var got: i64 = holder.take(0);
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_a_cast_applies_to_what_the_prefix_produced() {
    // `*p as u64` reads the pointer and widens the byte. Grouping it the other
    // way casts the pointer itself, which is not a thing to dereference.
    let input = "
        fn at(p: *u8) u64 { return *p as u64; }
        fn main() {
            var bytes: Array<u8, 2> = [7, 8];
            var value: u64 = at(&bytes[0]);
        }
    ";
    assert_ir_contains(input, &["load i8"]);
}

#[test]
fn test_vec_is_indexed_like_an_array() {
    // A Vec keeps its elements elsewhere, so an index loads the data pointer
    // and checks against the length the header carries.
    let input = "
        struct Item { id: i32 }
        fn main() {
            var v: Vec<i64> = Vec.new();
            v.push(1);
            var read: i64 = v[0];
            v[0] = 2;
            v[0] += 3;

            var items: Vec<Item> = Vec.new();
            items.push(Item { id: 7 });
            items[0].id = 8;

            var grid: Vec<Vec<i64>> = Vec.new();
            grid.push(v.copy());
            grid[0][0] = 9;
        }
    ";
    assert_ir_contains(input, &["bounds_panic"]);
}

#[test]
fn test_release_fast_drops_the_vec_bounds_check() {
    let input = "
        fn main() {
            var v: Vec<i64> = Vec.new();
            v.push(1);
            var read: i64 = v[0];
        }
    ";
    let ir = compile_to_ir_with_mode(input, SafetyMode::ReleaseFast).expect("Compilation failed");
    assert!(
        !ir.contains("bounds_panic"),
        "ReleaseFast should not check bounds:\n{ir}"
    );
}

#[test]
fn test_slice_is_indexed_for_reading() {
    // A slice is a pointer and a length, so an element is reached the way a
    // Vec's is; without this there is no way to look inside a str at all.
    let input = "
        fn main() {
            var text: str = \"abc\";
            var first: u8 = text[0];
            var last: u8 = text[text.len() - 1];
        }
    ";
    assert_ir_contains(input, &["bounds_panic"]);
}

#[test]
fn test_for_in_walks_a_vec() {
    // A Vec keeps its elements elsewhere and its length in the header, so the
    // loop counts against the loaded length and fetches the buffer each turn.
    let input = "
        struct Item { weight: i32 }
        fn main() {
            var items: Vec<Item> = Vec.new();
            items.push(Item { weight: 2 });
            var total: i32 = 0;
            for item in items {
                total += item.weight;
            }
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_tuple_field_is_a_place() {
    // A tuple has no field names, so the number is the index, and reaching one
    // goes through the same path a struct field does: it can be written too.
    let input = "
        fn main() {
            var p: (i32, i64) = (3, 4);
            var read: i64 = p.1;
            p.0 = 5;
            p.1 += 6;
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_tuple_literal_takes_the_expected_field_types() {
    // Without the expected type each element is typed on its own, so an i64
    // field holding a small literal comes out as an i32 the return rejects.
    let input = "
        fn make() (i32, i64) { return (7, 8); }
        fn main() {
            var got = make();
            var wide: i64 = got.1;
        }
    ";
    assert_ir_contains(input, &["{ i32, i64 }"]);
    assert_ir_lacks(input, &["{ i32, i32 }"]);
}

#[test]
fn test_a_mutable_receiver_is_reached_wherever_it_lives() {
    // `var self` takes the receiver by pointer, and the place it names need not
    // be a variable: an element or a field has storage just the same.
    let input = "
        struct Counter {
            n: i32,
            fn bump(var self) { self.n += 1; }
        }
        struct Holder { counters: Vec<Counter> }
        fn main() {
            var row: Array<Counter, 2> = [Counter { n: 0 }, Counter { n: 0 }];
            row[1].bump();

            var holder = Holder { counters: Vec.new() };
            holder.counters.push(Counter { n: 0 });
            holder.counters[0].bump();

            var single = Counter { n: 0 };
            var through: *Counter = &single;
            through.bump();
        }
    ";
    assert_compiles(input);
}

#[test]
fn test_static_method_takes_a_dot() {
    // A type on the left of the dot is a call on the type, which is the same
    // function `Box::empty()` names.
    let input = "
        struct Box {
            n: i32,
            fn empty() Box { return Box { n: 0 }; }
        }
        fn main() {
            var byDot = Box.empty();
            var byPath = Box::empty();
        }
    ";
    assert_compiles(input);
}
