use crate::codegen::SafetyMode;
use crate::codegen::compiler::Compiler;
use crate::errors::Sources;
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

    let mut sources = Sources::default();
    sources.push("test.zr", input);
    let mut compiler = Compiler::new(
        &context,
        &builder,
        &module,
        &analyzer,
        &sources,
        safety_mode,
    );
    compiler.compile_program(&program);

    if !compiler.errors.is_empty() {
        let msgs: Vec<_> = compiler.errors.iter().map(|e| e.message.as_str()).collect();
        return Err(format!("Codegen errors: {:?}", msgs));
    }
    module.verify().map_err(|e| e.to_string())?;

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
fn test_signed_widening_cast_sign_extends() {
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
fn test_method_call_on_temporary_evaluates_receiver_once() {
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
fn test_nested_for_allocates_in_entry_block() {
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
fn test_optional_literal_takes_payload_type() {
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
fn test_field_access_through_pointer() {
    let input = "
        struct S { v: i32, w: i32 }
        fn read(p: *S) i32 { return p.v; }
        fn write(p: *S) { p.w = 42; }
        fn main() { }
    ";
    assert_compiles(input);
}

#[test]
fn test_optional_methods() {
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
fn test_constant_index_needs_no_bounds_check() {
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
fn test_vec_mutation_reaches_any_place() {
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
fn test_generic_call_instantiates_for_the_argument_type() {
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
fn test_a_cast_applies_to_what_the_prefix_produced() {
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
fn test_tuple_literal_takes_the_expected_field_types() {
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
fn test_debug_build_has_line_tables() {
    let ir = compile_to_ir("fn main() {\n    var x = 1;\n}").unwrap();
    for pattern in [
        "!DISubprogram(name: \"main\"",
        "!DILocation(line: 2, column: 5",
    ] {
        assert!(ir.contains(pattern), "{pattern} missing:\n{ir}");
    }
    let ir = compile_to_ir_with_mode("fn main() { }", SafetyMode::ReleaseSafe).unwrap();
    assert!(!ir.contains("!DISubprogram"), "{ir}");
}

#[test]
fn test_a_panic_says_what_and_where() {
    let input = "fn main() {\n    var a = [1, 2];\n    var i: usize = 5;\n    var b = a[i];\n}";
    assert_ir_contains(input, &["panic at test.zr:4:5: index out of bounds\\0A"]);
}
