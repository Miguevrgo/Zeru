mod ast;
mod lexer;
mod parser;
mod sema;
mod token;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let input = r#"
    import std.math
    import std.os;
    import std.collections::Array;

    struct Vector3 {
        x: f32,
        y: f32,
        z: f32,

        fn new(x: f32, y: f32) Vector3 {
            return Vector3 {x: x, y: y, z: 0};
        }

        fn sum(self, amount: i32) i32 {
            self.x += amount as f32;
            self.y += amount as f32;
            self.z += amount as f32;
        }
    }

    fn print_value(val: f32) f32 {
        println("{}", y);
        return y;
    }

    fn main() {
        const pos: Vector3 = create_vector(10.5, 20.0);
        const numbers: Array = [1, 2, 3];
        var count = 100;

        if pos.x < 50.0 {
            print("Position is small\n");
        } else if pos.x > 100.0 {
            println("Position is big", pos.x * 2.0);
        } else {
            println(stderr, "\tPosition should not be inside \"[50-100]\"");
        }

        while count > 0 {
            count = count - 1;
            count -= count.clamp(0 as u32, 200 as u32);
        }

        const lines: Vector = file_content.split_lines();

        for line in lines {
            const content = os::read("file.txt");
            if content != None {
                output_file.write("{}", key.value);
            } else {
                break;
            }
        }
    }
    "#;
    const BOLD: &str = "\x1b[1m";
    const RESET: &str = "\x1b[0m";
    println!("{BOLD}\x1b[31mInput:{RESET}\n{input}");
    println!("{BOLD}\x1b[32mTokens:{RESET}");

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        println!("Errors detected during parsing:");
        for err in parser.errors {
            println!("\t{}", err);
        }
    } else {
        println!("AST generated:\n{:#?}", program);
    }
}
