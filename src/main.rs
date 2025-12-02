mod ast;
mod lexer;
mod parser;
mod token;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let input = r#"
        fn foo(x: f32, y: i32) i32 {
            const Z = x * y;
            return Z;
        }
    
        fn main() {
            var x = 10.5 + 5 * 2;
            pos.x;
            print(x);
            const Y: i32 = 20;
            return;
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
