mod ast;
mod lexer;
mod parser;
mod sema;
mod token;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::sema::analyzer::SemanticAnalyzer;

fn main() {
    let input = r#"
    struct Vector2 {
        x: f32,
        y: f32
    }

    fn make_vec(a: f32) Vector2 {
        return Vector2 { x: a, y: a * 2.0 };
    }

    fn main() {
        // Struct + Functions test
        var v = make_vec(10.0);
 
        // Shadowing 
        var v = 100; 

        // Mutability
        const PI: f32 = 3.14159;
        // PI = 3.0;

        // Scopes and blocks
        if v > 50 {
            var inner = 1;
            v = v + inner;
        }
        // inner = 2;

        // var error = v + 50.0;
    }
    "#;
    const BOLD: &str = "\x1b[1m";
    const RESET: &str = "\x1b[0m";
    println!("{BOLD}\x1b[31mInput:{RESET}\n{input}");
    println!("{BOLD}\x1b[32mTokens:{RESET}");

    println!("Compiling Zeru...\n");

    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        println!("❌ Parser Errors:");
        for err in parser.errors {
            println!("\t{}", err);
        }
        return;
    }

    println!("✅ Parsing OK. Running Semantic Analysis...");

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program);

    if !analyzer.errors.is_empty() {
        println!("❌ Semantic Errors:");
        for err in analyzer.errors {
            println!("\t{}", err);
        }
    } else {
        println!("✅ Semantic Analysis OK! Symbol Table Validated.");
        println!("   (Ready for Code Generation in Phase 2)");
    }
}
