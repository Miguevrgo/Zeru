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
    enum Status { Alive, Dead, Unknown }

    struct Player {
        name: String,
        health: i32,

        fn new(n: String) Player {
            return Player { name: n, health: 100 };
        }

        fn take_damage(self, amount: i32) {
            self.health -= amount;
        }

        fn heal(self) {
            self.health = 100;
        }
    }

    struct Vector2 {
        x: f32,
        y: f32,
    }

    fn make_vec(a: f32) Vector2 {
        return Vector2 { x: a, y: a * 2.0 };
    }

    fn main() {
        // Struct + Functions test
        var p = Player { name: "Hero", health: 100 };
 
        p.take_damage(20);

        var list: Array<i32, 3> = [1, 2, 3];
        var zeros: Array<u8, 10> = [0; 10];
        var auto = [10; 4];

        // Shadowing
        var x: f64 = 5.5;
        var x: i64 = 10; // Shadowing

        var level = 1;
        var rank = match level {
            1 => "Novice",
            2 => "Pro",
            default => "God"
        };

        // For Each
        const text: String = "Hello World";
        var text_mayus: String = text;
        var i = 0;
        for ch in text {
            var upper = ch - 32;
            text_mayus[i] = upper;
            i += 1;
        }

        const zero: i64 = 0;
        while (x > zero) {
            x -= (1 as i64);
        }
 
        // Mutability
        const PI: f32 = 3.14159;
        // PI = 3.0;

        // Scopes and blocks
        if p.health < 50 || false {
            p.heal();
        }

        var v = 10;
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
        println!("{BOLD}\x1b[31m❌ Semantic Errors:{RESET}");
        for err in analyzer.errors {
            println!("\t{}", err);
        }
    } else {
        println!("✅ Semantic Analysis OK! Symbol Table Validated.");
        println!("   (Ready for Code Generation in Phase 2)");
    }
}
