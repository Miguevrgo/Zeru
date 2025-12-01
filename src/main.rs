mod lexer;
mod token;

use crate::lexer::Lexer;
use crate::token::Token;

fn main() {
    let input = r#"
        import std.math
        fn main() {
            var x = 10.5;
            if x == 10.5 {
                print("Hello World!");
            }
        }
    "#;
    const BOLD: &str = "\x1b[1m";
    const RESET: &str = "\x1b[0m";
    println!("{BOLD}\x1b[31mInput:{RESET}\n{input}");
    println!("{BOLD}\x1b[32mTokens:{RESET}");

    let mut lexer = Lexer::new(input);
    loop {
        let token = lexer.next_token();
        println!("\t{token:?}");
        if token == Token::Eof {
            break;
        }
    }
}
