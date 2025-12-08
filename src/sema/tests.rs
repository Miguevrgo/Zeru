#[cfg(test)]
mod testing {
    use crate::parser::Parser;
    use crate::{lexer::Lexer, sema::analyzer::SemanticAnalyzer};

    fn analyze(input: &str) -> Vec<String> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            panic!("Parser errors in test: {:?}", parser.errors);
        }

        let mut analyzer = SemanticAnalyzer::new();
        analyzer.analyze(&program);
        analyzer.errors
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
                var val = p.z; // Field does not exist
            }
        ";
        let errors = analyze(input);
        assert!(!errors.is_empty());
        assert!(errors[0].contains("has no field"));
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
    fn test_function_call_args_mismatch() {
        let input = "
            fn add(a: i32, b: i32) i32 { return a + b; }
            fn main() {
                add(10); // Missing argument
            }
        ";
        let errors = analyze(input);
        assert!(!errors.is_empty());
        assert!(errors[0].contains("expects 2 arguments"));
    }
}
