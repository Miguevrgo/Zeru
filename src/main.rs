mod ast;
mod codegen;
mod lexer;
mod parser;
mod sema;
mod token;

use inkwell::context::Context;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::codegen::compiler::Compiler;
use crate::lexer::Lexer;
use crate::sema::analyzer::SemanticAnalyzer;

use clap::{Parser, Subcommand};

#[derive(Parser)]
#[command(name = "zeru")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        file: PathBuf,
        #[arg(short, long, default_value = "out")]
        output: String,
    },
    Run {
        file: PathBuf,
    },
    EmitIr {
        file: PathBuf,
    },
}

#[derive(PartialEq)]
enum OutputType {
    Binary,
    IR,
}

fn compile_pipeline(path: &Path, output_name: &str, out_type: OutputType) -> bool {
    let input = match fs::read_to_string(path) {
        Ok(code) => code,
        Err(e) => {
            println!("❌ Error reading file: {}", e);
            return false;
        }
    };

    let lexer = Lexer::new(&input);
    let mut parser = crate::parser::Parser::new(lexer);
    let program = parser.parse_program();

    if !parser.errors.is_empty() {
        println!("❌ Parser Errors:");
        for err in parser.errors {
            println!("\t{}", err);
        }
        return false;
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program);

    if !analyzer.errors.is_empty() {
        println!("❌ Semantic Errors:");
        for err in analyzer.errors {
            println!("\t{}", err);
        }
        return false;
    }

    let context = Context::create();
    let module = context.create_module(path.file_stem().unwrap().to_str().unwrap());
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module);
    compiler.compile_program(&program);

    if let Err(e) = module.verify() {
        println!("❌ LLVM Verify Error: {}", e.to_string());
        return false;
    }

    let ir_path = format!("{}.ll", output_name);
    if let Err(e) = module.print_to_file(&ir_path) {
        println!("❌ Failed to write LLVM IR: {}", e);
        return false;
    }

    if out_type == OutputType::Binary {
        let status = Command::new("clang")
            .arg(&ir_path)
            .arg("-o")
            .arg(output_name)
            .arg("-Wno-override-module")
            .status();

        match status {
            Ok(s) if s.success() => {
                println!("✅ Build successful: ./{}", output_name);
                let _ = fs::remove_file(ir_path);
                true
            }
            _ => {
                println!("❌ Linking failed. Ensure 'clang' is installed.");
                false
            }
        }
    } else {
        println!("✅ IR generated: {}", ir_path);
        true
    }
}

fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Build { file, output } => {
            compile_pipeline(file, output, OutputType::Binary);
        }
        Commands::Run { file } => {
            let output_name = "temp_run";
            if compile_pipeline(file, output_name, OutputType::Binary) {
                let status = Command::new(format!("./{}", output_name))
                    .status()
                    .expect("Failed to run executable");

                if let Some(code) = status.code() {
                    println!("Process exited with code: {}", code);
                }
                let _ = fs::remove_file(output_name);
            }
        }
        Commands::EmitIr { file } => {
            let output_name = file.file_stem().unwrap().to_str().unwrap();
            compile_pipeline(file, output_name, OutputType::IR);
        }
    }
}
