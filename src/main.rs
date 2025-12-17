mod ast;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod sema;
mod token;

use inkwell::context::Context;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::codegen::compiler::Compiler;
use crate::errors::report_errors;
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

        #[arg(short, long)]
        release: bool,

        #[arg(long)]
        emit_ir: bool,
    },
    Run {
        file: PathBuf,
        #[arg(short, long)]
        release: bool,
    },
    Clean,
}

fn compile_pipeline(
    path: &Path,
    release: bool,
    force_emit_ir: bool,
    quiet: bool,
) -> Option<PathBuf> {
    if path.extension().and_then(|s| s.to_str()) != Some("zr") {
        println!("⚠️ Warning: Zeru files usually end with .zr");
    }

    let filename = path.file_stem().unwrap().to_str().unwrap();
    let build_dir = Path::new("build");

    let ir_path = build_dir.join(format!("{}.ll", filename));
    let exe_path = build_dir.join(filename);

    if !quiet {
        println!(
            "  Compiling {} [{}]...",
            filename,
            if release { "RELEASE" } else { "DEBUG" }
        );
    }

    let input = match fs::read_to_string(path) {
        Ok(code) => code,
        Err(e) => {
            println!("❌ Error reading file: {}", e);
            return None;
        }
    };

    let lexer = Lexer::new(&input);
    let mut parser = crate::parser::Parser::new(lexer);
    let program = parser.parse_program();

    let filepath_str = path.to_str().unwrap_or("unknown");

    if !parser.errors.is_empty() {
        report_errors(&parser.errors, filepath_str, &input);
        return None;
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program);

    if !analyzer.errors.is_empty() {
        report_errors(&analyzer.errors, filepath_str, &input);
        return None;
    }

    let context = Context::create();
    let module = context.create_module(filename);
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module);
    compiler.compile_program(&program);

    if let Err(e) = module.verify() {
        println!("❌ LLVM Verify Error: {}", e.to_string());
        return None;
    }

    if let Err(e) = module.print_to_file(&ir_path) {
        println!("❌ Failed to write LLVM IR: {}", e);
        return None;
    }

    let optimization_flag = if release { "-O3" } else { "-O0" };

    let debug_flag = if release { "" } else { "-g" };

    let mut cmd = Command::new("clang");
    cmd.arg(ir_path.to_str().unwrap())
        .arg("-o")
        .arg(exe_path.to_str().unwrap())
        .arg(optimization_flag)
        .arg("-Wno-override-module");

    if !debug_flag.is_empty() {
        cmd.arg(debug_flag);
    }

    let status = cmd.status();

    match status {
        Ok(s) if s.success() => {
            if !quiet {
                println!("✅ Build successful: {}", exe_path.display());
            }
            let should_keep_ir = force_emit_ir || !release;

            if !should_keep_ir {
                let _ = fs::remove_file(ir_path);
            } else if !quiet {
                println!("  IR preserved: {}", ir_path.display());
            }

            Some(exe_path)
        }
        _ => {
            println!("❌ Linking failed. Ensure 'clang' is installed.");
            None
        }
    }
}

fn main() {
    let cli = Cli::parse();

    let build_dir = Path::new("build");
    if !build_dir.exists() {
        fs::create_dir(build_dir).expect("Failed to create build directory");
    }

    match &cli.command {
        Commands::Build {
            file,
            release,
            emit_ir,
        } => {
            compile_pipeline(file, *release, *emit_ir, false);
        }
        Commands::Run { file, release } => {
            if let Some(executable_path) = compile_pipeline(file, *release, false, true) {
                println!(" Running {}...\n", executable_path.display());

                let status = Command::new(&executable_path)
                    .status()
                    .expect("Failed to run executable");

                if let Some(code) = status.code() {
                    println!("\nProcess exited with code: {}", code);
                }
            }
        }
        Commands::Clean => {
            // It is guaranteed but lets keep it redundant for safety
            if build_dir.exists() {
                fs::remove_dir_all(build_dir).expect("Failed to clean build directory");
                println!("✅ Build directory cleaned");
            }
        }
    }
}
