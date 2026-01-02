mod ast;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod sema;
mod token;

use inkwell::context::Context;
use std::collections::HashSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::codegen::SafetyMode;
use crate::codegen::compiler::Compiler;
use crate::errors::report_errors;
use crate::lexer::Lexer;
use crate::sema::analyzer::SemanticAnalyzer;
use crate::token::Token;

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

        #[arg(long)]
        release_safe: bool,

        #[arg(long)]
        release_fast: bool,

        #[arg(long)]
        emit_ir: bool,
    },
    Run {
        file: PathBuf,

        #[arg(long)]
        release_safe: bool,

        #[arg(long)]
        release_fast: bool,
    },
    Clean,
}

fn resolve_std_import(import_path: &str) -> Option<PathBuf> {
    let parts: Vec<&str> = import_path.split('.').collect();
    if parts.is_empty() || parts[0] != "std" {
        return None;
    }

    if parts.len() == 1 {
        return Some(PathBuf::from("std/builtin.zr"));
    }

    let module_name = parts[1..].join("/");
    Some(PathBuf::from(format!("std/{module_name}.zr")))
}

/// Extracts import paths from code efficiently by scanning only the top of the file.
/// Since imports must appear at the beginning of the file, we stop as soon as we
/// encounter a non-import token.
fn extract_imports(code: &str) -> Vec<String> {
    let mut lexer = Lexer::new(code);
    let mut imports = Vec::new();

    loop {
        let (token, _, _) = lexer.next_token();

        match token {
            Token::Import => {
                let mut path_parts = Vec::new();

                if let (Token::Identifier(name), _, _) = lexer.next_token() {
                    path_parts.push(name);
                } else {
                    break;
                }

                loop {
                    let (next, _, _) = lexer.next_token();
                    if next == Token::Dot {
                        if let (Token::Identifier(name), _, _) = lexer.next_token() {
                            path_parts.push(name);
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                if !path_parts.is_empty() {
                    imports.push(path_parts.join("."));
                }
            }
            Token::Eof => break,
            _ => break,
        }
    }

    imports
}

fn load_std_modules(imports: &[String], loaded: &mut HashSet<String>) -> String {
    let mut code = String::new();

    for import_path in imports {
        if loaded.contains(import_path) {
            continue;
        }

        if let Some(file_path) = resolve_std_import(import_path)
            && file_path.exists()
            && let Ok(content) = fs::read_to_string(&file_path)
        {
            loaded.insert(import_path.clone());

            let nested_imports = extract_imports(&content);
            let nested_code = load_std_modules(&nested_imports, loaded);

            code.push_str(&nested_code);
            code.push_str(&content);
            code.push('\n');
        }
    }

    code
}

fn compile_pipeline(
    path: &Path,
    safety_mode: SafetyMode,
    force_emit_ir: bool,
    quiet: bool,
) -> Option<PathBuf> {
    if path.extension().and_then(|s| s.to_str()) != Some("zr") {
        eprintln!("⚠️ Warning: Zeru extension is .zr");
    }

    let filename = path.file_stem().unwrap().to_str().unwrap();
    let build_dir = Path::new("build");

    let ir_path = build_dir.join(format!("{}.ll", filename));
    let exe_path = build_dir.join(filename);

    let mode_str = match safety_mode {
        SafetyMode::Debug => "DEBUG",
        SafetyMode::ReleaseSafe => "RELEASE-SAFE",
        SafetyMode::ReleaseFast => "RELEASE-FAST",
    };

    if !quiet {
        println!("  Compiling {} [{}]...", filename, mode_str);
    }

    let user_code = match fs::read_to_string(path) {
        Ok(code) => code,
        Err(e) => {
            eprintln!("❌ Error reading file: {}", e);
            return None;
        }
    };

    let std_builtin = include_str!("../std/builtin.zr");
    let user_imports = extract_imports(&user_code);
    let mut loaded_modules = HashSet::new();
    loaded_modules.insert("std.builtin".to_string());

    let additional_std = load_std_modules(&user_imports, &mut loaded_modules);
    let input = format!("{}\n{}\n{}", std_builtin, additional_std, user_code);

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

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode.clone());
    compiler.compile_program(&program);

    if let Err(e) = module.verify() {
        eprintln!("❌ LLVM Verify Error: {}", e.to_string());
        return None;
    }

    if let Err(e) = module.print_to_file(&ir_path) {
        eprintln!("❌ Failed to write LLVM IR: {}", e);
        return None;
    }

    let (opt_level, debug_flag) = match &safety_mode {
        SafetyMode::Debug => ("-O0", Some("-g")),
        SafetyMode::ReleaseSafe => ("-O2", None),
        SafetyMode::ReleaseFast => ("-O3", None),
    };

    let mut cmd = Command::new("clang");
    cmd.arg(&ir_path)
        .arg("-o")
        .arg(&exe_path)
        .arg(opt_level)
        .arg("-Wno-override-module");

    if let Some(flag) = debug_flag {
        cmd.arg(flag);
    }

    let status = cmd.status();

    match status {
        Ok(s) if s.success() => {
            if !quiet {
                println!("✅ Build successful: {}", exe_path.display());
            }
            let should_keep_ir = force_emit_ir || safety_mode == SafetyMode::Debug;

            if !should_keep_ir {
                let _ = fs::remove_file(ir_path);
            } else if !quiet {
                println!("  IR preserved: {}", ir_path.display());
            }

            Some(exe_path)
        }
        _ => {
            eprintln!("❌ Linking failed. Ensure 'clang' is installed.");
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
            release_safe,
            release_fast,
            emit_ir,
        } => {
            let safety_mode = if *release_fast {
                SafetyMode::ReleaseFast
            } else if *release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            if compile_pipeline(file, safety_mode, *emit_ir, false).is_none() {
                std::process::exit(1);
            };
        }
        Commands::Run {
            file,
            release_safe,
            release_fast,
        } => {
            let safety_mode = if *release_fast {
                SafetyMode::ReleaseFast
            } else if *release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            if let Some(executable_path) = compile_pipeline(file, safety_mode, false, true) {
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
