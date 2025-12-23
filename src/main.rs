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
use std::time::Instant;

use crate::codegen::SafetyMode;
use crate::codegen::compiler::Compiler;
use crate::errors::report_errors;
use crate::lexer::Lexer;
use crate::sema::analyzer::SemanticAnalyzer;

use clap::{Parser, Subcommand};

fn print_stats(loc: usize, lex_parse_us: u128, sema_us: u128, codegen_us: u128, verify_us: u128) {
    let total = lex_parse_us + sema_us + codegen_us + verify_us;
    let loc_f = loc as f64;
    let rate = |us: u128| {
        if us > 0 {
            loc_f / (us as f64 / 1_000_000.0)
        } else {
            0.0
        }
    };

    println!("\n  {:>12} {:>10} {:>12}", "Phase", "µs", "LOC/s");
    println!("  {:->12} {:->10} {:->12}", "", "", "");
    println!(
        "  {:>12} {:>10} {:>12.0}",
        "Lex+Parse",
        lex_parse_us,
        rate(lex_parse_us)
    );
    println!("  {:>12} {:>10} {:>12.0}", "Sema", sema_us, rate(sema_us));
    println!(
        "  {:>12} {:>10} {:>12.0}",
        "Codegen",
        codegen_us,
        rate(codegen_us)
    );
    println!(
        "  {:>12} {:>10} {:>12.0}",
        "LLVM Verify",
        verify_us,
        rate(verify_us)
    );
    println!("  {:->12} {:->10} {:->12}", "", "", "");
    println!(
        "  {:>12} {:>10} {:>12.0}  ({} LOC)",
        "Total",
        total,
        rate(total),
        loc
    );
}

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
        #[arg(long)]
        stats: bool,
    },
    Run {
        file: PathBuf,
        #[arg(long)]
        release_safe: bool,
        #[arg(long)]
        release_fast: bool,
        #[arg(long)]
        stats: bool,
    },
    Clean,
}

fn compile_pipeline(
    path: &Path,
    safety_mode: SafetyMode,
    force_emit_ir: bool,
    quiet: bool,
    show_stats: bool,
) -> Option<PathBuf> {
    if path.extension().and_then(|s| s.to_str()) != Some("zr") {
        println!("⚠️ Warning: Zeru extension is .zr");
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
            println!("❌ Error reading file: {}", e);
            return None;
        }
    };

    let std_io = include_str!("../std/builtin.zr");
    let input = format!("{}\n{}", std_io, user_code);

    let loc = input
        .lines()
        .filter(|l| {
            let t = l.trim();
            !t.is_empty() && !t.starts_with("//")
        })
        .count();

    let t0 = Instant::now();
    let lexer = Lexer::new(&input);
    let mut parser = crate::parser::Parser::new(lexer);
    let program = parser.parse_program();
    let lex_parse_us = t0.elapsed().as_micros();

    let filepath_str = path.to_str().unwrap_or("unknown");

    if !parser.errors.is_empty() {
        report_errors(&parser.errors, filepath_str, &input);
        return None;
    }

    let t0 = Instant::now();
    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program);
    let sema_us = t0.elapsed().as_micros();

    if !analyzer.errors.is_empty() {
        report_errors(&analyzer.errors, filepath_str, &input);
        return None;
    }

    let t0 = Instant::now();
    let context = Context::create();
    let module = context.create_module(filename);
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode.clone());
    compiler.compile_program(&program);
    let codegen_us = t0.elapsed().as_micros();

    let t0 = Instant::now();
    if let Err(e) = module.verify() {
        println!("❌ LLVM Verify Error: {}", e.to_string());
        return None;
    }
    let verify_us = t0.elapsed().as_micros();

    if let Err(e) = module.print_to_file(&ir_path) {
        println!("❌ Failed to write LLVM IR: {}", e);
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
            if show_stats {
                print_stats(loc, lex_parse_us, sema_us, codegen_us, verify_us);
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
            release_safe,
            release_fast,
            emit_ir,
            stats,
        } => {
            let safety_mode = if *release_fast {
                SafetyMode::ReleaseFast
            } else if *release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            compile_pipeline(file, safety_mode, *emit_ir, false, *stats);
        }
        Commands::Run {
            file,
            release_safe,
            release_fast,
            stats,
        } => {
            let safety_mode = if *release_fast {
                SafetyMode::ReleaseFast
            } else if *release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            if let Some(executable_path) = compile_pipeline(file, safety_mode, false, true, *stats)
            {
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
