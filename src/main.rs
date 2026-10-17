mod ast;
mod codegen;
mod errors;
mod generics;
mod lexer;
mod modules;
mod parser;
mod resolver;
mod sema;
mod token;

use crate::resolver::{GREEN, RED, RESET, compile_pipeline, status};
use crate::{codegen::SafetyMode, errors::CompileError};
use clap::{Parser, Subcommand};
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};

#[derive(Parser)]
#[command(version, about = "The Zeru compiler")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    #[command(about = "Compile a .zr file into build/")]
    Build {
        file: PathBuf,
        #[arg(
            long,
            conflicts_with = "release_fast",
            help = "Optimize, keep runtime checks"
        )]
        release_safe: bool,
        #[arg(long, help = "Optimize, drop runtime checks")]
        release_fast: bool,
        #[arg(long, help = "Keep the LLVM IR in build/")]
        emit_ir: bool,
    },
    #[command(about = "Compile a .zr file and run it")]
    Run {
        file: PathBuf,
        #[arg(
            long,
            conflicts_with = "release_fast",
            help = "Optimize, keep runtime checks"
        )]
        release_safe: bool,
        #[arg(long, help = "Optimize, drop runtime checks")]
        release_fast: bool,
    },
    #[command(about = "Remove the build/ directory")]
    Clean,
}

fn run_compiler(args: Cli) -> Result<(), CompileError> {
    match args.command {
        Commands::Build {
            file,
            release_safe,
            release_fast,
            emit_ir,
        } => {
            let safety_mode = SafetyMode::from_flags(release_fast, release_safe);
            compile_pipeline(&file, safety_mode, emit_ir)?;
        }
        Commands::Run {
            file,
            release_safe,
            release_fast,
        } => {
            let safety_mode = SafetyMode::from_flags(release_fast, release_safe);
            let executable_path = compile_pipeline(&file, safety_mode, false)?;
            status(GREEN, '\u{e7d5}', "Running", executable_path.display());
            let status = std::process::Command::new(&executable_path).status()?;
            std::process::exit(
                status
                    .code()
                    .unwrap_or_else(|| status.signal().map(|s| 128 + s).unwrap_or(1)),
            );
        }
        Commands::Clean => {
            let build_dir = Path::new("build");
            if build_dir.exists() {
                std::fs::remove_dir_all(build_dir)?;
                status(GREEN, '\u{ea81}', "Cleaned", build_dir.display());
            }
        }
    }
    Ok(())
}

fn main() {
    if let Err(err) = run_compiler(Cli::parse()) {
        eprintln!("{RED}[-] Error: {err}{RESET}");
        std::process::exit(1);
    }
}
