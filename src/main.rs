mod ast;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod resolver;
mod sema;
mod token;

use crate::codegen::SafetyMode;
use crate::errors::CompileError;
use crate::resolver::{GREEN_FG, RED_FG, RESET, compile_pipeline, status};
use clap::{Parser, Subcommand};
use std::fs;
use std::os::unix::process::ExitStatusExt;
use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Parser)]
#[command(version)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Build {
        file: PathBuf,
        #[arg(long, conflicts_with = "release_fast")]
        release_safe: bool,
        #[arg(long, conflicts_with = "release_safe")]
        release_fast: bool,
        #[arg(long)]
        emit_ir: bool,
    },
    Run {
        file: PathBuf,
        #[arg(long, conflicts_with = "release_fast")]
        release_safe: bool,
        #[arg(long, conflicts_with = "release_safe")]
        release_fast: bool,
    },
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
            status(GREEN_FG, '\u{e7d5}', "Running", executable_path.display());
            let status = Command::new(&executable_path).status()?;
            std::process::exit(
                status
                    .code()
                    .unwrap_or_else(|| status.signal().map(|s| 128 + s).unwrap_or(1)),
            );
        }
        Commands::Clean => {
            let build_dir = Path::new("build");
            if build_dir.exists() {
                fs::remove_dir_all(build_dir)?;
                status(GREEN_FG, '\u{ea81}', "Cleaned", build_dir.display());
            }
        }
    }
    Ok(())
}

fn main() {
    if let Err(err) = run_compiler(Cli::parse()) {
        eprintln!("{RED_FG}[-] Error: {err}{RESET}");
        std::process::exit(1);
    }
}
