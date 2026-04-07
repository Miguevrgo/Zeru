mod ast;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod resolver;
mod sema;
mod token;

use crate::resolver::{GREEN_FG, RED_FG, RESET, compile_pipeline};
use clap::{Parser, Subcommand};
use inkwell::support::LLVMString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use thiserror::Error;

use crate::codegen::SafetyMode;

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

#[derive(Error, Debug)]
enum CompileError {
    #[error("[IO]: {0}")]
    Io(#[from] std::io::Error),
    #[error("Not defined YET")]
    Unknown,
    #[error("Not defined YET")]
    Llvm(#[from] LLVMString),
}

fn run_compiler(args: Cli) -> Result<(), CompileError> {
    match args.command {
        Commands::Build {
            file,
            release_safe,
            release_fast,
            emit_ir,
        } => {
            let safety_mode = if release_fast {
                SafetyMode::ReleaseFast
            } else if release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            compile_pipeline(&file, safety_mode, emit_ir)?;
        }
        Commands::Run {
            file,
            release_safe,
            release_fast,
        } => {
            let safety_mode = if release_fast {
                SafetyMode::ReleaseFast
            } else if release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            let executable_path = compile_pipeline(&file, safety_mode.clone(), false)?;
            eprintln!(
                "  {GREEN_FG} Running   {RESET}{}",
                executable_path.display()
            );
            Command::new(&executable_path).status()?;
        }
        Commands::Clean => {
            let build_dir = Path::new("build");
            if build_dir.exists() {
                fs::remove_dir_all(build_dir)?;
                println!("{GREEN_FG}✅ Build directory cleaned{RESET}");
            }
        }
    }
    Ok(())
}

fn main() {
    let cli = Cli::parse();
    run_compiler(cli).unwrap_or_else(|err| panic!("{RED_FG}[-] Error: {err}{RESET}"));
}
