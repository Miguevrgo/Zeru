mod ast;
mod check;
mod driver;
mod errors;
mod fmt;
mod items;
mod lexer;
mod lower;
mod mir;
mod parser;
mod tir;
mod token;
mod ty;

use crate::driver::{GREEN, RED, check, compile, format, status, test};
use crate::{errors::CompileError, lower::SafetyMode};
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
        #[arg(last = true, help = "Arguments for the program, after --")]
        program_args: Vec<String>,
    },
    #[command(about = "Run the tests in #test of a .zr file and the files it imports")]
    Test {
        file: PathBuf,
        #[arg(help = "Run only the tests whose name holds this")]
        filter: Option<String>,
        #[arg(
            long,
            conflicts_with = "release_fast",
            help = "Optimize, keep runtime checks"
        )]
        release_safe: bool,
        #[arg(long, help = "Optimize, drop runtime checks")]
        release_fast: bool,
    },
    #[command(about = "Check a .zr file for errors without compiling it")]
    Check {
        file: PathBuf,
        #[arg(long, help = "One JSON object per error, for editors")]
        json: bool,
    },
    #[command(about = "Format .zr files in place; - formats stdin to stdout")]
    Fmt {
        #[arg(required = true)]
        files: Vec<PathBuf>,
        #[arg(long, help = "List the files not formatted, changing nothing")]
        check: bool,
    },
    #[command(about = "Remove the build/ directory")]
    Clean,
    #[command(about = "Install the build of main's latest commit")]
    Update {
        #[arg(long, help = "Install the latest release instead")]
        stable: bool,
    },
}

const INSTALLER: &str = "https://raw.githubusercontent.com/Miguevrgo/Zeru/main/install.sh";

fn run_compiler(args: Cli) -> Result<(), CompileError> {
    match args.command {
        Commands::Build {
            file,
            release_safe,
            release_fast,
            emit_ir,
        } => {
            let safety_mode = SafetyMode::from_flags(release_fast, release_safe);
            compile(&file, safety_mode, emit_ir, false)?;
        }
        Commands::Run {
            file,
            release_safe,
            release_fast,
            program_args,
        } => {
            let safety_mode = SafetyMode::from_flags(release_fast, release_safe);
            let (executable_path, _) = compile(&file, safety_mode, false, false)?;
            status(GREEN, '\u{e7d5}', "Running", executable_path.display());
            let status = std::process::Command::new(&executable_path)
                .args(program_args)
                .status()?;
            std::process::exit(
                status
                    .code()
                    .unwrap_or_else(|| status.signal().map(|s| 128 + s).unwrap_or(1)),
            );
        }
        Commands::Test {
            file,
            filter,
            release_safe,
            release_fast,
        } => {
            let safety_mode = SafetyMode::from_flags(release_fast, release_safe);
            if !test(&file, safety_mode, filter.as_deref())? {
                std::process::exit(1);
            }
        }
        Commands::Check { file, json } => check(&file, json)?,
        Commands::Fmt { files, check } => format(&files, check)?,
        Commands::Clean => {
            let build_dir = Path::new("build");
            if build_dir.exists() {
                std::fs::remove_dir_all(build_dir)?;
                status(GREEN, '\u{ea81}', "Cleaned", build_dir.display());
            }
        }
        Commands::Update { stable } => {
            let channel = if stable { "" } else { " -s -- --nightly" };
            let installed = std::process::Command::new("bash")
                .arg("-c")
                .arg(format!(
                    "set -o pipefail; curl -fsSL {INSTALLER} | bash{channel}"
                ))
                .status()?;
            if !installed.success() {
                std::process::exit(1);
            }
        }
    }
    Ok(())
}

/// The passes recurse along the syntax tree, so they run on a thread whose
/// stack fits a deep one: memory is only taken as it is used.
fn main() {
    let compiler = std::thread::Builder::new().stack_size(1 << 30).spawn(|| {
        if let Err(err) = run_compiler(Cli::parse()) {
            let (red, reset) = crate::errors::paint(RED);
            eprintln!("{red}[-] Error: {err}{reset}");
            std::process::exit(1);
        }
    });
    if !compiler.is_ok_and(|thread| thread.join().is_ok()) {
        eprintln!(
            "internal compiler error: a bug in Zeru stopped it; please report it with the file it compiled"
        );
        std::process::exit(101);
    }
}
