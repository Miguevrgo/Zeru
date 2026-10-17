//! The pipeline: load and parse every file, collect the items, type check
//! into the typed IR, lower that to LLVM, and link with clang.

use inkwell::context::Context;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::ast::{Program, StatementKind};
use crate::check::Checker;
use crate::errors::{CompileError, Sources, Span, ZeruError, report_errors};
use crate::items::{Items, SourceModule};
use crate::lower::SafetyMode;
use crate::{lexer::Lexer, parser::Parser};

pub const RED: &str = "\x1b[1;38;2;224;108;117m";
pub const GREEN: &str = "\x1b[1;38;2;152;195;121m";
pub const RESET: &str = "\x1b[0m";
const YELLOW: &str = "\x1b[1;38;2;229;192;123m";
const VERB_WIDTH: usize = 9;

pub fn status(colour: &str, icon: char, verb: &str, detail: impl std::fmt::Display) {
    eprintln!("  {colour}{icon} {verb:>VERB_WIDTH$}{RESET} {detail}");
}

fn std_path() -> Result<PathBuf, CompileError> {
    if let Ok(path) = std::env::var("ZERU_STD_PATH") {
        return Ok(PathBuf::from(path));
    }
    let home = std::env::var("HOME").map_err(|_| CompileError::StdNotFound)?;
    Ok(PathBuf::from(home).join(".zeru/std"))
}

fn resolve_import(import: &str, root: &Path) -> Result<PathBuf, CompileError> {
    let (base, relative) = match import.strip_prefix("std.") {
        Some(rest) => (std_path()?, rest),
        None => (root.to_path_buf(), import),
    };
    Ok(base.join(relative.replace('.', "/")).with_extension("zr"))
}

fn parse_file(
    name: String,
    source: &str,
    sources: &mut Sources,
    errors: &mut Vec<ZeruError>,
) -> Program {
    let start = sources.push(name, source);
    let mut parser = Parser::new(Lexer::at(source, start));
    let program = parser.parse_program();
    errors.append(&mut parser.errors);
    program
}

pub fn compile(path: &Path, mode: SafetyMode, keep_ir: bool) -> Result<PathBuf, CompileError> {
    if !path.extension().is_some_and(|ext| ext == "zr") {
        status(YELLOW, '\u{ea6c}', "Warning", "Zeru extension is .zr");
    }
    let name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or(CompileError::InvalidPath)?;
    let build_dir = Path::new("build");
    std::fs::create_dir_all(build_dir)?;
    let ir_path = build_dir.join(format!("{name}.ll"));
    let exe_path = build_dir.join(name);

    status(
        GREEN,
        '\u{eb6d}',
        "Compiling",
        format_args!("v{} ({})", env!("CARGO_PKG_VERSION"), path.display()),
    );
    let start = std::time::Instant::now();
    let user_code = std::fs::read_to_string(path)?;

    let (sources, modules, errors) = load(path, &user_code)?;
    let report = |errors: &[ZeruError]| {
        report_errors(errors, &sources);
        Err(CompileError::Unknown)
    };
    if !errors.is_empty() {
        return report(&errors);
    }
    let items = Items::collect(&modules);
    let program = match Checker::new(&items).check() {
        Err(errors) => return report(&errors),
        Ok(program) => program,
    };
    let context = Context::create();
    let module = context.create_module(name);
    crate::lower::lower(&program, &context, &module, &sources, mode);
    module.verify()?;
    module.print_to_file(&ir_path)?;

    let link = Command::new("clang")
        .arg(&ir_path)
        .arg("-o")
        .arg(&exe_path)
        .args(mode.clang_flags())
        .args(["-Wno-override-module", "-lm"])
        .status()?;
    if !link.success() {
        return Err(CompileError::Link(link));
    }

    let end = start.elapsed().as_secs_f64();
    status(
        GREEN,
        '\u{ef0a}',
        "Finished",
        format_args!("{mode} in {end:.3}s"),
    );
    if keep_ir {
        status(GREEN, '\u{eaf3}', "IR saved", ir_path.display());
    } else {
        std::fs::remove_file(ir_path)?;
    }
    Ok(exe_path)
}

/// Parse the prelude, the root file and every module they import, each into
/// a module of its own.
fn load(
    path: &Path,
    user_code: &str,
) -> Result<(Sources, Vec<SourceModule>, Vec<ZeruError>), CompileError> {
    let mut sources = Sources::default();
    let mut errors = Vec::new();
    let prelude = parse_file(
        String::from("std/builtin.zr"),
        include_str!("../std/builtin.zr"),
        &mut sources,
        &mut errors,
    );
    let root = parse_file(
        path.display().to_string(),
        user_code,
        &mut sources,
        &mut errors,
    );
    let root_end = sources.end();
    if !root
        .statements
        .iter()
        .any(|stmt| matches!(&stmt.kind, StatementKind::Function { name, .. } if name == "main"))
    {
        errors.push(ZeruError::syntax(
            "Missing 'fn main'",
            Span::new(root_end - 1, root_end),
        ));
    }
    let mut modules = vec![
        SourceModule {
            name: None,
            program: prelude,
            std: true,
        },
        SourceModule {
            name: None,
            program: root,
            std: false,
        },
    ];
    let dir = path.parent().unwrap_or(Path::new("."));
    let mut loaded: HashMap<String, String> = HashMap::new();
    let mut next = 1;
    while next < modules.len() {
        for (import, span) in imports(&modules[next].program) {
            let short = import.rsplit('.').next().unwrap_or_default().to_string();
            match loaded.get(&short) {
                Some(full) if *full == import => continue,
                Some(full) => {
                    errors.push(ZeruError::semantic(
                        format!("'{import}' and '{full}' are both called '{short}'; a program can import only one of them"),
                        span,
                    ));
                    continue;
                }
                None => {}
            }
            let file = resolve_import(&import, dir)?;
            let source = std::fs::read_to_string(&file)
                .map_err(|_| CompileError::ModuleNotFound(import.clone(), file.clone()))?;
            let program = parse_file(
                file.display().to_string(),
                &source,
                &mut sources,
                &mut errors,
            );
            modules.push(SourceModule {
                name: Some(short.clone()),
                program,
                std: import.starts_with("std."),
            });
            loaded.insert(short, import);
        }
        next += 1;
    }
    Ok((sources, modules, errors))
}

fn imports(program: &Program) -> Vec<(String, Span)> {
    program
        .statements
        .iter()
        .filter_map(|statement| match &statement.kind {
            StatementKind::Import { path, .. } => Some((path.join("."), statement.span)),
            _ => None,
        })
        .collect()
}
