//! The pipeline: load and parse every file, collect the items, type check
//! into the typed IR, build the MIR, lower that to LLVM, emit an object file
//! and link it statically, with no libc, through the system C compiler.

use inkwell::context::{AsContextRef, Context};
use inkwell::llvm_sys::LLVMDiagnosticSeverity;
use inkwell::llvm_sys::core::{
    LLVMContextSetDiagnosticHandler, LLVMDisposeMessage, LLVMGetDiagInfoDescription,
    LLVMGetDiagInfoSeverity,
};
use inkwell::llvm_sys::prelude::LLVMDiagnosticInfoRef;
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::FileType;
use std::collections::HashMap;
use std::ffi::{CStr, c_void};
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
const YELLOW: &str = "\x1b[1;38;2;229;192;123m";
const VERB_WIDTH: usize = 9;

pub fn status(colour: &str, icon: char, verb: &str, detail: impl std::fmt::Display) {
    let (colour, reset) = crate::errors::paint(colour);
    eprintln!("  {colour}{icon} {verb:>VERB_WIDTH$}{reset} {detail}");
}

/// The standard library: `ZERU_STD_PATH`, or else the one beside the
/// compiler, so a compiler always reads the std it was released with.
fn std_path() -> Option<PathBuf> {
    if let Some(path) = std::env::var_os("ZERU_STD_PATH") {
        return Some(path.into());
    }
    std_beside(&std::env::current_exe().ok()?)
}

/// The `std` directory in the closest directory above `exe` that has one:
/// `~/.zeru/std` for `~/.zeru/bin/zeru`, and the repository's for
/// `target/release/zeru`.
fn std_beside(exe: &Path) -> Option<PathBuf> {
    exe.ancestors()
        .map(|dir| dir.join("std"))
        .find(|std| std.join("builtin.zr").is_file())
}

fn resolve_import(import: &str, root: &Path) -> Option<PathBuf> {
    let (base, relative) = match import.strip_prefix("std.") {
        Some(rest) => (std_path()?, rest),
        None => (root.to_path_buf(), import),
    };
    Some(base.join(relative.replace('.', "/")).with_extension("zr"))
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

/// What checking a program gives: its typed IR and MIR, or its errors.
type Analysis = Result<(crate::tir::Program, Vec<crate::mir::Body>), Vec<ZeruError>>;

/// Everything before code generation: load and parse every file, collect the
/// items, type check, and build the MIR and check its borrows.
fn analyze(path: &Path, mode: SafetyMode) -> Result<(Sources, Analysis), CompileError> {
    let user_code = std::fs::read_to_string(path)?;
    let (sources, modules, errors) = load(path, &user_code);
    if !errors.is_empty() {
        return Ok((sources, Err(errors)));
    }
    let items = Items::collect(&modules);
    let analysis = Checker::new(&items).check().and_then(|mut program| {
        let bodies = crate::mir::build(&mut program, mode)?;
        Ok((program, bodies))
    });
    Ok((sources, analysis))
}

/// Check `path` as `compile` would, without generating code, and report its
/// errors as text or, for editors, as one JSON object per line.
pub fn check(path: &Path, json: bool) -> Result<(), CompileError> {
    let (sources, analysis) = analyze(path, SafetyMode::Debug)?;
    let Err(errors) = analysis else {
        return Ok(());
    };
    match json {
        true => errors
            .iter()
            .for_each(|error| println!("{}", error.json(&sources))),
        false => report_errors(&errors, &sources),
    }
    Err(CompileError::Unknown)
}

/// Format each of `files` in place, or with `check` list those that are not
/// formatted, changing nothing. `-` formats stdin to stdout.
pub fn format(files: &[PathBuf], check: bool) -> Result<(), CompileError> {
    let mut failed = false;
    for path in files {
        let stdin = path.as_os_str() == "-";
        let source = match stdin {
            true => std::io::read_to_string(std::io::stdin())?,
            false => std::fs::read_to_string(path)?,
        };
        match crate::fmt::format(&source) {
            Ok(formatted) if stdin => print!("{formatted}"),
            Ok(formatted) if formatted == source => {}
            Ok(_) if check => {
                status(RED, '\u{ea6c}', "Unformatted", path.display());
                failed = true;
            }
            Ok(formatted) => {
                std::fs::write(path, formatted)?;
                status(GREEN, '\u{eb6d}', "Formatted", path.display());
            }
            Err(errors) => {
                let mut sources = Sources::default();
                sources.push(path.display().to_string(), &source);
                report_errors(&errors, &sources);
                failed = true;
            }
        }
    }
    match failed {
        true => Err(CompileError::Unknown),
        false => Ok(()),
    }
}

/// Keep each error LLVM reports, as an asm instruction it does not know, in
/// the `Vec<String>` at `errors`: without this LLVM prints it and goes on.
extern "C" fn collect_error(info: LLVMDiagnosticInfoRef, errors: *mut c_void) {
    unsafe {
        if LLVMGetDiagInfoSeverity(info) != LLVMDiagnosticSeverity::LLVMDSError {
            return;
        }
        let text = LLVMGetDiagInfoDescription(info);
        let message = CStr::from_ptr(text).to_string_lossy().into_owned();
        LLVMDisposeMessage(text);
        (*errors.cast::<Vec<String>>()).push(message);
    }
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
    let object_path = build_dir.join(format!("{name}.o"));
    let exe_path = build_dir.join(name);

    status(
        GREEN,
        '\u{eb6d}',
        "Compiling",
        format_args!("v{} ({})", env!("CARGO_PKG_VERSION"), path.display()),
    );
    let start = std::time::Instant::now();
    let (sources, analysis) = analyze(path, mode)?;
    let (program, bodies) = analysis.map_err(|errors| {
        report_errors(&errors, &sources);
        CompileError::Unknown
    })?;
    let mut rejected: Vec<String> = Vec::new();
    let context = Context::create();
    unsafe {
        LLVMContextSetDiagnosticHandler(
            (&context).as_ctx_ref(),
            Some(collect_error),
            (&raw mut rejected).cast(),
        );
    }
    let module = context.create_module(name);
    let machine = crate::lower::host_machine(mode);
    crate::lower::lower(
        &program, &bodies, &context, &module, &sources, mode, &machine,
    );
    module.verify()?;
    if keep_ir {
        module.print_to_file(&ir_path)?;
    }

    let options = PassBuilderOptions::create();
    options.set_loop_slp_vectorization(mode != SafetyMode::Debug);
    module.run_passes(mode.passes(), &machine, options)?;
    machine.write_to_file(&module, FileType::Object, &object_path)?;
    rejected.dedup();
    if !rejected.is_empty() {
        std::fs::remove_file(object_path)?;
        return Err(CompileError::Rejected(rejected.join("\n")));
    }
    let link = Command::new("cc")
        .arg(&object_path)
        .arg("-o")
        .arg(&exe_path)
        .args(["-nostdlib", "-static", "-no-pie"])
        .status()?;
    std::fs::remove_file(object_path)?;
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
    }
    Ok(exe_path)
}

/// Parse the prelude, the root file and every module they import, each into
/// a module of its own.
fn load(path: &Path, user_code: &str) -> (Sources, Vec<SourceModule>, Vec<ZeruError>) {
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
            let Some(file) = resolve_import(&import, dir) else {
                errors.push(ZeruError::semantic(
                    "The standard library is not beside the compiler; set ZERU_STD_PATH",
                    span,
                ));
                continue;
            };
            let Ok(source) = std::fs::read_to_string(&file) else {
                errors.push(ZeruError::semantic(
                    format!("Module '{import}' not found at {}", file.display()),
                    span,
                ));
                continue;
            };
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
    (sources, modules, errors)
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

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    #[test]
    fn std_is_found_above_the_compiler() {
        let exe = std::env::current_exe().unwrap();
        let repo = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        assert_eq!(super::std_beside(&exe), Some(repo.join("std")));
    }
}
