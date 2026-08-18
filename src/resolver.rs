// NOTE: This file is meant to be removed once there is a proper handling
// for modules in the compiler
use inkwell::context::Context;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::codegen::{SafetyMode, compiler::Compiler};
use crate::errors::{Sources, ZeruError, report_errors};
use crate::sema::analyzer::SemanticAnalyzer;
use crate::{CompileError, ast::Program};
use crate::{lexer::Lexer, modules, parser::Parser, token::Token};

pub const RED: &str = "\x1b[1;38;2;224;108;117m";
pub const GREEN: &str = "\x1b[1;38;2;152;195;121m";
pub const RESET: &str = "\x1b[0m";
const YELLOW: &str = "\x1b[1;38;2;229;192;123m";
const VERB_WIDTH: usize = 9;

pub fn status(colour: &str, icon: char, verb: &str, detail: impl std::fmt::Display) {
    eprintln!("  {colour}{icon} {verb:>VERB_WIDTH$}{RESET} {detail}");
}

fn get_std_path() -> Result<PathBuf, CompileError> {
    if let Ok(path) = std::env::var("ZERU_STD_PATH") {
        return Ok(PathBuf::from(path));
    }
    let home = std::env::var("HOME").map_err(|_| CompileError::StdNotFound)?;
    Ok(PathBuf::from(home).join(".zeru/std"))
}

fn resolve_import(import_path: &str, root: &Path) -> Result<Option<PathBuf>, CompileError> {
    let mut parts = import_path.split('.').peekable();
    let base = match parts.peek() {
        Some(&"std") => {
            parts.next();
            get_std_path()?
        }
        Some(_) => root.to_path_buf(),
        None => return Ok(None),
    };

    let relative: Vec<&str> = parts.collect();
    if relative.is_empty() {
        return Ok(None);
    }

    let full_path = base.join(format!("{}.zr", relative.join("/")));
    Ok(full_path.exists().then_some(full_path))
}

struct ImportInfo {
    path: String,
    symbols: Option<Vec<String>>,
}

/// Extracts imports from the source code, only needs to look
/// at the beginning as zeru imposes import in the beginning
fn extract_imports(source: &str) -> Vec<ImportInfo> {
    let mut lexer = Lexer::new(source);
    let mut imports = Vec::new();
    let mut current = lexer.next_token().0;

    while current == Token::Import {
        let mut path = Vec::new();
        current = lexer.next_token().0;

        while let Token::Identifier(name) = current {
            path.push(name);
            current = lexer.next_token().0;
            if current == Token::Dot {
                current = lexer.next_token().0;
            } else {
                break;
            }
        }

        if path.is_empty() {
            break;
        }

        let mut symbols = None;
        if current == Token::DoubleColon {
            if lexer.next_token().0 == Token::LBrace {
                let mut listed = Vec::new();
                loop {
                    match lexer.next_token().0 {
                        Token::Identifier(sym) => listed.push(sym),
                        Token::RBrace | Token::Eof => break,
                        _ => {}
                    }
                }
                symbols = Some(listed);
            }
            current = lexer.next_token().0;
        }

        if current == Token::Semicolon {
            current = lexer.next_token().0;
        }

        imports.push(ImportInfo {
            path: path.join("."),
            symbols,
        });
    }

    imports
}

fn load_modules(
    imports: &[ImportInfo],
    root: &Path,
    loaded: &mut HashSet<String>,
    aliases: &mut HashMap<String, String>,
    sources: &mut Sources,
    program: &mut Program,
    errors: &mut Vec<ZeruError>,
) -> Result<(), CompileError> {
    for import in imports {
        let short_name = import.path.split('.').next_back().unwrap();
        if let Some(listed) = &import.symbols {
            aliases.extend(
                listed
                    .iter()
                    .map(|sym| (sym.clone(), format!("{short_name}::{sym}"))),
            );
        }

        if loaded.insert(import.path.clone()) {
            let Some(file_path) = resolve_import(&import.path, root)? else {
                return Err(CompileError::ModuleNotFound(import.path.clone()));
            };
            let source = std::fs::read_to_string(&file_path)
                .map_err(|_| CompileError::ModuleNotFound(import.path.clone()))?;

            let mut inner_aliases = HashMap::new();
            let inner = extract_imports(&source);
            load_modules(
                &inner,
                root,
                loaded,
                &mut inner_aliases,
                sources,
                program,
                errors,
            )?;

            let mut module = parse_file(file_path.display().to_string(), &source, sources, errors);
            modules::qualify(&mut module, Some(short_name), &inner_aliases);
            program.statements.append(&mut module.statements);
        }
    }

    Ok(())
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

pub fn compile_pipeline(
    path: &Path,
    safety_mode: SafetyMode,
    force_emit_ir: bool,
) -> Result<PathBuf, CompileError> {
    if !path.extension().is_some_and(|ext| ext == "zr") {
        status(YELLOW, '\u{ea6c}', "Warning", "Zeru extension is .zr");
    }

    let filename = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or(CompileError::InvalidPath)?;
    let build_dir = Path::new("build");

    std::fs::create_dir_all(build_dir)?;
    let ir_path = build_dir.join(format!("{filename}.ll"));
    let exe_path = build_dir.join(filename);

    status(
        GREEN,
        '\u{eb6d}',
        "Compiling",
        format_args!("v{} ({})", env!("CARGO_PKG_VERSION"), path.display()),
    );
    let start = std::time::Instant::now();

    let user_code = std::fs::read_to_string(path)?;
    let mut sources = Sources::default();
    let mut errors = Vec::new();
    let mut program = Program::default();

    let mut prelude = parse_file(
        String::from("std/builtin.zr"),
        include_str!("../std/builtin.zr"),
        &mut sources,
        &mut errors,
    );
    program.statements.append(&mut prelude.statements);

    let mut loaded = HashSet::from(["std.builtin".to_string()]);
    let mut aliases: HashMap<String, String> = HashMap::new();

    let root = path.parent().unwrap_or(Path::new("."));
    load_modules(
        &extract_imports(&user_code),
        root,
        &mut loaded,
        &mut aliases,
        &mut sources,
        &mut program,
        &mut errors,
    )?;

    let mut main = parse_file(
        path.display().to_string(),
        &user_code,
        &mut sources,
        &mut errors,
    );
    modules::qualify(&mut main, None, &aliases);
    program.statements.append(&mut main.statements);

    let check_errors = |errs: &[ZeruError]| -> Result<(), CompileError> {
        if !errs.is_empty() {
            report_errors(errs, &sources);
            return Err(CompileError::Unknown);
        }
        Ok(())
    };

    check_errors(&errors)?;

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&mut program);
    check_errors(&analyzer.errors)?;

    let context = Context::create();
    let module = context.create_module(filename);
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode);
    compiler.compile_program(&program);
    check_errors(&compiler.errors)?;

    module.verify()?;
    module.print_to_file(&ir_path)?;

    let link = Command::new("clang")
        .arg(&ir_path)
        .arg("-o")
        .arg(&exe_path)
        .args(safety_mode.clang_flags())
        .arg("-Wno-override-module")
        .status()?;

    if !link.success() {
        return Err(CompileError::Link(link));
    }

    let end = start.elapsed().as_secs_f64();
    status(
        GREEN,
        '\u{ef0a}',
        "Finished",
        format_args!("{safety_mode} in {end:.3}s"),
    );

    if !force_emit_ir {
        std::fs::remove_file(ir_path)?;
    } else {
        status(GREEN, '\u{eaf3}', "IR saved", ir_path.display());
    }

    Ok(exe_path)
}
