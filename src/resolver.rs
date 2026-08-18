// NOTE: This file is meant to be removed once there is a proper handling
// for modules in the compiler
use inkwell::context::Context;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::CompileError;
use crate::ast::Program;
use crate::codegen::SafetyMode;
use crate::codegen::compiler::Compiler;
use crate::errors::{Sources, Span, ZeruError, report_errors};
use crate::lexer::Lexer;
use crate::modules;
use crate::parser::Parser;
use crate::sema::analyzer::SemanticAnalyzer;
use crate::token::Token;

pub const RED_FG: &str = "\x1b[1;38;2;224;108;117m";
pub const GREEN_FG: &str = "\x1b[1;38;2;152;195;121m";
pub const YELLOW_FG: &str = "\x1b[1;38;2;229;192;123m";
pub const RESET: &str = "\x1b[0m";

const VERB_WIDTH: usize = 9;
pub fn status(colour: &str, icon: char, verb: &str, detail: impl std::fmt::Display) {
    eprintln!("  {colour}{icon} {verb:>VERB_WIDTH$}{RESET} {detail}");
}

fn get_std_path() -> Result<PathBuf, CompileError> {
    if let Ok(path) = std::env::var("ZERU_STD_PATH") {
        return Ok(PathBuf::from(path));
    }
    let home = std::env::var("HOME").map_err(|_| CompileError::StdNotFound)?;
    Ok(PathBuf::from(home).join(".zeru").join("std"))
}

/// File a dotted import path names. `std.*` resolves under the installed
/// library, everything else under `root`, so a path means the same thing
/// written from any module.
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

fn load_builtin_std() -> String {
    include_str!("../std/builtin.zr").to_string()
}

struct ImportInfo {
    path: String,
    symbols: Option<Vec<String>>,
}

/// Every token with its span, so the rewrites below splice at exact offsets
/// instead of rescanning characters. String literals and comments are then out
/// of reach: they never arrive as identifiers.
fn tokenize(source: &str) -> Vec<(Token, Span)> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();

    loop {
        let (token, _, span) = lexer.next_token();
        if token == Token::Eof {
            return tokens;
        }
        tokens.push((token, span));
    }
}

/// `import a.b;` imports the module, `import a.b::{x, y};` its listed symbols.
fn extract_imports(source: &str) -> Vec<ImportInfo> {
    let tokens = tokenize(source);
    let mut imports = Vec::new();
    let mut i = 0;

    while i < tokens.len() {
        if tokens[i].0 != Token::Import {
            i += 1;
            continue;
        }
        i += 1;

        // The dotted path: `std`, `.`, `math`, ...
        let mut path = Vec::new();
        while let Some((Token::Identifier(name), _)) = tokens.get(i) {
            path.push(name.clone());
            i += 1;
            if tokens.get(i).map(|(t, _)| t) != Some(&Token::Dot) {
                break;
            }
            i += 1;
        }
        if path.is_empty() {
            continue;
        }

        // An optional `::{a, b}` selection.
        let mut symbols = None;
        if tokens.get(i).map(|(t, _)| t) == Some(&Token::DoubleColon)
            && tokens.get(i + 1).map(|(t, _)| t) == Some(&Token::LBrace)
        {
            i += 2;
            let mut listed = Vec::new();
            while let Some((token, _)) = tokens.get(i) {
                i += 1;
                match token {
                    Token::Identifier(name) => listed.push(name.clone()),
                    Token::RBrace => break,
                    _ => {}
                }
            }
            symbols = Some(listed);
        }

        imports.push(ImportInfo {
            path: path.join("."),
            symbols,
        });
    }

    imports
}

fn get_module_short_name(import_path: &str) -> String {
    import_path.split('.').next_back().unwrap_or("").to_string()
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
        let short_name = get_module_short_name(&import.path);

        // Record how this importer names the module, loaded or not. A plain
        // import is written `module::name`, which is already how the parser
        // reads a qualified path, so only a selective import needs an alias.
        if let Some(listed) = &import.symbols {
            aliases.extend(
                listed
                    .iter()
                    .map(|sym| (sym.clone(), format!("{short_name}::{sym}"))),
            );
        }

        if !loaded.insert(import.path.clone()) {
            continue;
        }

        let Some(file_path) = resolve_import(&import.path, root)? else {
            return Err(CompileError::ModuleNotFound(import.path.clone()));
        };
        let source = fs::read_to_string(&file_path)
            .map_err(|_| CompileError::ModuleNotFound(import.path.clone()))?;

        // Depth first, in a scope of its own: a module body may only name what
        // it imports itself, not what a sibling happened to import.
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
        modules::qualify(&mut module, Some(&short_name), &inner_aliases);
        program.statements.append(&mut module.statements);
    }

    Ok(())
}

/// Parse one file into its own tree, recording it in `sources` so its spans
/// stay locatable once every file has been merged.
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
    if path.extension().and_then(|s| s.to_str()) != Some("zr") {
        status(
            YELLOW_FG,
            '\u{ea6c}',
            "Warning",
            "Zeru sources use the .zr extension",
        );
    }

    let filename = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or(CompileError::InvalidPath)?;
    let build_dir = Path::new("build");

    fs::create_dir_all(build_dir)?;

    let ir_path = build_dir.join(format!("{filename}.ll"));
    let exe_path = build_dir.join(filename);

    status(
        GREEN_FG,
        '\u{eb6d}',
        "Compiling",
        format_args!("v{} ({})", env!("CARGO_PKG_VERSION"), path.display()),
    );
    let start = std::time::Instant::now();

    let user_code = fs::read_to_string(path)?;

    let mut sources = Sources::default();
    let mut errors = Vec::new();
    let mut program = Program {
        statements: Vec::new(),
    };

    let builtin = load_builtin_std();
    let mut prelude = parse_file(
        "std/builtin.zr".to_string(),
        &builtin,
        &mut sources,
        &mut errors,
    );
    program.statements.append(&mut prelude.statements);

    let mut loaded = HashSet::from(["std.builtin".to_string()]);
    let mut aliases: HashMap<String, String> = HashMap::new();

    // Project imports resolve from the directory of the file being built, so a
    // path means the same thing written from any module.
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

    if !errors.is_empty() {
        report_errors(&errors, &sources);
        return Err(CompileError::Unknown);
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&mut program);

    if !analyzer.errors.is_empty() {
        report_errors(&analyzer.errors, &sources);
        return Err(CompileError::Unknown);
    }

    let context = Context::create();
    let module = context.create_module(filename);
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode.clone());
    compiler.compile_program(&program);

    if !compiler.errors.is_empty() {
        report_errors(&compiler.errors, &sources);
        return Err(CompileError::Unknown);
    }

    module.verify()?;
    module.print_to_file(&ir_path)?;

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

    let link = cmd.status()?;
    if !link.success() {
        return Err(CompileError::Link(link));
    }

    let end = start.elapsed().as_millis() as f64 / 1000.0;
    status(
        GREEN_FG,
        '\u{ef0a}',
        "Finished",
        format_args!("{safety_mode} in {end:.3}s"),
    );

    if !force_emit_ir {
        fs::remove_file(ir_path)?;
    } else {
        status(GREEN_FG, '\u{eaf3}', "IR saved", ir_path.display());
    }

    Ok(exe_path)
}
