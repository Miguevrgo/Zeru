// NOTE: This file is meant to be removed once there is a proper handling
// for modules in the compiler
use inkwell::context::Context;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::CompileError;
use crate::codegen::SafetyMode;
use crate::codegen::compiler::Compiler;
use crate::errors::report_errors;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::sema::analyzer::SemanticAnalyzer;
use crate::token::Token;

pub const RED_FG: &str = "\x1b[1;38;2;224;108;117m";
pub const GREEN_FG: &str = "\x1b[1;38;2;152;195;121m";
pub const YELLOW_FG: &str = "\x1b[1;38;2;229;192;123m";
pub const RESET: &str = "\x1b[0m";

fn get_std_path() -> Result<PathBuf, CompileError> {
    if let Ok(path) = std::env::var("ZERU_STD_PATH") {
        return Ok(PathBuf::from(path));
    }
    let home = std::env::var("HOME").map_err(|_| CompileError::StdNotFound)?;
    Ok(PathBuf::from(home).join(".zeru").join("std"))
}

fn resolve_std_import(import_path: &str) -> Result<Option<PathBuf>, CompileError> {
    let parts: Vec<&str> = import_path.split('.').collect();
    if parts.is_empty() || parts[0] != "std" || parts.len() == 1 {
        return Ok(None);
    }

    let module_file = format!("{}.zr", parts[1..].join("/"));
    let full_path = get_std_path()?.join(&module_file);

    if full_path.exists() {
        Ok(Some(full_path))
    } else {
        Ok(None)
    }
}

fn load_builtin_std() -> String {
    include_str!("../std/builtin.zr").to_string()
}

struct ImportInfo {
    path: String,
    symbols: Option<Vec<String>>,
}

fn extract_imports(code: &str) -> Vec<ImportInfo> {
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
                    continue;
                }

                loop {
                    let (next, _, _) = lexer.next_token();
                    if next == Token::Dot {
                        if let (Token::Identifier(name), _, _) = lexer.next_token() {
                            path_parts.push(name);
                        } else {
                            break;
                        }
                    } else if next == Token::DoubleColon {
                        let (brace, _, _) = lexer.next_token();
                        if brace != Token::LBrace {
                            break;
                        }
                        let mut symbols = Vec::new();
                        loop {
                            let (sym_tok, _, _) = lexer.next_token();
                            if let Token::Identifier(sym) = sym_tok {
                                symbols.push(sym);
                            } else if sym_tok == Token::RBrace {
                                break;
                            }
                            let (comma_or_brace, _, _) = lexer.next_token();
                            if comma_or_brace == Token::RBrace {
                                break;
                            }
                        }
                        if !path_parts.is_empty() {
                            imports.push(ImportInfo {
                                path: path_parts.join("."),
                                symbols: Some(symbols),
                            });
                        }
                        break;
                    } else {
                        if !path_parts.is_empty() {
                            imports.push(ImportInfo {
                                path: path_parts.join("."),
                                symbols: None,
                            });
                        }
                        break;
                    }
                }
            }
            Token::Eof => break,
            _ => continue,
        }
    }

    imports
}

fn get_module_short_name(import_path: &str) -> String {
    import_path.split('.').next_back().unwrap_or("").to_string()
}

fn prefix_definitions(content: &str, prefix: &str) -> String {
    // Pass 1: collect top-level definition names (skip methods inside structs)
    let mut def_names: HashSet<String> = HashSet::new();
    {
        let mut chars = content.chars().peekable();
        let mut brace_depth: i32 = 0;
        while let Some(c) = chars.next() {
            if c == '{' {
                brace_depth += 1;
                continue;
            }
            if c == '}' {
                brace_depth -= 1;
                continue;
            }
            // Skip string literals
            if c == '"' {
                while let Some(sc) = chars.next() {
                    if sc == '\\' {
                        chars.next();
                    } else if sc == '"' {
                        break;
                    }
                }
                continue;
            }
            // Skip single-line comments
            if c == '/' && chars.peek() == Some(&'/') {
                for cc in chars.by_ref() {
                    if cc == '\n' {
                        break;
                    }
                }
                continue;
            }
            if c.is_alphabetic() || c == '_' {
                let mut word = String::from(c);
                while chars
                    .peek()
                    .is_some_and(|&nc| nc.is_alphanumeric() || nc == '_')
                {
                    word.push(chars.next().unwrap());
                }
                if matches!(word.as_str(), "fn" | "struct" | "const") && brace_depth == 0 {
                    while chars.peek().is_some_and(|&ws| ws.is_whitespace()) {
                        chars.next();
                    }
                    let mut name = String::new();
                    while chars
                        .peek()
                        .is_some_and(|&nc| nc.is_alphanumeric() || nc == '_')
                    {
                        name.push(chars.next().unwrap());
                    }
                    if !name.is_empty() {
                        def_names.insert(name);
                    }
                }
            }
        }
    }

    // Pass 2: prefix every identifier that matches a top-level definition name
    let mut result = String::with_capacity(content.len() + def_names.len() * (prefix.len() + 2));
    let mut chars = content.chars().peekable();

    while let Some(c) = chars.next() {
        // Copy string literals verbatim
        if c == '"' {
            result.push(c);
            while let Some(sc) = chars.next() {
                result.push(sc);
                if sc == '\\' {
                    if let Some(esc) = chars.next() {
                        result.push(esc);
                    }
                } else if sc == '"' {
                    break;
                }
            }
            continue;
        }
        if c.is_alphabetic() || c == '_' {
            let mut word = String::from(c);
            while chars
                .peek()
                .is_some_and(|&nc| nc.is_alphanumeric() || nc == '_')
            {
                word.push(chars.next().unwrap());
            }
            if def_names.contains(&word) {
                result.push_str(prefix);
                result.push_str("__");
            }
            result.push_str(&word);
        } else {
            result.push(c);
        }
    }
    result
}

fn load_std_modules(
    imports: &[ImportInfo],
    loaded: &mut HashSet<String>,
    direct_symbols: &mut HashMap<String, String>,
    module_prefixes: &mut HashSet<String>,
) -> Result<String, CompileError> {
    let mut code = String::new();

    for import in imports {
        if loaded.contains(&import.path) {
            if let Some(ref symbols) = import.symbols {
                let short_name = get_module_short_name(&import.path);
                for sym in symbols {
                    direct_symbols.insert(sym.clone(), format!("{}__{}", short_name, sym));
                }
            } else {
                let short_name = get_module_short_name(&import.path);
                module_prefixes.insert(short_name);
            }
            continue;
        }

        if import.path.starts_with("std.") {
            if let Some(file_path) = resolve_std_import(&import.path)? {
                if file_path.exists()
                    && let Ok(content) = fs::read_to_string(&file_path)
                {
                    loaded.insert(import.path.clone());
                    let short_name = get_module_short_name(&import.path);

                    let nested_imports = extract_imports(&content);
                    let nested_code =
                        load_std_modules(&nested_imports, loaded, direct_symbols, module_prefixes)?;
                    code.push_str(&nested_code);

                    let prefixed = prefix_definitions(&content, &short_name);
                    code.push_str(&prefixed);
                    code.push('\n');

                    if let Some(ref symbols) = import.symbols {
                        for sym in symbols {
                            direct_symbols.insert(sym.clone(), format!("{}__{}", short_name, sym));
                        }
                    } else {
                        module_prefixes.insert(short_name);
                    }
                } else {
                    return Err(CompileError::StdNotFound);
                }
            } else {
                return Err(CompileError::StdNotFound);
            }
        }
    }

    Ok(code)
}

/// Resolve symbols in code based on import style:
/// - For selective imports: symbol() -> prefixed__symbol()
/// - For module imports: module::symbol() -> prefixed__symbol()
fn resolve_direct_symbols(
    code: &str,
    direct_symbols: &HashMap<String, String>,
    module_prefixes: &HashSet<String>,
) -> String {
    if direct_symbols.is_empty() && module_prefixes.is_empty() {
        return code.to_string();
    }
    let mut result = String::with_capacity(code.len() + 50);
    let mut chars = code.chars().peekable();

    while let Some(c) = chars.next() {
        if c.is_alphabetic() || c == '_' {
            let mut ident = String::from(c);
            while let Some(&next) = chars.peek() {
                if next.is_alphanumeric() || next == '_' {
                    ident.push(chars.next().unwrap());
                } else {
                    break;
                }
            }

            let mut peek_chars = chars.clone();
            let mut is_module_call = false;

            while peek_chars.peek().is_some_and(|ws| ws.is_whitespace()) {
                peek_chars.next();
            }

            if peek_chars.next() == Some(':') && peek_chars.next() == Some(':') {
                while peek_chars.peek().is_some_and(|ws| ws.is_whitespace()) {
                    peek_chars.next();
                }

                let mut sym = String::new();
                while let Some(&nc) = peek_chars.peek() {
                    if nc.is_alphanumeric() || nc == '_' {
                        sym.push(peek_chars.next().unwrap());
                    } else {
                        break;
                    }
                }

                if module_prefixes.contains(&ident) && !sym.is_empty() {
                    result.push_str(&format!("{}__{}", ident, sym));
                    chars = peek_chars;
                    is_module_call = true;
                }
            }

            if !is_module_call {
                if let Some(qualified) = direct_symbols.get(&ident) {
                    result.push_str(qualified);
                } else {
                    result.push_str(&ident);
                }
            }
        } else {
            result.push(c);
        }
    }
    result
}

pub fn compile_pipeline(
    path: &Path,
    safety_mode: SafetyMode,
    force_emit_ir: bool,
) -> Result<PathBuf, CompileError> {
    if path.extension().and_then(|s| s.to_str()) != Some("zr") {
        eprintln!("{YELLOW_FG}⚠️Warning: Zeru extension is .zr{RESET}");
    }

    let filename = path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or(CompileError::InvalidPath)?;
    let build_dir = Path::new("build");

    fs::create_dir_all(build_dir)?;

    let ir_path = build_dir.join(format!("{filename}.ll"));
    let exe_path = build_dir.join(filename);

    eprintln!(
        "  {GREEN_FG} Compiling {RESET}v{} ({})",
        env!("CARGO_PKG_VERSION"),
        path.to_str().unwrap(),
    );
    let start = std::time::Instant::now();

    let user_code = fs::read_to_string(path)?;

    let std_builtin = load_builtin_std();
    let user_imports = extract_imports(&user_code);
    let mut loaded_modules = HashSet::new();
    loaded_modules.insert("std.builtin".to_string());
    let mut direct_symbols: HashMap<String, String> = HashMap::new();
    let mut module_prefixes: HashSet<String> = HashSet::new();

    let additional_std = load_std_modules(
        &user_imports,
        &mut loaded_modules,
        &mut direct_symbols,
        &mut module_prefixes,
    )?;

    let user_code_resolved = resolve_direct_symbols(&user_code, &direct_symbols, &module_prefixes);
    let offset = std_builtin.len() + 1 + additional_std.len() + 1;
    let input = format!("{std_builtin}\n{additional_std}\n{user_code_resolved}",);

    let lexer = Lexer::new(&input);
    let mut parser = Parser::new(lexer);
    let mut program = parser.parse_program();

    let filepath_str = path.to_str().unwrap();

    if !parser.errors.is_empty() {
        report_errors(&parser.errors, filepath_str, &input, offset);
        return Err(CompileError::Unknown);
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&mut program);

    if !analyzer.errors.is_empty() {
        report_errors(&analyzer.errors, filepath_str, &input, offset);
        return Err(CompileError::Unknown);
    }

    let context = Context::create();
    let module = context.create_module(filename);
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode.clone());
    compiler.compile_program(&program);

    if !compiler.errors.is_empty() {
        report_errors(&compiler.errors, filepath_str, &input, offset);
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

    let status = cmd.status()?;
    if status.success() {
        let end = start.elapsed().as_millis() as f64 / 1000.0;
        eprintln!("  {GREEN_FG}✅Finished  {RESET}{safety_mode} in {end:.3}s",);

        if !force_emit_ir {
            let _ = fs::remove_file(ir_path);
        } else {
            println!("  {GREEN_FG} IR saved  {RESET}{}", ir_path.display());
        }
    }
    Ok(exe_path)
}
