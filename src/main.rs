mod ast;
mod codegen;
mod errors;
mod lexer;
mod parser;
mod sema;
mod token;

use inkwell::context::Context;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::codegen::SafetyMode;
use crate::codegen::compiler::Compiler;
use crate::errors::report_errors;
use crate::lexer::Lexer;
use crate::sema::analyzer::SemanticAnalyzer;
use crate::token::Token;

use clap::{Parser, Subcommand};

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
    Install,
}

fn get_zeru_home() -> PathBuf {
    if let Ok(home) = std::env::var("ZERU_HOME") {
        return PathBuf::from(home);
    }
    if let Ok(home) = std::env::var("HOME") {
        return PathBuf::from(home).join(".zeru");
    }
    PathBuf::from("/usr/local/lib/zeru")
}

fn get_std_path() -> PathBuf {
    get_zeru_home().join("std")
}

/// Get the path to the std directory relative to the executable.
/// This handles cases where the compiler is run from different directories.
fn get_exe_relative_std_path() -> Option<PathBuf> {
    let exe_path = std::env::current_exe().ok()?;
    // The executable is typically at target/release/zeru or target/debug/zeru
    // So we go up to find the project root and then look for std/
    let mut path = exe_path.parent()?; // target/release or target/debug

    // Try going up the directory tree to find std/
    for _ in 0..4 {
        let std_path = path.join("std");
        if std_path.exists() && std_path.is_dir() {
            return Some(std_path);
        }
        path = path.parent()?;
    }
    None
}

fn resolve_std_import(import_path: &str) -> Option<PathBuf> {
    let parts: Vec<&str> = import_path.split('.').collect();
    if parts.is_empty() || parts[0] != "std" {
        return None;
    }

    let module_file = if parts.len() == 1 {
        "builtin.zr".to_string()
    } else {
        format!("{}.zr", parts[1..].join("/"))
    };

    // Build search paths, including exe-relative path if available
    let mut search_paths = vec![
        PathBuf::from("std"),
        get_std_path(),
        PathBuf::from("/usr/local/lib/zeru/std"),
    ];

    // Add exe-relative std path (highest priority after cwd)
    if let Some(exe_std) = get_exe_relative_std_path() {
        search_paths.insert(1, exe_std);
    }

    for base in &search_paths {
        let full_path = base.join(&module_file);
        if full_path.exists() {
            return Some(full_path);
        }
    }

    None
}

fn load_builtin_std() -> String {
    // Build search paths, including exe-relative path if available
    let mut search_paths = vec![
        PathBuf::from("std/builtin.zr"),
        get_std_path().join("builtin.zr"),
        PathBuf::from("/usr/local/lib/zeru/std/builtin.zr"),
    ];

    // Add exe-relative std path (highest priority after cwd)
    if let Some(exe_std) = get_exe_relative_std_path() {
        search_paths.insert(1, exe_std.join("builtin.zr"));
    }

    for path in &search_paths {
        if let Ok(content) = fs::read_to_string(path) {
            return content;
        }
    }

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
    let mut result = String::new();
    let mut chars = content.chars().peekable();

    while let Some(c) = chars.next() {
        if c == 'f' && chars.peek() == Some(&'n') {
            let mut word = String::from(c);
            while let Some(&next) = chars.peek() {
                if next.is_alphabetic() || next == '_' {
                    word.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            if word == "fn" {
                result.push_str("fn ");
                while let Some(&ws) = chars.peek() {
                    if ws.is_whitespace() {
                        result.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                let mut name = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_alphanumeric() || nc == '_' {
                        name.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if !name.is_empty() {
                    result.push_str(prefix);
                    result.push_str("__");
                }
                result.push_str(&name);
            } else {
                result.push_str(&word);
            }
        } else if c == 's' && chars.peek() == Some(&'t') {
            let mut word = String::from(c);
            while let Some(&next) = chars.peek() {
                if next.is_alphabetic() || next == '_' {
                    word.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            if word == "struct" {
                result.push_str("struct ");
                while let Some(&ws) = chars.peek() {
                    if ws.is_whitespace() {
                        result.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                let mut name = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_alphanumeric() || nc == '_' {
                        name.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if !name.is_empty() {
                    result.push_str(prefix);
                    result.push_str("__");
                }
                result.push_str(&name);
            } else {
                result.push_str(&word);
            }
        } else if c == 'c' && chars.peek() == Some(&'o') {
            let mut word = String::from(c);
            while let Some(&next) = chars.peek() {
                if next.is_alphabetic() || next == '_' {
                    word.push(chars.next().unwrap());
                } else {
                    break;
                }
            }
            if word == "const" {
                result.push_str("const ");
                while let Some(&ws) = chars.peek() {
                    if ws.is_whitespace() {
                        result.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                let mut name = String::new();
                while let Some(&nc) = chars.peek() {
                    if nc.is_alphanumeric() || nc == '_' {
                        name.push(chars.next().unwrap());
                    } else {
                        break;
                    }
                }
                if !name.is_empty() {
                    result.push_str(prefix);
                    result.push_str("__");
                }
                result.push_str(&name);
            } else {
                result.push_str(&word);
            }
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
) -> String {
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

        if let Some(file_path) = resolve_std_import(&import.path)
            && file_path.exists()
            && let Ok(content) = fs::read_to_string(&file_path)
        {
            loaded.insert(import.path.clone());
            let short_name = get_module_short_name(&import.path);

            let nested_imports = extract_imports(&content);
            let nested_code =
                load_std_modules(&nested_imports, loaded, direct_symbols, module_prefixes);
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
        }
    }

    code
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
    let mut result = String::new();
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
            while let Some(&ws) = peek_chars.peek() {
                if ws.is_whitespace() {
                    peek_chars.next();
                } else {
                    break;
                }
            }

            if peek_chars.peek() == Some(&':') {
                peek_chars.next();
                if peek_chars.peek() == Some(&':') {
                    peek_chars.next();

                    // Skip whitespace after ::
                    while let Some(&ws) = peek_chars.peek() {
                        if ws.is_whitespace() {
                            peek_chars.next();
                        } else {
                            break;
                        }
                    }

                    let mut symbol_name = String::new();
                    while let Some(&nc) = peek_chars.peek() {
                        if nc.is_alphanumeric() || nc == '_' {
                            symbol_name.push(peek_chars.next().unwrap());
                        } else {
                            break;
                        }
                    }

                    if module_prefixes.contains(&ident) && !symbol_name.is_empty() {
                        result.push_str(&format!("{}__{}", ident, symbol_name));
                        // Advance the main iterator past the :: and symbol
                        while chars.peek().is_some() && chars.peek() != peek_chars.peek() {
                            chars.next();
                        }
                        continue;
                    }
                }
            }

            if let Some(qualified) = direct_symbols.get(&ident) {
                result.push_str(qualified);
                continue;
            }

            result.push_str(&ident);
        } else {
            result.push(c);
        }
    }
    result
}

fn compile_pipeline(
    path: &Path,
    safety_mode: SafetyMode,
    force_emit_ir: bool,
    quiet: bool,
) -> Option<PathBuf> {
    if path.extension().and_then(|s| s.to_str()) != Some("zr") {
        eprintln!("⚠️ Warning: Zeru extension is .zr");
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
            eprintln!("❌ Error reading file: {}", e);
            return None;
        }
    };

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
    );

    let user_code_resolved = resolve_direct_symbols(&user_code, &direct_symbols, &module_prefixes);
    let input = format!(
        "{}\n{}\n{}",
        std_builtin, additional_std, user_code_resolved
    );

    let lexer = Lexer::new(&input);
    let mut parser = crate::parser::Parser::new(lexer);
    let program = parser.parse_program();

    let filepath_str = path.to_str().unwrap_or("unknown");

    if !parser.errors.is_empty() {
        report_errors(&parser.errors, filepath_str, &input);
        return None;
    }

    let mut analyzer = SemanticAnalyzer::new();
    analyzer.analyze(&program);

    if !analyzer.errors.is_empty() {
        report_errors(&analyzer.errors, filepath_str, &input);
        return None;
    }

    let context = Context::create();
    let module = context.create_module(filename);
    let builder = context.create_builder();

    let mut compiler = Compiler::new(&context, &builder, &module, safety_mode.clone());
    compiler.compile_program(&program);

    if let Err(e) = module.verify() {
        eprintln!("❌ LLVM Verify Error: {}", e.to_string());
        return None;
    }

    if let Err(e) = module.print_to_file(&ir_path) {
        eprintln!("❌ Failed to write LLVM IR: {}", e);
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

            Some(exe_path)
        }
        _ => {
            eprintln!("❌ Linking failed. Ensure 'clang' is installed.");
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
        } => {
            let safety_mode = if *release_fast {
                SafetyMode::ReleaseFast
            } else if *release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            if compile_pipeline(file, safety_mode, *emit_ir, false).is_none() {
                std::process::exit(1);
            };
        }
        Commands::Run {
            file,
            release_safe,
            release_fast,
        } => {
            let safety_mode = if *release_fast {
                SafetyMode::ReleaseFast
            } else if *release_safe {
                SafetyMode::ReleaseSafe
            } else {
                SafetyMode::Debug
            };
            if let Some(executable_path) = compile_pipeline(file, safety_mode, false, true) {
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
        Commands::Install => {
            install_zeru();
        }
    }
}

fn install_zeru() {
    let zeru_home = get_zeru_home();
    let std_dest = zeru_home.join("std");
    let bin_dest = zeru_home.join("bin");

    println!("Installing Zeru to {}...", zeru_home.display());

    if let Err(e) = fs::create_dir_all(&std_dest) {
        eprintln!("❌ Failed to create std directory: {}", e);
        std::process::exit(1);
    }
    if let Err(e) = fs::create_dir_all(&bin_dest) {
        eprintln!("❌ Failed to create bin directory: {}", e);
        std::process::exit(1);
    }

    let std_files = ["builtin.zr", "math.zr"];
    let src_std = PathBuf::from("std");

    for file in &std_files {
        let src = src_std.join(file);
        let dest = std_dest.join(file);
        if src.exists() {
            if let Err(e) = fs::copy(&src, &dest) {
                eprintln!("❌ Failed to copy {}: {}", file, e);
            } else {
                println!("  Installed std/{}", file);
            }
        }
    }

    let current_exe = std::env::current_exe().expect("Failed to get current executable path");
    let bin_path = bin_dest.join("zeru");
    if let Err(e) = fs::copy(&current_exe, &bin_path) {
        eprintln!("❌ Failed to copy binary: {}", e);
        std::process::exit(1);
    }
    println!("  Installed zeru binary");

    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&bin_path).unwrap().permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&bin_path, perms).ok();
    }

    println!("\n✅ Zeru installed successfully!");
    println!("\nAdd the following to your shell profile (.bashrc, .zshrc, etc.):");
    println!("  export PATH=\"{}:$PATH\"", bin_dest.display());
}
