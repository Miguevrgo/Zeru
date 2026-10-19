//! Completion and signature help. What is being typed rarely parses, so the
//! program is analysed again with a placeholder name where the cursor is,
//! ended by a `;`, or by nothing where a `;` breaks the statement, and the
//! checker's record of that name and of what comes before it is read.

use std::collections::HashSet;
use std::path::Path;

use lsp_types::{
    CompletionItem, CompletionItemKind, CompletionParams, CompletionResponse, ParameterInformation,
    ParameterLabel, SignatureHelp, SignatureHelpParams, SignatureInformation,
};

use super::analysis::{Analysis, analyse, lex, path_at};
use super::{Docs, Lines, document, read};
use crate::ast::{PRIMITIVES, StatementKind};
use crate::check::{Def, PRINTS, Receiver, VEC_METHODS};
use crate::errors::Span;
use crate::items::{DefId, ItemKind, ModuleId};
use crate::lexer::KEYWORDS;
use crate::token::Token;
use crate::ty::{Ty, TyKind, Types};

const HOLE: &str = "__zeru_hole";

/// What the text before the cursor asks to complete.
enum Context {
    Member,
    Path(String),
    Name,
}

/// What a `.` follows: a value, by its type seen through any pointers, or a
/// struct or `Vec` named.
enum Subject {
    Value(Ty),
    Struct(DefId),
    Vec,
}

pub fn completion(docs: &Docs, params: CompletionParams) -> Option<CompletionResponse> {
    let at = params.text_document_position;
    let (path, text) = document(docs, &at.text_document.uri)?;
    let offset = Lines::new(text).offset(at.position);
    let start = text[..offset]
        .trim_end_matches(|c: char| c.is_ascii_alphanumeric() || c == '_')
        .len();
    let head = &text[..start];
    let context = if let Some(path) = head.strip_suffix("::") {
        let from = path
            .trim_end_matches(|c: char| c.is_ascii_alphanumeric() || c == '_' || c == ':')
            .len();
        Context::Path(path[from..].to_string())
    } else if head.ends_with('.') && !head.ends_with("..") {
        Context::Member
    } else {
        Context::Name
    };
    let found = patched(
        docs,
        &path,
        text,
        start,
        offset,
        |analysis, hole, last| match &context {
            Context::Member => Some(members(analysis, subject(analysis, hole - 1)?)),
            Context::Path(path) => Some(path_items(analysis, path)),
            Context::Name => names(analysis, hole, last),
        },
    );
    Some(CompletionResponse::Array(found?))
}

/// `then` on the program with the text from `cut` to `end` replaced by a
/// placeholder, which it is handed the offset of, and whether this is the
/// last try.
fn patched<R>(
    docs: &Docs,
    path: &Path,
    text: &str,
    cut: usize,
    end: usize,
    then: impl Fn(&mut Analysis, usize, bool) -> Option<R>,
) -> Option<R> {
    for (close, last) in [(";", false), ("", true)] {
        let text = format!("{}{HOLE}{close}{}", &text[..cut], &text[end..]);
        let found = analyse(path, &text, &|file| read(docs, file), |analysis| {
            let hole = analysis.root().start + cut;
            then(analysis, hole, last)
        });
        if found.is_some() {
            return found;
        }
    }
    None
}

/// What the expression ending at the `.` at `dot` is.
fn subject(analysis: &Analysis, dot: usize) -> Option<Subject> {
    let checked = analysis
        .record
        .types
        .iter()
        .rev()
        .find(|(span, ty)| span.end == dot && *ty != Types::ERROR);
    if let Some(&(_, mut ty)) = checked {
        let types = &analysis.checker.types;
        while let TyKind::Ref(inner) | TyKind::RefMut(inner) | TyKind::Ptr(inner) = types.kind(ty) {
            ty = *inner;
        }
        return Some(Subject::Value(ty));
    }
    let tokens = &analysis.root().tokens;
    let at = tokens.iter().position(|(_, span)| span.end == dot)?;
    let path = path_at(tokens, at)?;
    if path == "Vec" {
        return Some(Subject::Vec);
    }
    let def = analysis.items.lookup_type(ModuleId::ROOT, &path)?;
    (analysis.items.item(def).kind == ItemKind::Struct).then_some(Subject::Struct(def))
}

fn item(label: &str, kind: CompletionItemKind, detail: Option<String>) -> CompletionItem {
    CompletionItem {
        label: label.to_string(),
        kind: Some(kind),
        detail,
        ..Default::default()
    }
}

/// The fields and methods of a value, or the functions of a type.
fn members(analysis: &mut Analysis, subject: Subject) -> Vec<CompletionItem> {
    let method = |name: &str| item(name, CompletionItemKind::METHOD, None);
    let methods_named = |names: &[&str]| names.iter().map(|name| method(name)).collect();
    let ty = match subject {
        Subject::Vec => return methods_named(&["new", "with_capacity"]),
        Subject::Struct(def) => return methods(analysis, def, false),
        Subject::Value(ty) => ty,
    };
    let field = |name: &str, ty: Ty, types: &Types| {
        item(name, CompletionItemKind::FIELD, Some(types.show(ty)))
    };
    let mut found: Vec<CompletionItem> = match analysis.checker.types.kind(ty).clone() {
        TyKind::Adt(def, _) if analysis.checker.is_struct(ty).is_some() => {
            let fields = analysis.checker.fields_of(ty);
            let types = &analysis.checker.types;
            let fields = fields.iter().map(|(name, ty)| field(name, *ty, types));
            let mut found: Vec<CompletionItem> = fields.collect();
            found.extend(methods(analysis, def, true));
            found
        }
        TyKind::Tuple(parts) => {
            let types = &analysis.checker.types;
            let parts = parts.iter().enumerate();
            parts
                .map(|(at, ty)| field(&at.to_string(), *ty, types))
                .collect()
        }
        TyKind::Vec(_) => methods_named(&VEC_METHODS.map(|(name, _)| name)),
        TyKind::Slice(_) => methods_named(&["len"]),
        TyKind::Optional(_) => methods_named(&["is_some", "is_none", "unwrap"]),
        TyKind::Result(..) => methods_named(&["is_ok", "is_err", "unwrap", "unwrap_err"]),
        _ => Vec::new(),
    };
    found.extend(analysis.checker.owns_heap(ty).then(|| method("copy")));
    found
}

/// The methods of a struct that take `self`, or with `on_value` false, those
/// that do not.
fn methods(analysis: &mut Analysis, owner: DefId, on_value: bool) -> Vec<CompletionItem> {
    let defs: Vec<DefId> = analysis
        .items
        .methods_of(owner)
        .filter(|&def| {
            let sig = analysis.checker.sig(def);
            sig.is_some_and(|sig| (sig.receiver != Receiver::None) == on_value)
        })
        .collect();
    defs.into_iter()
        .map(|def| {
            let name = analysis.name_of(Def::Item(def));
            item(name, CompletionItemKind::METHOD, Some(analysis.header(def)))
        })
        .collect()
}

fn kind_of(kind: ItemKind) -> CompletionItemKind {
    match kind {
        ItemKind::Fn => CompletionItemKind::FUNCTION,
        ItemKind::Struct => CompletionItemKind::STRUCT,
        ItemKind::Enum => CompletionItemKind::ENUM,
        ItemKind::Trait => CompletionItemKind::INTERFACE,
        ItemKind::Const => CompletionItemKind::CONSTANT,
    }
}

/// Whether the root file reaches `def` by `path`.
fn reaches(analysis: &Analysis, path: &str, def: DefId) -> bool {
    let items = analysis.items;
    [
        items.lookup_type(ModuleId::ROOT, path),
        items.lookup_value(ModuleId::ROOT, path),
        items.lookup_trait(ModuleId::ROOT, path),
    ]
    .contains(&Some(def))
}

/// The variants of the enum `path` names, or the public items of the
/// module it names.
fn path_items(analysis: &mut Analysis, path: &str) -> Vec<CompletionItem> {
    if let Some(def) = analysis.items.lookup_type(ModuleId::ROOT, path)
        && analysis.items.item(def).kind == ItemKind::Enum
    {
        let variants = analysis.checker.variants_of(def);
        return variants
            .into_iter()
            .map(|(name, fields)| {
                let shown: Vec<String> = fields
                    .iter()
                    .map(|&ty| analysis.checker.types.show(ty))
                    .collect();
                let detail = (!shown.is_empty()).then(|| format!("({})", shown.join(", ")));
                item(&name, CompletionItemKind::ENUM_MEMBER, detail)
            })
            .collect();
    }
    let items = analysis.items;
    (0..items.items.len() as u32)
        .map(DefId)
        .filter(|&def| {
            let item = items.item(def);
            item.owner.is_none()
                && item.is_pub
                && items.module_name(item.module) == Some(path)
                && reaches(analysis, &item.name, def)
        })
        .map(|def| {
            let name = analysis.name_of(Def::Item(def));
            item(name, kind_of(items.item(def).kind), None)
        })
        .collect()
}

/// Every name the root file can use where `hole` is: the locals in scope
/// there, which only a try whose placeholder was checked knows, the items
/// and modules it sees, the keywords and the built-in names.
fn names(analysis: &mut Analysis, hole: usize, last: bool) -> Option<Vec<CompletionItem>> {
    let scope = analysis
        .record
        .scopes
        .iter()
        .find(|(span, _)| span.start == hole);
    if scope.is_none() && !last {
        return None;
    }
    let mut seen = HashSet::new();
    let mut found: Vec<CompletionItem> = scope
        .into_iter()
        .flat_map(|(_, locals)| locals.iter().rev())
        .filter(|(name, _)| seen.insert(name.clone()))
        .map(|(name, ty)| {
            let shown = analysis.checker.types.show(*ty);
            item(name, CompletionItemKind::VARIABLE, Some(shown))
        })
        .collect();
    let items = analysis.items;
    for (at, entry) in items.items.iter().enumerate() {
        let def = DefId(at as u32);
        let name = analysis.name_of(Def::Item(def));
        if entry.owner.is_none() && reaches(analysis, name, def) {
            found.push(item(name, kind_of(entry.kind), None));
        }
    }
    for statement in &analysis.modules[ModuleId::ROOT.0 as usize]
        .program
        .statements
    {
        if let StatementKind::Import { path, .. } = &statement.kind {
            let name = path.last().map_or("", String::as_str);
            found.push(item(name, CompletionItemKind::MODULE, None));
        }
    }
    let words = KEYWORDS
        .iter()
        .map(|(word, _)| (*word, CompletionItemKind::KEYWORD));
    let types = PRIMITIVES.iter().chain(&["void", "Vec", "Array"]);
    let types = types.map(|name| (*name, CompletionItemKind::STRUCT));
    let prints = PRINTS
        .iter()
        .map(|name| (*name, CompletionItemKind::FUNCTION));
    let built = words.chain(types).chain(prints);
    found.extend(built.map(|(name, kind)| item(name, kind, None)));
    Some(found)
}

pub fn signature_help(docs: &Docs, params: SignatureHelpParams) -> Option<SignatureHelp> {
    let at = params.text_document_position_params;
    let (path, text) = document(docs, &at.text_document.uri)?;
    let offset = Lines::new(text).offset(at.position);
    let tokens = lex(&text[..offset], 0);
    let (open, active) = open_call(&tokens)?;
    let callee = open.checked_sub(1)?;
    let Token::Identifier(name) = &tokens[callee].0 else {
        return None;
    };
    let dot = callee
        .checked_sub(1)
        .filter(|&at| tokens[at].0 == Token::Dot)
        .map(|at| tokens[at].1.start);
    let path_name = path_at(&tokens, callee)?;
    let (cut, end) = dot.map_or((offset, offset), |dot| (dot + 1, tokens[callee].1.end));
    patched(docs, &path, text, cut, end, |analysis, hole, _| {
        let def = match dot {
            None => analysis
                .items
                .lookup_value(ModuleId::ROOT, &path_name)
                .filter(|&def| analysis.items.item(def).kind == ItemKind::Fn)?,
            Some(_) => {
                let owner = match subject(analysis, hole - 1)? {
                    Subject::Struct(def) => def,
                    Subject::Value(ty) => analysis.checker.is_struct(ty)?,
                    Subject::Vec => return None,
                };
                analysis.items.method(owner, name)?
            }
        };
        let header = analysis.header(def);
        let skip = analysis
            .checker
            .sig(def)
            .is_some_and(|sig| sig.receiver != Receiver::None);
        let parameters = parameters(&header)
            .into_iter()
            .map(|label| ParameterInformation {
                label: ParameterLabel::Simple(label),
                documentation: None,
            })
            .collect();
        Some(SignatureHelp {
            signatures: vec![SignatureInformation {
                label: header,
                documentation: None,
                parameters: Some(parameters),
                active_parameter: None,
            }],
            active_signature: Some(0),
            active_parameter: Some((active + usize::from(skip)) as u32),
        })
    })
}

/// The `(` of the call the last token is inside, and how many of its
/// arguments come before.
fn open_call(tokens: &[(Token, Span)]) -> Option<(usize, usize)> {
    let mut depth = 0;
    let mut commas = 0;
    for (at, (token, _)) in tokens.iter().enumerate().rev() {
        match token {
            Token::RParen | Token::RBracket | Token::RBrace => depth += 1,
            Token::LParen if depth == 0 => return Some((at, commas)),
            Token::LParen | Token::LBracket | Token::LBrace if depth > 0 => depth -= 1,
            Token::LBracket | Token::LBrace | Token::Semicolon if depth == 0 => return None,
            Token::Comma if depth == 0 => commas += 1,
            _ => {}
        }
    }
    None
}

/// Each parameter of a signature, as written between its parentheses.
fn parameters(header: &str) -> Vec<String> {
    let Some(open) = header.find('(') else {
        return Vec::new();
    };
    let mut depth = 0;
    let mut found = vec![String::new()];
    for c in header[open + 1..].chars() {
        match c {
            ')' if depth == 0 => break,
            ',' if depth == 0 => {
                found.push(String::new());
                continue;
            }
            '(' | '<' | '[' => depth += 1,
            ')' | '>' | ']' => depth -= 1,
            _ => {}
        }
        found.last_mut().unwrap().push(c);
    }
    let found = found.iter().map(|parameter| parameter.trim().to_string());
    found.filter(|parameter| !parameter.is_empty()).collect()
}
