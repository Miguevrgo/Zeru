//! The language server, `zeru lsp`: LSP over stdin and stdout, answering from
//! the front end `zeru build` runs, so the two never disagree. Requests are
//! answered one at a time, each from a fresh analysis of the program as the
//! editor holds it; Zeru files are small.

mod analysis;
mod complete;
mod tokens;

use std::collections::HashMap;
use std::io;
use std::panic::{AssertUnwindSafe, catch_unwind};
use std::path::{Path, PathBuf};

use lsp_server::{Connection, ErrorCode, Message, Notification, Request, Response};
use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, DidSaveTextDocument,
    Notification as _, PublishDiagnostics,
};
use lsp_types::request::{
    Completion, DocumentSymbolRequest, GotoDefinition, HoverRequest, References, Rename,
    Request as _, SemanticTokensFullRequest, SignatureHelpRequest,
};
use lsp_types::{
    CompletionOptions, Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity,
    DocumentSymbol, DocumentSymbolParams, DocumentSymbolResponse, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, HoverProviderCapability, Location,
    MarkupContent, MarkupKind, OneOf, Position, PublishDiagnosticsParams, Range, ReferenceParams,
    RenameParams, SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    ServerCapabilities, SignatureHelpOptions, SymbolKind, TextDocumentSyncCapability,
    TextDocumentSyncKind, TextDocumentSyncOptions, TextDocumentSyncSaveOptions, TextEdit, Uri,
    WorkspaceEdit,
};

use crate::ast::{Statement, StatementKind};
use crate::check::Checker;
use crate::errors::Span;
use crate::items::{Items, ModuleId};
use crate::lower::SafetyMode;
use crate::token::Token;
use analysis::{Analysis, analyse};

/// The text of each file the editor has open, by path.
type Docs = HashMap<PathBuf, String>;

pub fn run() -> io::Result<()> {
    let (connection, threads) = Connection::stdio();
    let capabilities = serde_json::to_value(capabilities())?;
    connection
        .initialize(capabilities)
        .map_err(io::Error::other)?;
    serve(connection)?;
    threads.join()
}

/// Answer each request, and keep the diagnostics of every open file current,
/// until the editor shuts the server down.
fn serve(connection: Connection) -> io::Result<()> {
    let mut docs = Docs::new();
    for message in &connection.receiver {
        match message {
            Message::Request(request) => {
                if connection
                    .handle_shutdown(&request)
                    .map_err(io::Error::other)?
                {
                    break;
                }
                let id = request.id.clone();
                let response = catch_unwind(AssertUnwindSafe(|| respond(&docs, request)))
                    .unwrap_or_else(|_| {
                        let code = ErrorCode::InternalError as i32;
                        Response::new_err(id, code, "The analysis failed".to_string())
                    });
                send(&connection, Message::Response(response))?;
            }
            Message::Notification(notification) => {
                let Some(changed) = update(&mut docs, notification) else {
                    continue;
                };
                if !docs.contains_key(&changed) {
                    publish(&connection, &changed, Vec::new())?;
                }
                for path in docs.keys() {
                    let found = catch_unwind(|| diagnostics(&docs, path));
                    publish(&connection, path, found.unwrap_or_default())?;
                }
            }
            Message::Response(_) => {}
        }
    }
    Ok(())
}

fn capabilities() -> ServerCapabilities {
    ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                change: Some(TextDocumentSyncKind::FULL),
                save: Some(TextDocumentSyncSaveOptions::Supported(true)),
                ..Default::default()
            },
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        references_provider: Some(OneOf::Left(true)),
        rename_provider: Some(OneOf::Left(true)),
        document_symbol_provider: Some(OneOf::Left(true)),
        completion_provider: Some(CompletionOptions {
            trigger_characters: Some(vec![".".into(), ":".into()]),
            ..Default::default()
        }),
        signature_help_provider: Some(SignatureHelpOptions {
            trigger_characters: Some(vec!["(".into(), ",".into()]),
            ..Default::default()
        }),
        semantic_tokens_provider: Some(
            SemanticTokensOptions {
                legend: SemanticTokensLegend {
                    token_types: tokens::LEGEND.to_vec(),
                    token_modifiers: Vec::new(),
                },
                full: Some(SemanticTokensFullOptions::Bool(true)),
                ..Default::default()
            }
            .into(),
        ),
        ..Default::default()
    }
}

fn send(connection: &Connection, message: Message) -> io::Result<()> {
    connection.sender.send(message).map_err(io::Error::other)
}

fn respond(docs: &Docs, request: Request) -> Response {
    match request.method.as_str() {
        HoverRequest::METHOD => answer::<HoverRequest>(request, |p| hover(docs, p)),
        GotoDefinition::METHOD => answer::<GotoDefinition>(request, |p| definition(docs, p)),
        References::METHOD => answer::<References>(request, |p| references(docs, p)),
        Rename::METHOD => answer::<Rename>(request, |p| rename(docs, p)),
        DocumentSymbolRequest::METHOD => {
            answer::<DocumentSymbolRequest>(request, |p| symbols(docs, p))
        }
        Completion::METHOD => answer::<Completion>(request, |p| complete::completion(docs, p)),
        SignatureHelpRequest::METHOD => {
            answer::<SignatureHelpRequest>(request, |p| complete::signature_help(docs, p))
        }
        SemanticTokensFullRequest::METHOD => {
            answer::<SemanticTokensFullRequest>(request, |p| tokens::semantic_tokens(docs, p))
        }
        method => {
            let code = ErrorCode::MethodNotFound as i32;
            Response::new_err(request.id, code, format!("{method} is not supported"))
        }
    }
}

fn answer<R: lsp_types::request::Request>(
    request: Request,
    handle: impl FnOnce(R::Params) -> R::Result,
) -> Response {
    match serde_json::from_value(request.params) {
        Ok(params) => Response::new_ok(request.id, handle(params)),
        Err(error) => {
            let code = ErrorCode::InvalidParams as i32;
            Response::new_err(request.id, code, error.to_string())
        }
    }
}

/// Keep the text of an opened, changed or closed file, and say which file
/// that was.
fn update(docs: &mut Docs, notification: Notification) -> Option<PathBuf> {
    let params = notification.params;
    match notification.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: lsp_types::DidOpenTextDocumentParams =
                serde_json::from_value(params).ok()?;
            let path = path_of(&params.text_document.uri)?;
            docs.insert(path.clone(), params.text_document.text);
            Some(path)
        }
        DidChangeTextDocument::METHOD => {
            let mut params: lsp_types::DidChangeTextDocumentParams =
                serde_json::from_value(params).ok()?;
            let path = path_of(&params.text_document.uri)?;
            docs.insert(path.clone(), params.content_changes.pop()?.text);
            Some(path)
        }
        DidSaveTextDocument::METHOD => {
            let params: lsp_types::DidSaveTextDocumentParams =
                serde_json::from_value(params).ok()?;
            path_of(&params.text_document.uri)
        }
        DidCloseTextDocument::METHOD => {
            let params: lsp_types::DidCloseTextDocumentParams =
                serde_json::from_value(params).ok()?;
            let path = path_of(&params.text_document.uri)?;
            docs.remove(&path);
            Some(path)
        }
        _ => None,
    }
}

fn publish(connection: &Connection, path: &Path, diagnostics: Vec<Diagnostic>) -> io::Result<()> {
    let params = PublishDiagnosticsParams {
        uri: uri_of(path),
        diagnostics,
        version: None,
    };
    let notification = Notification::new(PublishDiagnostics::METHOD.to_string(), params);
    send(connection, Message::Notification(notification))
}

/// A module's text: the editor's, if it has the file open.
fn read(docs: &Docs, path: &Path) -> Option<String> {
    docs.get(path)
        .cloned()
        .or_else(|| std::fs::read_to_string(path).ok())
}

fn path_of(uri: &Uri) -> Option<PathBuf> {
    let path = uri.path().as_estr().decode().into_string().ok()?;
    Some(PathBuf::from(path.into_owned()))
}

fn uri_of(path: &Path) -> Uri {
    let mut uri = String::from("file://");
    for byte in path.to_string_lossy().bytes() {
        match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'/' | b'-' | b'.' | b'_' | b'~' => {
                uri.push(byte as char)
            }
            _ => uri.push_str(&format!("%{byte:02X}")),
        }
    }
    uri.parse().expect("an encoded path is a URI")
}

/// The open file `uri` names: its path and text.
fn document<'d>(docs: &'d Docs, uri: &Uri) -> Option<(PathBuf, &'d str)> {
    let path = path_of(uri)?;
    let text = docs.get(&path)?;
    Some((path, text))
}

/// Analyse the program rooted at the open file `uri` names, and hand `then`
/// the analysis and where `position` is among its spans.
fn analyse_at<R>(
    docs: &Docs,
    uri: &Uri,
    position: Position,
    then: impl FnOnce(&mut Analysis, usize) -> Option<R>,
) -> Option<R> {
    let (path, text) = document(docs, uri)?;
    analyse(&path, text, &|file| read(docs, file), |analysis| {
        let root = analysis.root();
        let offset = root.start + Lines::new(root.text).offset(position);
        then(analysis, offset)
    })
}

/// A file's text and where its lines start, to turn byte offsets into LSP
/// positions, whose columns count UTF-16 units, and back.
pub struct Lines<'t> {
    text: &'t str,
    starts: Vec<usize>,
}

impl<'t> Lines<'t> {
    pub fn new(text: &'t str) -> Self {
        let starts = std::iter::once(0)
            .chain(text.match_indices('\n').map(|(at, _)| at + 1))
            .collect();
        Self { text, starts }
    }

    pub fn position(&self, offset: usize) -> Position {
        let offset = offset.min(self.text.len());
        let line = self.starts.partition_point(|&start| start <= offset) - 1;
        let column = self.text[self.starts[line]..offset].encode_utf16().count();
        Position::new(line as u32, column as u32)
    }

    pub fn offset(&self, position: Position) -> usize {
        let Some(&start) = self.starts.get(position.line as usize) else {
            return self.text.len();
        };
        let mut units = 0;
        for (at, ch) in self.text[start..].char_indices() {
            if units >= position.character as usize || ch == '\n' {
                return start + at;
            }
            units += ch.len_utf16();
        }
        self.text.len()
    }

    pub fn range(&self, start: usize, end: usize) -> Range {
        Range::new(self.position(start), self.position(end))
    }
}

/// Where `span` is, in the file it is in.
fn location(analysis: &Analysis, span: Span) -> Location {
    let file = analysis.file_at(span.start);
    let start = analysis.files[file].start;
    let lines = Lines::new(analysis.files[file].text);
    let range = lines.range(span.start - start, span.end - start);
    Location::new(uri_of(&analysis.path(file)), range)
}

/// What `zeru build` would report for the program rooted at `path`: the
/// errors of loading and parsing, else those of checking, else those of the
/// borrow checker. An error in another file is shown at the top.
fn diagnostics(docs: &Docs, path: &Path) -> Vec<Diagnostic> {
    let (sources, modules, mut errors) =
        crate::driver::load(path, &docs[path], &|file| read(docs, file));
    errors.retain(|error| error.message != "Missing 'fn main'");
    if errors.is_empty() {
        let items = Items::collect(&modules);
        errors = match Checker::new(&items).check() {
            Ok(mut program) => crate::mir::build(&mut program, SafetyMode::Debug)
                .err()
                .unwrap_or_default(),
            Err(errors) => errors,
        };
    }
    let files: Vec<(PathBuf, usize, &str)> = sources
        .files()
        .enumerate()
        .map(|(at, (name, start, text))| (analysis::file_path(at, name), start, text))
        .collect();
    let locate = |span: Span| {
        let (path, start, text) = files
            .iter()
            .rev()
            .find(|(_, start, _)| *start <= span.start)?;
        let range = Lines::new(text).range(span.start - start, span.end - start);
        Some(Location::new(uri_of(path), range))
    };
    let root = uri_of(path);
    errors
        .iter()
        .map(|error| {
            let at = locate(error.span);
            let (range, message) = match at {
                Some(at) if at.uri == root => (at.range, error.message.clone()),
                _ => {
                    let (name, line, column) = sources.position(error.span).unwrap_or_default();
                    let message = format!("{name}:{line}:{column}: {}", error.message);
                    (Range::default(), message)
                }
            };
            let related = error
                .notes
                .iter()
                .filter_map(|(note, span)| {
                    Some(DiagnosticRelatedInformation {
                        location: locate(*span)?,
                        message: note.clone(),
                    })
                })
                .collect();
            Diagnostic {
                range,
                severity: Some(DiagnosticSeverity::ERROR),
                source: Some("zeru".to_string()),
                message,
                related_information: Some(related),
                ..Default::default()
            }
        })
        .collect()
}

fn hover(docs: &Docs, params: HoverParams) -> Option<Hover> {
    let at = params.text_document_position_params;
    analyse_at(
        docs,
        &at.text_document.uri,
        at.position,
        |analysis, offset| {
            let (span, def) = analysis.at(offset)?;
            let shown = analysis.describe(span, def)?;
            Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```zeru\n{shown}\n```"),
                }),
                range: None,
            })
        },
    )
}

fn definition(docs: &Docs, params: GotoDefinitionParams) -> Option<GotoDefinitionResponse> {
    let at = params.text_document_position_params;
    analyse_at(
        docs,
        &at.text_document.uri,
        at.position,
        |analysis, offset| {
            let (span, def) = analysis.at(offset)?;
            let declared = analysis.declared(def, analysis.text(span))?;
            Some(GotoDefinitionResponse::Scalar(location(analysis, declared)))
        },
    )
}

/// Every name in the program that names what the name at the cursor does.
fn occurrences(analysis: &Analysis, offset: usize) -> Option<Vec<Span>> {
    let name = analysis.at(offset)?;
    let spans = analysis
        .names
        .iter()
        .filter(|&&other| analysis.same(name, other))
        .map(|(span, _)| *span)
        .collect();
    Some(spans)
}

fn references(docs: &Docs, params: ReferenceParams) -> Option<Vec<Location>> {
    let at = params.text_document_position;
    let with_declaration = params.context.include_declaration;
    analyse_at(
        docs,
        &at.text_document.uri,
        at.position,
        |analysis, offset| {
            let (span, def) = analysis.at(offset)?;
            let declared = analysis.declared(def, analysis.text(span));
            let spans = occurrences(analysis, offset)?;
            let found = spans
                .into_iter()
                .filter(|&span| with_declaration || Some(span) != declared)
                .map(|span| location(analysis, span))
                .collect();
            Some(found)
        },
    )
}

/// Rename what the name at the cursor names, wherever the program names it.
/// What the standard library declares stays as it is.
fn rename(docs: &Docs, params: RenameParams) -> Option<WorkspaceEdit> {
    let at = params.text_document_position;
    analyse_at(
        docs,
        &at.text_document.uri,
        at.position,
        |analysis, offset| {
            let (span, def) = analysis.at(offset)?;
            let declared = analysis.declared(def, analysis.text(span))?;
            if analysis.modules[analysis.file_at(declared.start)].std {
                return None;
            }
            let spans = occurrences(analysis, offset)?;
            let file = |span: &Span| analysis.file_at(span.start);
            let changes = spans.chunk_by(|a, b| file(a) == file(b)).map(|spans| {
                let edits = spans.iter().map(|&span| {
                    TextEdit::new(location(analysis, span).range, params.new_name.clone())
                });
                (uri_of(&analysis.path(file(&spans[0]))), edits.collect())
            });
            Some(WorkspaceEdit::new(changes.collect()))
        },
    )
}

fn symbols(docs: &Docs, params: DocumentSymbolParams) -> Option<DocumentSymbolResponse> {
    let (path, text) = document(docs, &params.text_document.uri)?;
    analyse(&path, text, &|file| read(docs, file), |analysis| {
        let lines = Lines::new(analysis.root().text);
        let statements = &analysis.modules[ModuleId::ROOT.0 as usize]
            .program
            .statements;
        let found = statements
            .iter()
            .filter_map(|statement| symbol(analysis, &lines, statement))
            .collect();
        Some(DocumentSymbolResponse::Nested(found))
    })
}

/// The symbol a declaration in the root file makes, with those of its
/// fields, methods and variants.
fn symbol(analysis: &Analysis, lines: &Lines, statement: &Statement) -> Option<DocumentSymbol> {
    let any = |_: &Token| true;
    let (name, kind, children) = match &statement.kind {
        StatementKind::Function { name, .. } => (name, SymbolKind::FUNCTION, Vec::new()),
        StatementKind::Var { name, .. } => (name, SymbolKind::CONSTANT, Vec::new()),
        StatementKind::Trait { name, .. } => (name, SymbolKind::INTERFACE, Vec::new()),
        StatementKind::Struct {
            name,
            fields,
            methods,
            ..
        } => {
            let colon = |next: &Token| *next == Token::Colon;
            let fields = fields.iter().filter_map(|(field, _)| {
                let at = analysis.find(statement.span, field, colon)?;
                Some(leaf(lines, analysis, field, SymbolKind::FIELD, at, at))
            });
            let methods = methods.iter().filter_map(|method| {
                let StatementKind::Function { name, .. } = &method.kind else {
                    return None;
                };
                let at = analysis.find(method.span, name, any)?;
                Some(leaf(
                    lines,
                    analysis,
                    name,
                    SymbolKind::METHOD,
                    method.span,
                    at,
                ))
            });
            (name, SymbolKind::STRUCT, fields.chain(methods).collect())
        }
        StatementKind::Enum { name, variants } => {
            let variants = variants.iter().filter_map(|(variant, _)| {
                let at = analysis.find(statement.span, variant, |next| {
                    matches!(next, Token::LParen | Token::Comma | Token::RBrace)
                })?;
                Some(leaf(
                    lines,
                    analysis,
                    variant,
                    SymbolKind::ENUM_MEMBER,
                    at,
                    at,
                ))
            });
            (name, SymbolKind::ENUM, variants.collect())
        }
        _ => return None,
    };
    let at = analysis.find(statement.span, name, any)?;
    let mut symbol = leaf(lines, analysis, name, kind, statement.span, at);
    symbol.children = Some(children);
    Some(symbol)
}

#[allow(deprecated)]
fn leaf(
    lines: &Lines,
    analysis: &Analysis,
    name: &str,
    kind: SymbolKind,
    whole: Span,
    at: Span,
) -> DocumentSymbol {
    let start = analysis.root().start;
    DocumentSymbol {
        name: name.to_string(),
        detail: None,
        kind,
        tags: None,
        deprecated: None,
        range: lines.range(whole.start - start, whole.end - start),
        selection_range: lines.range(at.start - start, at.end - start),
        children: None,
    }
}
