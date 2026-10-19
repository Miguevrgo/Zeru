//! One look at a program for the language server: the front end run over the
//! editor's text as `zeru build` runs it, up to the type checker, and every
//! name in every file of the program paired with what it names.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::ast::StatementKind;
use crate::check::{Checker, Def, Record};
use crate::errors::Span;
use crate::items::{DefId, ItemKind, Items, ModuleId, SourceModule};
use crate::lexer::Lexer;
use crate::token::Token;

/// A file of the program, in the order loaded, which is its module's: its
/// name, where its spans start, its text and its tokens.
pub struct File<'a> {
    pub name: &'a str,
    pub start: usize,
    pub text: &'a str,
    pub tokens: Vec<(Token, Span)>,
}

pub struct Analysis<'a> {
    pub files: Vec<File<'a>>,
    pub modules: &'a [SourceModule],
    pub items: &'a Items<'a>,
    pub checker: Checker<'a>,
    pub record: Record,
    /// Each name in the program, at the span of its token, with what it
    /// names, in order.
    pub names: Vec<(Span, Def)>,
}

/// Load, parse and check the program whose root is `text`, at `path`,
/// reading the modules it imports with `read`, and hand what was found to
/// `then`. Errors do not stop it: what checked is kept.
pub fn analyse<R>(
    path: &Path,
    text: &str,
    read: &dyn Fn(&Path) -> Option<String>,
    then: impl FnOnce(&mut Analysis) -> R,
) -> R {
    let (sources, modules, _) = crate::driver::load(path, text, read);
    let items = Items::collect(&modules);
    let mut checker = Checker::new(&items);
    checker.record = Some(Record::default());
    checker.run();
    let record = checker.record.take().unwrap_or_default();
    let files = sources
        .files()
        .map(|(name, start, text)| File {
            name,
            start,
            text,
            tokens: lex(text, start),
        })
        .collect();
    let mut analysis = Analysis {
        files,
        modules: &modules,
        items: &items,
        checker,
        record,
        names: Vec::new(),
    };
    analysis.names = analysis.index();
    then(&mut analysis)
}

impl<'a> Analysis<'a> {
    pub fn root(&self) -> &File<'a> {
        &self.files[ModuleId::ROOT.0 as usize]
    }

    /// The index of the file `offset` is in.
    pub fn file_at(&self, offset: usize) -> usize {
        self.files
            .partition_point(|file| file.start <= offset)
            .max(1)
            - 1
    }

    pub fn path(&self, file: usize) -> PathBuf {
        file_path(file, self.files[file].name)
    }

    pub fn text(&self, span: Span) -> &'a str {
        let file = &self.files[self.file_at(span.start)];
        &file.text[span.start - file.start..(span.end - file.start).min(file.text.len())]
    }

    /// The whole line `span` starts on.
    pub fn line(&self, span: Span) -> &'a str {
        let file = &self.files[self.file_at(span.start)];
        let at = span.start - file.start;
        let start = file.text[..at].rfind('\n').map_or(0, |end| end + 1);
        let end = file.text[at..]
            .find('\n')
            .map_or(file.text.len(), |end| at + end);
        &file.text[start..end]
    }

    /// The name and what it names at `offset`, touching it from either side.
    pub fn at(&self, offset: usize) -> Option<(Span, Def)> {
        let after = self.names.partition_point(|(span, _)| span.start <= offset);
        let (span, def) = *self.names[..after].last()?;
        (offset <= span.end).then_some((span, def))
    }

    /// Whether two names name the same thing.
    pub fn same(&self, (a_span, a): (Span, Def), (b_span, b): (Span, Def)) -> bool {
        match (a, b) {
            (Def::Local(a, _), Def::Local(b, _)) => {
                a == b && self.text(a_span) == self.text(b_span)
            }
            _ => a == b,
        }
    }

    /// The name a definition other than a local's is declared with.
    pub fn name_of(&self, def: Def) -> &'a str {
        match def {
            Def::Local(..) => "",
            Def::Item(def) => self
                .items
                .item(def)
                .name
                .rsplit("::")
                .next()
                .unwrap_or_default(),
            Def::Field(def, at) => match &self.items.item(def).decl.kind {
                StatementKind::Struct { fields, .. } => &fields[at].0,
                _ => "",
            },
            Def::Variant(def, at) => match &self.items.item(def).decl.kind {
                StatementKind::Enum { variants, .. } => &variants[at].0,
                _ => "",
            },
        }
    }

    /// The token naming `name` in `within` that `next` may follow.
    pub fn find(&self, within: Span, name: &str, next: impl Fn(&Token) -> bool) -> Option<Span> {
        let tokens = &self.files[self.file_at(within.start)].tokens;
        let from = tokens.partition_point(|(_, span)| span.start < within.start);
        tokens[from..]
            .windows(2)
            .take_while(|pair| pair[0].1.start < within.end)
            .find(|pair| is_named(&pair[0].0, name) && next(&pair[1].0))
            .map(|pair| pair[0].1)
    }

    /// Where `def`, called `name`, is declared.
    pub fn declared(&self, def: Def, name: &str) -> Option<Span> {
        let decl = |def: DefId| self.items.item(def).decl.span;
        match def {
            Def::Local(span, _) => self.find(span, name, |next| {
                !matches!(next, Token::DoubleColon | Token::LParen)
            }),
            Def::Item(def) => self.find(decl(def), name, |_| true),
            Def::Field(def, _) => self.find(decl(def), name, |next| *next == Token::Colon),
            Def::Variant(def, _) => self.find(decl(def), name, |next| {
                matches!(next, Token::LParen | Token::Comma | Token::RBrace)
            }),
        }
    }

    /// What hovering over the name at `span` shows: a local's type in the
    /// instance checked, a function's signature, a variant's values, or the
    /// line declaring anything else.
    pub fn describe(&mut self, span: Span, def: Def) -> Option<String> {
        let name = self.text(span);
        Some(match def {
            Def::Local(_, ty) => format!("var {name}: {}", self.checker.types.show(ty)),
            Def::Item(def) if self.items.item(def).kind == ItemKind::Fn => self.header(def),
            Def::Item(def) => {
                let line = self.line(self.items.item(def).decl.span);
                line.trim().trim_end_matches('{').trim_end().to_string()
            }
            Def::Field(..) => {
                let line = self.line(self.declared(def, name)?);
                line.trim().trim_end_matches(',').to_string()
            }
            Def::Variant(owner, at) => {
                let (variant, fields) = self.checker.variants_of(owner).swap_remove(at);
                let owner = &self.items.item(owner).name;
                let fields: Vec<String> = fields
                    .iter()
                    .map(|&ty| self.checker.types.show(ty))
                    .collect();
                match fields.is_empty() {
                    true => format!("{owner}::{variant}"),
                    false => format!("{owner}::{variant}({})", fields.join(", ")),
                }
            }
        })
    }

    /// A function's signature as written, on one line.
    pub fn header(&self, def: DefId) -> String {
        let text = self.text(self.items.item(def).decl.span);
        text.split_whitespace().collect::<Vec<_>>().join(" ")
    }

    /// Every name in the program: those the checker resolved, every
    /// declaration, and the names of types and functions where the checker
    /// has no span to note them by, looked up as it looks them up.
    fn index(&self) -> Vec<(Span, Def)> {
        let mut names: Vec<(Span, Def)> = Vec::new();
        for &(span, def) in &self.record.uses {
            let found = match def {
                Def::Local(..) => Some(span),
                _ => self.find(span, self.name_of(def), |_| true),
            };
            names.extend(found.map(|at| (at, def)));
        }
        for (name, decl, ty) in &self.record.locals {
            let def = Def::Local(*decl, *ty);
            names.extend(self.declared(def, name).map(|at| (at, def)));
        }
        for (at, item) in self.items.items.iter().enumerate() {
            let def = DefId(at as u32);
            let parts = match &item.decl.kind {
                StatementKind::Struct { fields, .. } => {
                    (0..fields.len()).map(|at| Def::Field(def, at)).collect()
                }
                StatementKind::Enum { variants, .. } => (0..variants.len())
                    .map(|at| Def::Variant(def, at))
                    .collect(),
                _ => Vec::new(),
            };
            for def in std::iter::once(Def::Item(def)).chain(parts) {
                names.extend(self.declared(def, self.name_of(def)).map(|at| (at, def)));
            }
        }
        let known: HashSet<usize> = names.iter().map(|(span, _)| span.start).collect();
        for (module, file) in self.files.iter().enumerate() {
            let module = ModuleId(module as u32);
            for (at, (_, span)) in file.tokens.iter().enumerate() {
                if known.contains(&span.start) || at > 0 && file.tokens[at - 1].0 == Token::Dot {
                    continue;
                }
                let Some(path) = path_at(&file.tokens, at) else {
                    continue;
                };
                let called = file
                    .tokens
                    .get(at + 1)
                    .is_some_and(|(next, _)| *next == Token::LParen);
                let def = self
                    .items
                    .lookup_type(module, &path)
                    .or_else(|| self.items.lookup_trait(module, &path))
                    .or_else(|| {
                        let def = self.items.lookup_value(module, &path)?;
                        (called && self.items.item(def).kind == ItemKind::Fn).then_some(def)
                    });
                names.extend(def.map(|def| (*span, Def::Item(def))));
            }
        }
        names.sort_by_key(|(span, _)| span.start);
        names.dedup_by_key(|(span, _)| span.start);
        names
    }
}

/// The tokens of `text`, whose spans start at `start`.
pub fn lex(text: &str, start: usize) -> Vec<(Token, Span)> {
    let mut lexer = Lexer::at(text, start);
    std::iter::from_fn(|| Some(lexer.next_token()))
        .take_while(|(token, _)| *token != Token::Eof)
        .collect()
}

/// The path of the file loaded `at`, named `name`: the prelude's is the one
/// beside the compiler.
pub fn file_path(at: usize, name: &str) -> PathBuf {
    match at {
        0 => crate::driver::std_path()
            .unwrap_or_else(|| "std".into())
            .join("builtin.zr"),
        _ => name.into(),
    }
}

fn is_named(token: &Token, name: &str) -> bool {
    match token {
        Token::Identifier(word) => word == name,
        Token::SelfTok => name == "self",
        _ => false,
    }
}

/// The path ending at token `at`, as `module::name`, if `at` is a name.
pub fn path_at(tokens: &[(Token, Span)], at: usize) -> Option<String> {
    let Token::Identifier(name) = &tokens[at].0 else {
        return None;
    };
    let module = match tokens.get(at.wrapping_sub(2)..at) {
        Some([(Token::Identifier(module), _), (Token::DoubleColon, _)]) => module,
        _ => return Some(name.clone()),
    };
    Some(format!("{module}::{name}"))
}
