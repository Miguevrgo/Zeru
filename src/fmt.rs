//! `zeru fmt`: a file laid out again from its syntax tree.
//! Indents are four spaces, and a line that would pass 100 columns breaks at
//! its outermost group first: a call's arguments one per line, an operator
//! chain before each operator, a method chain before each `.`. Comments and
//! single blank lines stay where they were; a comment inside an expression,
//! which has no place in the new layout, is an error instead. The result is
//! parsed again and must give the same tree and comments, or nothing changes.

use std::collections::HashSet;

use crate::ast::{
    Expression, ExpressionKind as E, Pattern, PatternKind, Program, Statement, StatementKind as S,
    TypeParameter, TypeSpec,
};
use crate::errors::{Span, ZeruError};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::Token;

const WIDTH: usize = 100;
const INDENT: usize = 4;

/// A layout, as in Wadler's prettier printer. `Line` is a space and `Soft`
/// nothing while their group fits on the line, and both are line breaks
/// once it does not; `IfBroken` is text only a broken group shows, as a
/// list's trailing comma; `Suffix` is text that takes no room, as a
/// comment after a statement. `Nest` indents what its group breaks onto
/// new lines, and `Indent` a block's lines whatever its group does.
enum Doc {
    Text(String),
    Suffix(String),
    Line,
    Soft,
    Hard,
    IfBroken(&'static str),
    Nest(Box<Doc>),
    Indent(Box<Doc>),
    Group(Box<Doc>),
    Seq(Vec<Doc>),
}

fn text(text: impl Into<String>) -> Doc {
    Doc::Text(text.into())
}

fn seq(docs: Vec<Doc>) -> Doc {
    Doc::Seq(docs)
}

fn nest(doc: Doc) -> Doc {
    Doc::Nest(Box::new(doc))
}

fn group(doc: Doc) -> Doc {
    Doc::Group(Box::new(doc))
}

/// `items` between `open` and `close`: on one line when they fit, with a
/// space inside the brackets if `padded`, else one per line with a
/// trailing comma.
fn list(open: &str, items: Vec<Doc>, close: &str, padded: bool) -> Doc {
    if items.is_empty() {
        return text(format!("{open}{close}"));
    }
    let edge = || if padded { Doc::Line } else { Doc::Soft };
    let mut inside = vec![edge()];
    for (at, item) in items.into_iter().enumerate() {
        if at > 0 {
            inside.extend([text(","), Doc::Line]);
        }
        inside.push(item);
    }
    inside.push(Doc::IfBroken(","));
    group(seq(vec![
        text(open),
        nest(seq(inside)),
        edge(),
        text(close),
    ]))
}

fn width(text: &str) -> usize {
    text.chars().count()
}

fn render(doc: &Doc) -> String {
    let mut out = String::new();
    let mut column = 0;
    let mut indent_due = None;
    let put = |out: &mut String, text: &str, column: &mut usize, due: &mut Option<usize>| {
        if let Some(indent) = due.take() {
            out.extend(std::iter::repeat_n(' ', indent));
        }
        out.push_str(text);
        *column += width(text);
    };
    let mut stack = vec![(0, false, doc)];
    while let Some((indent, flat, doc)) = stack.pop() {
        match doc {
            Doc::Text(text) | Doc::Suffix(text) => {
                put(&mut out, text, &mut column, &mut indent_due)
            }
            Doc::IfBroken(text) if !flat => put(&mut out, text, &mut column, &mut indent_due),
            Doc::IfBroken(_) => {}
            Doc::Line if flat => put(&mut out, " ", &mut column, &mut indent_due),
            Doc::Soft if flat => {}
            Doc::Line | Doc::Soft | Doc::Hard => {
                out.push('\n');
                column = indent;
                indent_due = Some(indent);
            }
            Doc::Nest(doc) if flat => stack.push((indent, flat, doc)),
            Doc::Nest(doc) | Doc::Indent(doc) => stack.push((indent + INDENT, flat, doc)),
            Doc::Group(doc) => {
                let flat = flat || fits(WIDTH.saturating_sub(column), doc, &stack);
                stack.push((indent, flat, doc));
            }
            Doc::Seq(docs) => stack.extend(docs.iter().rev().map(|doc| (indent, flat, doc))),
        }
    }
    out
}

/// Whether `doc` fits flat in `room` columns, with what follows it on the
/// line, from `rest`, up to its first line break. A block's break ends the
/// line too, so a call ending in a `match` keeps `f(match x {` together.
fn fits(room: usize, doc: &Doc, rest: &[(usize, bool, &Doc)]) -> bool {
    let mut room = room as isize;
    let mut todo = vec![(true, doc)];
    let mut rest = rest.iter().rev();
    loop {
        let (flat, doc) = match todo.pop() {
            Some(next) => next,
            None => match rest.next() {
                Some(&(_, flat, doc)) => (flat, doc),
                None => return true,
            },
        };
        match doc {
            Doc::Text(text) if text.contains('\n') => return !flat,
            Doc::Text(text) => room -= width(text) as isize,
            Doc::IfBroken(text) if !flat => room -= width(text) as isize,
            Doc::Suffix(_) | Doc::IfBroken(_) => {}
            Doc::Line if flat => room -= 1,
            Doc::Soft if flat => {}
            Doc::Line | Doc::Soft | Doc::Hard => return true,
            Doc::Nest(doc) | Doc::Indent(doc) | Doc::Group(doc) => todo.push((flat, doc)),
            Doc::Seq(docs) => todo.extend(docs.iter().rev().map(|doc| (flat, doc))),
        }
        if room < 0 {
            return false;
        }
    }
}

/// `source` formatted, or what stops it: a syntax error, or a comment the
/// layout has no place for.
pub fn format(source: &str) -> Result<String, Vec<ZeruError>> {
    let (program, comments, parens) = parse(source)?;
    let mut lexer = Lexer::at(source, 0);
    let mut tokens = Vec::new();
    loop {
        let (token, span) = lexer.next_token();
        if token == Token::Eof {
            break;
        }
        tokens.push((token, span));
    }
    let mut printer = Printer {
        src: source,
        tokens,
        comments: comments.clone(),
        next: 0,
        parens: parens.into_iter().collect(),
        misplaced: None,
    };
    let doc = printer.file(&program.statements);
    if let Some(span) = printer.misplaced {
        let message = "A comment inside an expression has no place once formatted; move it above the statement";
        return Err(vec![ZeruError::syntax(message, span)]);
    }
    let formatted = render(&doc);
    let same = parse(&formatted).is_ok_and(|(again, again_comments, _)| {
        let texts = |spans: &[Span], text: &str| -> Vec<String> {
            let texts = spans
                .iter()
                .map(|span| text[span.start..span.end].trim_end());
            texts.map(str::to_string).collect()
        };
        shape(&again) == shape(&program)
            && texts(&again_comments, &formatted) == texts(&comments, source)
    });
    if !same {
        let message = "The formatter would change what this file means, so it is left as it is";
        return Err(vec![ZeruError::syntax(message, Span::new(0, 0))]);
    }
    Ok(formatted)
}

fn parse(source: &str) -> Result<(Program, Vec<Span>, Vec<Span>), Vec<ZeruError>> {
    let mut parser = Parser::new(Lexer::at(source, 0));
    let program = parser.parse_program();
    match parser.errors.is_empty() {
        true => Ok((program, parser.comments, parser.parens)),
        false => Err(parser.errors),
    }
}

/// `program` without its spans, to compare two parses of the same code.
fn shape(program: &Program) -> String {
    let debug = format!("{:?}", program.statements);
    let mut out = String::new();
    let mut rest = debug.as_str();
    while let Some(at) = rest.find("span: Span {") {
        out.push_str(&rest[..at]);
        rest = &rest[at..];
        rest = &rest[rest.find('}').map_or(rest.len(), |end| end + 1)..];
    }
    out + rest
}

struct Printer<'s> {
    src: &'s str,
    tokens: Vec<(Token, Span)>,
    comments: Vec<Span>,
    next: usize,
    parens: HashSet<Span>,
    misplaced: Option<Span>,
}

impl<'s> Printer<'s> {
    fn source(&self, span: Span) -> &'s str {
        &self.src[span.start..span.end]
    }

    fn file(&mut self, statements: &[Statement]) -> Doc {
        let mut lines = self.lines(statements, self.src.len(), Self::extent, Self::statement);
        if lines.is_empty() {
            return text("");
        }
        lines.remove(0);
        lines.push(Doc::Hard);
        seq(lines)
    }

    /// Where `stmt` starts and ends, its body included: a function's own span
    /// stops at its signature.
    fn extent(&self, stmt: &Statement) -> Span {
        match stmt.kind {
            S::Function { .. } => Span::new(stmt.span.start, self.braces(stmt.span.end).1.end),
            _ => stmt.span,
        }
    }

    /// The first `{` at or after `from`, and the `}` that closes it.
    fn braces(&self, from: usize) -> (Span, Span) {
        let mut depth = 0;
        let mut open = None;
        for &(ref token, span) in self.tokens.iter().filter(|(_, span)| span.start >= from) {
            match token {
                Token::LBrace => {
                    open.get_or_insert(span);
                    depth += 1;
                }
                Token::RBrace if depth == 1 => return (open.unwrap_or(span), span),
                Token::RBrace => depth -= 1,
                _ => {}
            }
        }
        let end = Span::new(self.src.len(), self.src.len());
        (open.unwrap_or(end), end)
    }

    /// Where each member inside the braces `open` and `close` begins and
    /// ends: it begins at a token, at the braces' own depth, that `begins`
    /// takes, and ends at the last token before the next member that is not
    /// a `,` or `;` between them. Stops at a token `stops` takes.
    fn members(
        &self,
        (open, close): (Span, Span),
        begins: impl Fn(usize) -> bool,
        stops: impl Fn(&Token) -> bool,
    ) -> Vec<Span> {
        let mut spans: Vec<Span> = Vec::new();
        let mut depth = 0;
        for (at, (token, span)) in self.tokens.iter().enumerate() {
            if span.start <= open.start || span.start >= close.start {
                continue;
            }
            if depth == 0 && stops(token) {
                break;
            }
            if depth == 0 && begins(at) {
                spans.push(*span);
            } else if (depth > 0 || !matches!(token, Token::Comma | Token::Semicolon))
                && let Some(last) = spans.last_mut()
            {
                last.end = span.end;
            }
            match token {
                Token::LBrace | Token::LParen | Token::LBracket => depth += 1,
                Token::RBrace | Token::RParen | Token::RBracket => depth -= 1,
                _ => {}
            }
        }
        spans
    }

    /// `items` one per line, each after the comments before it and keeping
    /// a blank line the source has before it, then the comments left before
    /// `end`. Each line starts with its break.
    fn lines<T>(
        &mut self,
        items: &[T],
        end: usize,
        extent: impl Fn(&Self, &T) -> Span,
        print: impl Fn(&mut Self, &T) -> Doc,
    ) -> Vec<Doc> {
        let mut out = Vec::new();
        let mut last = None;
        for item in items {
            let span = extent(self, item);
            self.comments_before(span.start, &mut out, &mut last);
            self.newline(&mut out, last, span.start);
            out.push(print(self, item));
            while let Some(&comment) = self.comments.get(self.next)
                && comment.start < span.end
            {
                self.misplaced.get_or_insert(comment);
                self.next += 1;
            }
            last = Some(span.end);
            if let Some(&comment) = self.comments.get(self.next) {
                let between = &self.src[span.end..comment.start];
                let separators = |c: char| c == ' ' || c == '\t' || c == ',' || c == ';';
                if between.chars().all(separators) {
                    out.push(Doc::Suffix(format!(" {}", self.source(comment).trim_end())));
                    self.next += 1;
                    last = Some(comment.end);
                }
            }
        }
        self.comments_before(end, &mut out, &mut last);
        out
    }

    fn comments_before(&mut self, at: usize, out: &mut Vec<Doc>, last: &mut Option<usize>) {
        while let Some(&comment) = self.comments.get(self.next)
            && comment.start < at
        {
            self.newline(out, *last, comment.start);
            out.push(text(self.source(comment).trim_end()));
            *last = Some(comment.end);
            self.next += 1;
        }
    }

    /// A line break, after a blank line when the source has one between
    /// `last` and `at`.
    fn newline(&self, out: &mut Vec<Doc>, last: Option<usize>, at: usize) {
        let gap = last.and_then(|last| self.src.get(last..at));
        if gap.is_some_and(|gap| gap.matches('\n').count() > 1) {
            out.push(Doc::Hard);
        }
        out.push(Doc::Hard);
    }

    /// `{`, `lines` a level in, `}` on its own line; `{}` when empty.
    fn braced(&self, head: Doc, lines: Vec<Doc>) -> Doc {
        match lines.is_empty() {
            true => seq(vec![head, text("{}")]),
            false => seq(vec![
                head,
                text("{"),
                Doc::Indent(Box::new(seq(lines))),
                Doc::Hard,
                text("}"),
            ]),
        }
    }

    fn block(&mut self, body: &[Statement], close: usize) -> Doc {
        let lines = self.lines(body, close, Self::extent, Self::statement);
        self.braced(text(""), lines)
    }

    fn statement(&mut self, stmt: &Statement) -> Doc {
        let public = if stmt.is_pub { "pub " } else { "" };
        match &stmt.kind {
            S::Var {
                name,
                is_const,
                value,
                type_annotation,
            } => {
                let keyword = if *is_const { "const" } else { "var" };
                let ty = type_annotation
                    .as_ref()
                    .map_or(String::new(), |ty| format!(": {}", ty_text(ty)));
                let head = text(format!("{public}{keyword} {name}{ty} = "));
                seq(vec![head, self.expr(value), text(";")])
            }
            S::Return(None) => text("return;"),
            S::Return(Some(value)) => seq(vec![text("return "), self.expr(value), text(";")]),
            S::Break => text("break;"),
            S::Continue => text("continue;"),
            S::While { cond, body } => seq(vec![
                text("while "),
                self.expr(cond),
                text(" "),
                self.statement(body),
            ]),
            S::ForIn {
                index,
                variable,
                iterable,
                body,
            } => seq(vec![
                text(match index {
                    Some(index) => format!("for {index}, {variable} in "),
                    None => format!("for {variable} in "),
                }),
                self.expr(iterable),
                text(" "),
                self.statement(body),
            ]),
            S::Expression(expr) if matches!(expr.kind, E::Match { .. }) => self.expr(expr),
            S::Expression(expr) => seq(vec![self.expr(expr), text(";")]),
            S::Block(body) => self.block(body, stmt.span.end - 1),
            S::Function {
                name,
                type_params,
                params,
                return_type,
                body,
            } => {
                let close = self.braces(stmt.span.end).1;
                let params = params.iter().map(|param| text(param_text(param))).collect();
                let ret = return_type
                    .as_ref()
                    .map_or(String::new(), |ty| format!(" {}", ty_text(ty)));
                seq(vec![
                    text(format!("{public}fn {name}{}", generics(type_params))),
                    list("(", params, ")", false),
                    text(format!("{ret} ")),
                    self.block(body, close.start),
                ])
            }
            S::Struct {
                name,
                type_params,
                fields,
                methods,
            } => {
                let braces = self.braces(stmt.span.start);
                let field = |at: usize| {
                    matches!(self.tokens[at].0, Token::Identifier(_))
                        && matches!(self.tokens.get(at + 1), Some((Token::Colon, _)))
                };
                let spans = self.members(braces, field, |t| matches!(t, Token::Fn | Token::Pub));
                let fields = spans
                    .into_iter()
                    .zip(fields)
                    .map(|(span, field)| (span, Ok(field)));
                let methods = methods.iter().map(|m| (self.extent(m), Err(m)));
                let members: Vec<_> = fields.chain(methods).collect();
                let lines = self.lines(
                    &members,
                    braces.1.start,
                    |_, member| member.0,
                    |p, member| match member.1 {
                        Ok((name, ty)) => text(format!("{name}: {},", ty_text(ty))),
                        Err(method) => p.statement(method),
                    },
                );
                let head = format!("{public}struct {name}{} ", generics(type_params));
                self.braced(text(head), lines)
            }
            S::Enum { name, variants } => {
                let braces = self.braces(stmt.span.start);
                let variant = |at: usize| matches!(self.tokens[at].0, Token::Identifier(_));
                let spans = self.members(braces, variant, |_| false);
                let variants: Vec<_> = spans.into_iter().zip(variants).collect();
                let lines = self.lines(
                    &variants,
                    braces.1.start,
                    |_, variant| variant.0,
                    |_, (_, (name, fields))| match fields.is_empty() {
                        true => text(format!("{name},")),
                        false => text(format!("{name}({}),", types(fields))),
                    },
                );
                self.braced(text(format!("{public}enum {name} ")), lines)
            }
            S::Trait { name, methods } => {
                let braces = self.braces(stmt.span.start);
                let method = |at: usize| self.tokens[at].0 == Token::Fn;
                let spans = self.members(braces, method, |_| false);
                let methods: Vec<_> = spans.into_iter().zip(methods).collect();
                let lines = self.lines(
                    &methods,
                    braces.1.start,
                    |_, method| method.0,
                    |_, (_, method)| {
                        let params = method.params.iter().map(|p| text(param_text(p))).collect();
                        let ret = method
                            .return_type
                            .as_ref()
                            .map_or(String::new(), |ty| format!(" {}", ty_text(ty)));
                        seq(vec![
                            text(format!("fn {}", method.name)),
                            list("(", params, ")", false),
                            text(format!("{ret};")),
                        ])
                    },
                );
                self.braced(text(format!("{public}trait {name} ")), lines)
            }
            S::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let mut parts = vec![
                    text("if "),
                    self.expr(condition),
                    text(" "),
                    self.statement(then_branch),
                ];
                if let Some(else_branch) = else_branch {
                    parts.extend([text(" else "), self.statement(else_branch)]);
                }
                seq(parts)
            }
            S::Import { path, symbols } => {
                let path = path.join(".");
                match symbols {
                    None => text(format!("import {path};")),
                    Some(symbols) => seq(vec![
                        text(format!("import {path}::")),
                        list("{", symbols.iter().map(text).collect(), "}", false),
                        text(";"),
                    ]),
                }
            }
        }
    }

    /// `expr`, in the parentheses the source wrote around it.
    fn expr(&mut self, expr: &Expression) -> Doc {
        let doc = self.bare(expr);
        match self.parens.contains(&expr.span) {
            true => seq(vec![text("("), doc, text(")")]),
            false => doc,
        }
    }

    fn bare(&mut self, expr: &Expression) -> Doc {
        match &expr.kind {
            E::Int(_) | E::Float(_) | E::StringLit(_) | E::InlineAsm { .. } => {
                text(self.source(expr.span))
            }
            E::Boolean(value) => text(value.to_string()),
            E::None => text("None"),
            E::Identifier(name) => text(name),
            E::Prefix { operator, right } => {
                seq(vec![text(operator.to_string()), self.expr(right)])
            }
            E::Infix {
                left,
                operator,
                right,
            } => {
                let right = seq(vec![
                    Doc::Line,
                    text(format!("{operator} ")),
                    self.expr(right),
                ]);
                group(seq(vec![self.expr(left), nest(right)]))
            }
            E::StructLiteral { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|(field, value)| seq(vec![text(format!("{field}: ")), self.expr(value)]))
                    .collect();
                seq(vec![text(format!("{name} ")), list("{", fields, "}", true)])
            }
            E::Call { .. } | E::Get { .. } | E::Index { .. } | E::Try { .. } => self.chain(expr),
            E::ArrayLiteral(items) => {
                let items = items.iter().map(|item| self.expr(item)).collect();
                list("[", items, "]", false)
            }
            E::Range { start, end } => seq(vec![self.expr(start), text(".."), self.expr(end)]),
            E::ArrayRepeat { value, count } => seq(vec![
                text("["),
                self.expr(value),
                text(format!("; {count}]")),
            ]),
            E::Assign {
                target,
                operator,
                value,
            } => seq(vec![
                self.expr(target),
                text(format!(" {operator} ")),
                self.expr(value),
            ]),
            E::Cast { left, target } => seq(vec![
                self.expr(left),
                text(format!(" as {}", ty_text(target))),
            ]),
            E::Match { value, arms } => {
                let head = seq(vec![text("match "), self.expr(value), text(" ")]);
                let lines = self.lines(
                    arms,
                    expr.span.end - 1,
                    |_, (pattern, body)| Span::new(pattern.span.start, body.span.end),
                    |p, (pattern, body)| {
                        let comma = if matches!(body.kind, E::Block(_)) {
                            ""
                        } else {
                            ","
                        };
                        seq(vec![
                            p.pattern(pattern),
                            text(" => "),
                            p.expr(body),
                            text(comma),
                        ])
                    },
                );
                self.braced(head, lines)
            }
            E::BorrowRef(inner) => {
                let borrows = matches!(inner.kind, E::BorrowRef(_) | E::BorrowRefMut(_));
                let space = if borrows && !self.parens.contains(&inner.span) {
                    " "
                } else {
                    ""
                };
                seq(vec![text(format!("&{space}")), self.expr(inner)])
            }
            E::BorrowRefMut(inner) => seq(vec![text("&var "), self.expr(inner)]),
            E::Dereference(inner) => seq(vec![text("*"), self.expr(inner)]),
            E::Block(body) => self.block(body, expr.span.end - 1),
            E::Tuple(items) if items.len() == 1 => {
                seq(vec![text("("), self.expr(&items[0]), text(",)")])
            }
            E::Tuple(items) => {
                let items = items.iter().map(|item| self.expr(item)).collect();
                list("(", items, ")", false)
            }
        }
    }

    /// Calls, fields, indexing and `?`/`!` after a root. With two `.`s or
    /// more and a method call among them, it breaks before each `.` when it
    /// does not fit.
    fn chain(&mut self, expr: &Expression) -> Doc {
        let mut links = Vec::new();
        let mut root = expr;
        loop {
            let inner = match &root.kind {
                E::Call { function, .. } => function,
                E::Get { object, .. } => object,
                E::Index { left, .. } => left,
                E::Try { value, .. } => value,
                _ => break,
            };
            links.push(root);
            root = inner;
            if self.parens.contains(&root.span) {
                break;
            }
        }
        let dots = links
            .iter()
            .filter(|link| matches!(link.kind, E::Get { .. }));
        let method = |link: &&Expression| matches!(&link.kind, E::Call { function, .. } if matches!(function.kind, E::Get { .. }));
        let breaks = dots.count() >= 2 && links.iter().any(method);
        let mut parts = Vec::new();
        for link in links.into_iter().rev() {
            parts.push(match &link.kind {
                E::Get { name, .. } if breaks => seq(vec![Doc::Soft, text(format!(".{name}"))]),
                E::Get { name, .. } => text(format!(".{name}")),
                E::Call {
                    type_args,
                    arguments,
                    ..
                } => {
                    let args = arguments.iter().map(|arg| self.expr(arg)).collect();
                    let type_args = match type_args.is_empty() {
                        true => String::new(),
                        false => format!("<{}>", types(type_args)),
                    };
                    seq(vec![text(type_args), list("(", args, ")", false)])
                }
                E::Index { index, .. } => seq(vec![text("["), self.expr(index), text("]")]),
                E::Try { operator, .. } => text(operator.to_string()),
                _ => unreachable!(),
            });
        }
        let root = self.expr(root);
        match breaks {
            true => group(seq(vec![root, nest(seq(parts))])),
            false => seq(std::iter::once(root).chain(parts).collect()),
        }
    }

    fn pattern(&mut self, pattern: &Pattern) -> Doc {
        match &pattern.kind {
            PatternKind::Default => text("default"),
            PatternKind::Constant(expr) => self.expr(expr),
            PatternKind::Binding(name) => text(name),
            PatternKind::Payload { path, fields } => {
                let mut parts = vec![text(format!("{path}("))];
                for (at, field) in fields.iter().enumerate() {
                    if at > 0 {
                        parts.push(text(", "));
                    }
                    parts.push(self.pattern(field));
                }
                parts.push(text(")"));
                seq(parts)
            }
            PatternKind::Or(alternatives) => {
                let mut parts = Vec::new();
                for (at, alternative) in alternatives.iter().enumerate() {
                    if at > 0 {
                        parts.extend([Doc::Line, text("| ")]);
                    }
                    parts.push(self.pattern(alternative));
                }
                group(seq(parts))
            }
        }
    }
}

fn ty_text(ty: &TypeSpec) -> String {
    match ty {
        TypeSpec::Named(name) => name.clone(),
        TypeSpec::Generic { name, args } => format!("{name}<{}>", types(args)),
        TypeSpec::IntLiteral(value) => value.to_string(),
        TypeSpec::Tuple(parts) if parts.len() == 1 => format!("({},)", ty_text(&parts[0])),
        TypeSpec::Tuple(parts) => format!("({})", types(parts)),
        TypeSpec::Pointer(inner) => format!("*{}", ty_text(inner)),
        TypeSpec::Optional(inner) => format!("{}?", suffixed(inner)),
        TypeSpec::Result(inner) => format!("{}!", suffixed(inner)),
        TypeSpec::Slice(inner) if **inner == TypeSpec::Named("u8".into()) => "str".into(),
        TypeSpec::Slice(inner) => format!("&[{}]", ty_text(inner)),
        TypeSpec::Ref(inner) => match ty_text(inner) {
            inner if inner.starts_with('&') => format!("& {inner}"),
            inner => format!("&{inner}"),
        },
        TypeSpec::RefMut(inner) => format!("&var {}", ty_text(inner)),
    }
}

/// `ty` before a `?` or `!`, in parentheses when the suffix would otherwise
/// bind to what it points at, as in `(&T)?`.
fn suffixed(ty: &TypeSpec) -> String {
    match ty {
        TypeSpec::Pointer(_)
        | TypeSpec::Ref(_)
        | TypeSpec::RefMut(_)
        | TypeSpec::Optional(_)
        | TypeSpec::Result(_) => format!("({})", ty_text(ty)),
        _ => ty_text(ty),
    }
}

fn types(types: &[TypeSpec]) -> String {
    types.iter().map(ty_text).collect::<Vec<_>>().join(", ")
}

fn generics(params: &[TypeParameter]) -> String {
    if params.is_empty() {
        return String::new();
    }
    let params: Vec<String> = params
        .iter()
        .map(|param| match &param.bound {
            Some(bound) => format!("{}: {bound}", param.name),
            None => param.name.clone(),
        })
        .collect();
    format!("<{}>", params.join(", "))
}

fn param_text((name, ty, is_mut): &(String, TypeSpec, bool)) -> String {
    let var = if *is_mut { "var " } else { "" };
    match ty {
        TypeSpec::Named(named) if name == "self" && named == "self" => format!("{var}self"),
        _ => format!("{var}{name}: {}", ty_text(ty)),
    }
}

#[cfg(test)]
mod tests {
    use super::format;

    #[test]
    fn lays_out_keeping_comments() {
        let source = "import std.fs;
// Points.
struct Point { x: i32,   // across
  y: i32, fn sum(self) i32 { return self.x+self.y; } }
enum Shape { Circle(f64), Rect(f64, f64) }


fn main() {
    var p = Point{x:1,y:2};   // made here
    if p.sum()>2 { println(\"{}\", (p.x + 1) * 2); }
    var long = some_function(first_argument_value, second_argument_value, third_argument_value, 4444);
    var chained = p.first_link().second_link().third_link_of_the_chain().fourth_link_in_it().done_now();
    p.items.push(match p.x { 1 | 2 => 3, default => { exit(4); } });
    // the end
}
";
        let expected = "import std.fs;
// Points.
struct Point {
    x: i32, // across
    y: i32,
    fn sum(self) i32 {
        return self.x + self.y;
    }
}
enum Shape {
    Circle(f64),
    Rect(f64, f64),
}

fn main() {
    var p = Point { x: 1, y: 2 }; // made here
    if p.sum() > 2 {
        println(\"{}\", (p.x + 1) * 2);
    }
    var long = some_function(
        first_argument_value,
        second_argument_value,
        third_argument_value,
        4444,
    );
    var chained = p
        .first_link()
        .second_link()
        .third_link_of_the_chain()
        .fourth_link_in_it()
        .done_now();
    p.items.push(match p.x {
        1 | 2 => 3,
        default => {
            exit(4);
        }
    });
    // the end
}
";
        assert_eq!(format(source).unwrap(), expected);
        assert!(format("fn main() { f(1, /* here */ 2); }").is_err());
    }

    #[test]
    fn std_and_examples_format_stably() {
        let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
        let mut dirs = vec![root.join("std"), root.join("examples")];
        while let Some(dir) = dirs.pop() {
            for path in std::fs::read_dir(dir)
                .unwrap()
                .map(|entry| entry.unwrap().path())
            {
                if path.is_dir() {
                    dirs.push(path);
                } else if path.extension().is_some_and(|ext| ext == "zr") {
                    let formatted = format(&std::fs::read_to_string(&path).unwrap()).unwrap();
                    assert_eq!(format(&formatted).unwrap(), formatted, "{}", path.display());
                }
            }
        }
    }
}
