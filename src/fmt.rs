//! `zeru fmt`: a file laid out again from its syntax tree.
//! Indents are four spaces, and a line that would pass 100 columns breaks at
//! its outermost group first: a call's arguments one per line, an operator
//! chain before each operator, a method chain before each `.`; a block may
//! end a line only as a list's last item. Comments, after a statement or a
//! `{` too, and single blank lines stay where they were; a list holding a
//! comment breaks one item per line, and an operator chain holding one
//! breaks before each operator, but for a `/* */` on one line, which stays
//! beside its operator. A comment elsewhere inside an expression,
//! which has no place in the new layout, is an error instead.
//! The result is parsed again and must give the same tree and comments, or
//! nothing changes.

use std::collections::HashSet;

use crate::ast::{
    ClosureBody, Expression, ExpressionKind as E, Names, Pattern, PatternKind, Program, Statement,
    StatementKind as S, TypeParameter, TypeSpec,
};
use crate::errors::{Span, ZeruError};
use crate::lexer::Lexer;
use crate::parser::{Parser, token_precedence};
use crate::token::Token;

const WIDTH: usize = 100;
const INDENT: usize = 4;

/// A layout. `Line` is a space and `Soft`
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
/// trailing comma. Only the last item may be a block that spans lines
/// while the list stays on one: one before it would put the items after it
/// on the block's last line.
fn list(open: &str, items: Vec<Doc>, close: &str, padded: bool) -> Doc {
    if items.is_empty() {
        return text(format!("{open}{close}"));
    }
    if items[..items.len() - 1].iter().any(breaks) {
        let lines = items
            .into_iter()
            .flat_map(|item| [Doc::Hard, item, text(",")])
            .collect();
        return broken(open, lines, close);
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

/// `lines` a level in, with `open` and `close` on lines of their own.
fn broken(open: &str, lines: Vec<Doc>, close: &str) -> Doc {
    seq(vec![
        text(open),
        Doc::Indent(Box::new(seq(lines))),
        Doc::Hard,
        text(close),
    ])
}

/// Whether `doc` always spans lines, as a block does.
fn breaks(doc: &Doc) -> bool {
    match doc {
        Doc::Hard => true,
        Doc::Text(text) => text.contains('\n'),
        Doc::Nest(doc) | Doc::Indent(doc) | Doc::Group(doc) => breaks(doc),
        Doc::Seq(docs) => docs.iter().any(breaks),
        Doc::Suffix(_) | Doc::Line | Doc::Soft | Doc::IfBroken(_) => false,
    }
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
        *column = match text.rsplit_once('\n') {
            Some((_, last)) => width(last),
            None => *column + width(text),
        };
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
            Doc::Text(text) if text.contains('\n') => {
                let first = text.split('\n').next().unwrap_or_default();
                return room >= width(first) as isize;
            }
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
    let mut printer = Printer {
        src: source,
        tokens: Lexer::at(source, 0).tokens(),
        comments: comments.clone(),
        next: 0,
        parens: parens.into_iter().collect(),
        misplaced: None,
    };
    let doc = printer.file(&program);
    if let Some(span) = printer.misplaced {
        let message = "A comment inside an expression has no place once formatted; move it above the statement";
        return Err(vec![ZeruError::syntax(message, span)]);
    }
    let formatted = render(&doc);
    let same = parse(&formatted).is_ok_and(|(again, again_comments, _)| {
        let texts = |spans: &[Span], text: &str| -> Vec<String> {
            spans
                .iter()
                .map(|span| text[span.start..span.end].trim_end().to_string())
                .collect()
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
    let debug = format!("{:?}{:?}", program.statements, program.tests);
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

    fn file(&mut self, program: &Program) -> Doc {
        let tests = self.tokens.iter().find(|(token, _)| *token == Token::Tests);
        let tests = tests.map(|&(_, span)| span);
        let end = tests.map_or(self.src.len(), |span| span.start);
        let mut lines = self.lines(&program.statements, end, Self::extent, Self::statement);
        if let Some(span) = tests {
            let close = self.brackets(span.end, Token::LBrace).1.start;
            let items: Vec<&Statement> = program
                .tests
                .iter()
                .filter(
                    |stmt| !matches!(&stmt.kind, S::Function { name, .. } if name.starts_with('#')),
                )
                .collect();
            let inner = self.lines(
                &items,
                close,
                |p, stmt| p.extent(stmt),
                |p, stmt| p.statement(stmt),
            );
            if !lines.is_empty() {
                lines.push(Doc::Hard);
            }
            lines.push(Doc::Hard);
            lines.push(self.braced(text("#test "), inner));
        }
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
            S::Function { .. } => Span::new(
                stmt.span.start,
                self.brackets(stmt.span.end, Token::LBrace).1.end,
            ),
            _ => stmt.span,
        }
    }

    /// The first `open` at or after `from`, and the bracket that closes it.
    fn brackets(&self, from: usize, open: Token) -> (Span, Span) {
        let mut depth = 0;
        let mut first = None;
        for &(ref token, span) in self.tokens.iter().filter(|(_, span)| span.start >= from) {
            if first.is_none() && *token != open {
                continue;
            }
            match token {
                Token::LBrace | Token::LParen | Token::LBracket => {
                    first.get_or_insert(span);
                    depth += 1;
                }
                Token::RBrace | Token::RParen | Token::RBracket if depth == 1 => {
                    return (first.unwrap_or(span), span);
                }
                Token::RBrace | Token::RParen | Token::RBracket => depth -= 1,
                _ => {}
            }
        }
        let end = Span::new(self.src.len(), self.src.len());
        (first.unwrap_or(end), end)
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
        if let Some(&comment) = self.comments.get(self.next)
            && items
                .first()
                .is_none_or(|item| comment.start < extent(self, item).start)
            && self.src[..comment.start]
                .trim_end_matches([' ', '\t'])
                .ends_with('{')
        {
            out.push(Doc::Suffix(format!(" {}", self.source(comment).trim_end())));
            self.next += 1;
            last = Some(comment.end);
        }
        for item in items {
            let span = extent(self, item);
            let before = self.next;
            self.comments_before(span.start, &mut out, &mut last);
            let gap = last.map_or("\n", |last| &self.src[last..span.start]);
            match self.next > before && gap.chars().all(|c| c == ' ' || c == '\t') {
                true => out.push(text(" ")),
                false => self.newline(&mut out, last, span.start),
            }
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

    /// The comments before `at`: the first at the end of the line, the
    /// others on lines of their own.
    fn trailing(&mut self, at: usize) -> Vec<Doc> {
        let mut docs = Vec::new();
        while let Some(&comment) = self.comments.get(self.next)
            && comment.start < at
        {
            let comment = self.source(comment).trim_end();
            match docs.is_empty() {
                true => docs.push(Doc::Suffix(format!(" {comment}"))),
                false => docs.extend([Doc::Hard, text(comment)]),
            }
            self.next += 1;
        }
        docs
    }

    /// `items` as `list` lays them out, or one per line, each with its
    /// comments, when a comment sits between them inside `within`.
    fn items<T>(
        &mut self,
        (open, close): (&str, &str),
        items: &[T],
        within: Span,
        padded: bool,
        span: impl Fn(&T) -> Span,
        print: impl Fn(&mut Self, &T) -> Doc,
    ) -> Doc {
        let inside = |at: usize| {
            items
                .iter()
                .any(|item| span(item).start <= at && at < span(item).end)
        };
        let between = self.comments[self.next..]
            .iter()
            .take_while(|comment| comment.start < within.end)
            .any(|comment| comment.start > within.start && !inside(comment.start));
        if !between {
            let docs = items.iter().map(|item| print(self, item)).collect();
            return list(open, docs, close, padded);
        }
        let lines = self.lines(
            items,
            within.end,
            |_, item| span(item),
            |p, item| seq(vec![print(p, item), text(",")]),
        );
        broken(open, lines, close)
    }

    /// `head(params) ret`: how a function and a trait method begin, the
    /// parameters in the first parentheses from `from`.
    fn signature(
        &mut self,
        head: Doc,
        from: usize,
        params: &[(String, TypeSpec, bool)],
        ret: &Option<TypeSpec>,
    ) -> Vec<Doc> {
        let parens = self.brackets(from, Token::LParen);
        let begins = |at: usize| matches!(self.tokens[at - 1].0, Token::LParen | Token::Comma);
        let spans = self.members(parens, begins, |_| false);
        let params: Vec<_> = spans.into_iter().zip(params).collect();
        let within = Span::new(parens.0.start, parens.1.end);
        let params = self.items(
            ("(", ")"),
            &params,
            within,
            false,
            |param| param.0,
            |_, param| text(param_text(param.1)),
        );
        vec![head, params, text(return_text(ret))]
    }

    /// `cond` and the space before its block, or a line break when `cond`
    /// is an operator or method chain that breaks, so the `{` stands apart
    /// from the chain's last line.
    fn condition(&mut self, cond: &Expression) -> Doc {
        let list = matches!(
            cond.kind,
            E::ArrayLiteral(_) | E::VecLiteral(_) | E::Tuple(_)
        );
        match self.expr(cond) {
            Doc::Group(doc) if !list => group(seq(vec![*doc, Doc::Line])),
            doc => seq(vec![doc, text(" ")]),
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
            false => seq(vec![head, broken("{", lines, "}")]),
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
            S::VarTuple {
                names,
                is_const,
                value,
            } => {
                let keyword = if *is_const { "const" } else { "var" };
                let names = names_text(&Names::Tuple(names.clone()));
                let head = text(format!("{public}{keyword} {names} = "));
                seq(vec![head, self.expr(value), text(";")])
            }
            S::Return(None) => text("return;"),
            S::Return(Some(value)) => seq(vec![text("return "), self.expr(value), text(";")]),
            S::Break => text("break;"),
            S::Continue => text("continue;"),
            S::While { cond, body } => seq(vec![
                text("while "),
                self.condition(cond),
                self.statement(body),
            ]),
            S::ForIn {
                index,
                variable,
                iterable,
                body,
            } => seq(vec![
                text(match index {
                    Some(index) => format!("for {index}, {} in ", names_text(variable)),
                    None => format!("for {} in ", names_text(variable)),
                }),
                self.condition(iterable),
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
                let close = self.brackets(stmt.span.end, Token::LBrace).1;
                if name.starts_with('!') {
                    let at = self.tokens.iter().find(|(token, span)| {
                        span.start > stmt.span.start && matches!(token, Token::StringLit(_))
                    });
                    let at = at.expect("a test has a name").1;
                    let head = text(format!("test({}) ", self.source(at)));
                    return seq(vec![head, self.block(body, close.start)]);
                }
                let head = seq(vec![
                    text(format!("{public}fn {name}")),
                    generics(type_params),
                ]);
                let mut parts = self.signature(head, stmt.span.start, params, return_type);
                parts.extend([text(" "), self.block(body, close.start)]);
                seq(parts)
            }
            S::Struct {
                name,
                type_params,
                fields,
                methods,
            } => {
                let braces = self.brackets(stmt.span.start, Token::LBrace);
                let field = |at: usize| {
                    matches!(self.tokens[at].0, Token::Identifier(_))
                        && matches!(self.tokens.get(at + 1), Some((Token::Colon, _)))
                };
                let spans = self.members(braces, field, |t| matches!(t, Token::Fn | Token::Pub));
                let fields: Vec<_> = spans.into_iter().zip(fields).collect();
                let end = match methods.is_empty() {
                    true => braces.1.start,
                    false => fields.last().map_or(braces.0.end, |(span, _)| span.end),
                };
                let mut lines = self.lines(
                    &fields,
                    end,
                    |_, field| field.0,
                    |_, (_, (name, ty))| text(format!("{name}: {},", ty_text(ty))),
                );
                if !fields.is_empty() && !methods.is_empty() {
                    lines.push(Doc::Hard);
                }
                lines.extend(self.lines(methods, braces.1.start, Self::extent, Self::statement));
                let head = text(format!("{public}struct {name}"));
                self.braced(seq(vec![head, generics(type_params), text(" ")]), lines)
            }
            S::Enum { name, variants } => {
                let braces = self.brackets(stmt.span.start, Token::LBrace);
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
                let braces = self.brackets(stmt.span.start, Token::LBrace);
                let method = |at: usize| self.tokens[at].0 == Token::Fn;
                let spans = self.members(braces, method, |_| false);
                let methods: Vec<_> = spans.into_iter().zip(methods).collect();
                let lines = self.lines(
                    &methods,
                    braces.1.start,
                    |_, method| method.0,
                    |p, (span, method)| {
                        let head = format!("fn {}", method.name);
                        let (params, ret) = (&method.params, &method.return_type);
                        let mut parts = p.signature(text(head), span.start, params, ret);
                        parts.push(text(";"));
                        seq(parts)
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
                    self.condition(condition),
                    self.statement(then_branch),
                ];
                if let Some(else_branch) = else_branch {
                    let comments = self.trailing(else_branch.span.start);
                    let gap = match comments.is_empty() {
                        true => text(" "),
                        false => Doc::Hard,
                    };
                    parts.extend(comments);
                    parts.extend([gap, text("else "), self.statement(else_branch)]);
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
                let doubled = matches!(&right.kind, E::Prefix { operator: inner, .. } if inner == operator)
                    && *operator == Token::Minus
                    && !self.parens.contains(&right.span);
                let space = if doubled { " " } else { "" };
                seq(vec![text(format!("{operator}{space}")), self.expr(right)])
            }
            E::Infix { .. } => {
                let mut rest = Vec::new();
                let mut first = expr;
                while let E::Infix {
                    left,
                    operator,
                    right,
                } = &first.kind
                {
                    rest.push((operator, right));
                    first = left;
                    let chained = matches!(&left.kind, E::Infix { operator: inner, .. }
                        if token_precedence(inner) == token_precedence(operator));
                    if !chained || self.parens.contains(&left.span) {
                        break;
                    }
                }
                let first = self.expr(first);
                let mut parts = Vec::new();
                let mut commented = false;
                for (operator, right) in rest.into_iter().rev() {
                    let (mut before, mut after) = (Vec::new(), Vec::new());
                    while let Some(&comment) = self.comments.get(self.next)
                        && comment.start < right.span.start
                    {
                        let written = self.source(comment).trim_end();
                        if !written.starts_with("/*") || written.contains('\n') {
                            break;
                        }
                        let operator = operator.to_string();
                        match self.src[comment.end..right.span.start].contains(&operator) {
                            true => before.push(text(format!(" {written}"))),
                            false => after.push(text(format!("{written} "))),
                        }
                        self.next += 1;
                    }
                    let comments = self.trailing(right.span.start);
                    commented |= !comments.is_empty();
                    parts.push((before, comments, operator, after, self.expr(right)));
                }
                let parts =
                    parts
                        .into_iter()
                        .flat_map(|(before, comments, operator, after, right)| {
                            let line = if commented { Doc::Hard } else { Doc::Line };
                            before
                                .into_iter()
                                .chain(comments)
                                .chain([line, text(format!("{operator} "))])
                                .chain(after)
                                .chain([right])
                        });
                match commented {
                    true => seq(vec![first, Doc::Indent(Box::new(seq(parts.collect())))]),
                    false => group(seq(vec![first, nest(seq(parts.collect()))])),
                }
            }
            E::StructLiteral { name, fields } => {
                let fields = self.items(
                    ("{", "}"),
                    fields,
                    expr.span,
                    true,
                    |(_, value)| value.span,
                    |p, (field, value)| seq(vec![text(format!("{field}: ")), p.expr(value)]),
                );
                seq(vec![text(format!("{name} ")), fields])
            }
            E::Call { .. } | E::Get { .. } | E::Index { .. } | E::Slice { .. } | E::Try { .. } => {
                self.chain(expr)
            }
            E::ArrayLiteral(items) | E::VecLiteral(items) => {
                let open = if matches!(expr.kind, E::VecLiteral(_)) {
                    "Vec["
                } else {
                    "["
                };
                let span = |item: &Expression| item.span;
                self.items((open, "]"), items, expr.span, false, span, Self::expr)
            }
            E::ArrayRepeat { value, count } | E::VecRepeat { value, count } => {
                let open = if matches!(expr.kind, E::VecRepeat { .. }) {
                    "Vec["
                } else {
                    "["
                };
                seq(vec![
                    text(open),
                    self.expr(value),
                    text("; "),
                    self.expr(count),
                    text("]"),
                ])
            }
            E::Range {
                start,
                end,
                inclusive,
            } => {
                let dots = if *inclusive { "..=" } else { ".." };
                seq(vec![self.expr(start), text(dots), self.expr(end)])
            }
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
            E::If {
                condition,
                then,
                otherwise,
            } => {
                let condition = self.expr(condition);
                let mut branch = |branch: &Expression| match branch.kind {
                    E::If { .. } | E::Block(_) => self.expr(branch),
                    _ => seq(vec![text("{ "), self.expr(branch), text(" }")]),
                };
                let (then, otherwise) = (branch(then), branch(otherwise));
                seq(vec![
                    text("if "),
                    condition,
                    text(" "),
                    then,
                    text(" else "),
                    otherwise,
                ])
            }
            E::Match { value, arms } => {
                let head = seq(vec![text("match "), self.expr(value), text(" ")]);
                let lines = self.lines(
                    arms,
                    expr.span.end - 1,
                    |_, arm| Span::new(arm.pattern.span.start, arm.body.span.end),
                    |p, arm| {
                        let comma = if matches!(arm.body.kind, E::Block(_)) {
                            ""
                        } else {
                            ","
                        };
                        let mut parts = vec![p.pattern(&arm.pattern)];
                        if let Some(guard) = &arm.guard {
                            parts.extend([text(" if "), p.expr(guard)]);
                        }
                        parts.extend([text(" => "), p.expr(&arm.body), text(comma)]);
                        seq(parts)
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
                let span = |item: &Expression| item.span;
                self.items(("(", ")"), items, expr.span, false, span, Self::expr)
            }
            E::Closure(closure) => {
                let params = closure
                    .params
                    .iter()
                    .map(|(name, ty, mutable)| {
                        let var = if *mutable { "var " } else { "" };
                        text(match ty {
                            Some(ty) => format!("{var}{name}: {}", ty_text(ty)),
                            None => format!("{var}{name}"),
                        })
                    })
                    .collect();
                let mut parts = vec![text("fn"), list("(", params, ")", false)];
                match &closure.body {
                    ClosureBody::Expression(body) => parts.extend([text(" "), self.expr(body)]),
                    ClosureBody::Block(body) => {
                        let ret = return_text(&closure.return_type);
                        parts
                            .extend([text(format!("{ret} ")), self.block(body, expr.span.end - 1)]);
                    }
                }
                seq(parts)
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
                E::Index { left, .. } | E::Slice { left, .. } => left,
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
        let root = self.expr(root);
        let mut parts = Vec::new();
        for link in links.into_iter().rev() {
            parts.push(match &link.kind {
                E::Get { name, .. } if breaks => seq(vec![Doc::Soft, text(format!(".{name}"))]),
                E::Get { name, .. } => text(format!(".{name}")),
                E::Call {
                    function,
                    type_args,
                    arguments,
                } => {
                    let within = Span::new(function.span.end, link.span.end);
                    let span = |arg: &Expression| arg.span;
                    let args = self.items(("(", ")"), arguments, within, false, span, Self::expr);
                    let type_args = match type_args.is_empty() {
                        true => String::new(),
                        false => format!("<{}>", types(type_args)),
                    };
                    seq(vec![text(type_args), args])
                }
                E::Index { index, .. } => seq(vec![text("["), self.expr(index), text("]")]),
                E::Slice { start, end, .. } => {
                    let mut parts = vec![text("[")];
                    parts.extend(start.iter().map(|start| self.expr(start)));
                    parts.push(text(".."));
                    parts.extend(end.iter().map(|end| self.expr(end)));
                    parts.push(text("]"));
                    seq(parts)
                }
                E::Try { operator, .. } => text(operator.to_string()),
                _ => unreachable!(),
            });
        }
        match breaks {
            true => group(seq(vec![root, nest(seq(parts))])),
            false => seq(std::iter::once(root).chain(parts).collect()),
        }
    }

    fn pattern(&mut self, pattern: &Pattern) -> Doc {
        match &pattern.kind {
            PatternKind::Default => text("default"),
            PatternKind::Constant(expr) => self.expr(expr),
            PatternKind::Range(low, high) => {
                seq(vec![self.expr(low), text("..="), self.expr(high)])
            }
            PatternKind::Binding(name) => text(name),
            PatternKind::Payload { fields, .. } | PatternKind::Tuple(fields) => {
                let path = match &pattern.kind {
                    PatternKind::Payload { path, .. } => path.as_str(),
                    _ => "",
                };
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

/// A return type after a space, or nothing.
fn return_text(ty: &Option<TypeSpec>) -> String {
    match ty {
        Some(ty) => format!(" {}", ty_text(ty)),
        None => String::new(),
    }
}

fn names_text(names: &Names) -> String {
    match names {
        Names::One(name, None) => name.clone(),
        Names::One(name, Some(ty)) => format!("{name}: {}", ty_text(ty)),
        Names::Skip => "_".to_string(),
        Names::Tuple(names) => {
            let parts: Vec<String> = names.iter().map(names_text).collect();
            let comma = if parts.len() == 1 { "," } else { "" };
            format!("({}{comma})", parts.join(", "))
        }
    }
}

pub(crate) fn ty_text(ty: &TypeSpec) -> String {
    match ty {
        TypeSpec::Named(name) => name.clone(),
        TypeSpec::Generic { name, args } => format!("{name}<{}>", types(args)),
        TypeSpec::IntLiteral(value) => value.to_string(),
        TypeSpec::Tuple(parts) if parts.len() == 1 => format!("({},)", ty_text(&parts[0])),
        TypeSpec::Tuple(parts) => format!("({})", types(parts)),
        TypeSpec::Pointer(inner) => format!("*{}", ty_text(inner)),
        TypeSpec::Optional(inner) => format!("{}?", suffixed(inner)),
        TypeSpec::Result(inner) if **inner == TypeSpec::Tuple(Vec::new()) => "!".into(),
        TypeSpec::Result(inner) => format!("{}!", suffixed(inner)),
        TypeSpec::Slice(inner) if **inner == TypeSpec::Named("u8".into()) => "str".into(),
        TypeSpec::Slice(inner) => format!("&[{}]", ty_text(inner)),
        TypeSpec::SliceMut(inner) => format!("&var [{}]", ty_text(inner)),
        TypeSpec::Ref(inner) => match ty_text(inner) {
            inner if inner.starts_with('&') => format!("& {inner}"),
            inner => format!("&{inner}"),
        },
        TypeSpec::RefMut(inner) => format!("&var {}", ty_text(inner)),
        TypeSpec::Fn(params, returns) => match returns {
            Some(returns) => format!("fn({}) {}", types(params), ty_text(returns)),
            None => format!("fn({})", types(params)),
        },
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

/// `<A, B: Ord>`, broken one per line as a call's arguments when long.
fn generics(params: &[TypeParameter]) -> Doc {
    if params.is_empty() {
        return text("");
    }
    let params = params
        .iter()
        .map(|param| match &param.bound {
            Some(bound) => text(format!("{}: {bound}", param.name)),
            None => text(&param.name),
        })
        .collect();
    list("<", params, ">", false)
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
        assert!(format("fn main() { x = /* here */ 2; }").is_err());
        let bang = "fn f(r: !, all: Vec<!>) ! {\n    const done: ! = r;\n}\n";
        assert_eq!(format(bang).unwrap(), bang);
    }

    #[test]
    fn keeps_comments_in_lists_operators_and_signatures() {
        let source = "struct P {
    x: i32, // trailing x
    /* y */ y: i32,
    fn get(self) i32 { return self.x; }
}

fn f(
    a: i32, // first
    b: i32,
) i32 {
    return a + /* mid */ b /* left */ - 1;
}

fn pick<Alphabetical: Ord, Betamaxers: Ord, Gammarays: Ord, Deltaforce: Ord, Epsilonics: Ord>() i32 {
    return 0;
}

fn main() {
    const p = P { x: 1, // one
        // before y
        y: 2 };
    println(\"{}\", f(p.x, // arg a
        p.y));
    const v: Vec<i32> = Vec[1, /* a */ 2];
    const sum = 1 + // plus
        2;
    if p.x > 0 {
        f(1, 2);
    } // between
    else {
        f(2, 1);
    }
    if p.first_condition > 100 && p.second_condition > 100 && p.third_condition_with_a_long_name > 1 {
        f(3, 4);
    }
    const total = p.width * p.height * p.depth + p.width * p.height + p.width * p.depth + p.height + 1;
}
";
        let expected = "struct P {
    x: i32, // trailing x
    /* y */ y: i32,

    fn get(self) i32 {
        return self.x;
    }
}

fn f(
    a: i32, // first
    b: i32,
) i32 {
    return a + /* mid */ b /* left */ - 1;
}

fn pick<
    Alphabetical: Ord,
    Betamaxers: Ord,
    Gammarays: Ord,
    Deltaforce: Ord,
    Epsilonics: Ord,
>() i32 {
    return 0;
}

fn main() {
    const p = P {
        x: 1, // one
        // before y
        y: 2,
    };
    println(\"{}\", f(
        p.x, // arg a
        p.y,
    ));
    const v: Vec<i32> = Vec[
        1, /* a */
        2,
    ];
    const sum = 1 // plus
        + 2;
    if p.x > 0 {
        f(1, 2);
    } // between
    else {
        f(2, 1);
    }
    if p.first_condition > 100
        && p.second_condition > 100
        && p.third_condition_with_a_long_name > 1
    {
        f(3, 4);
    }
    const total = p.width * p.height * p.depth
        + p.width * p.height
        + p.width * p.depth
        + p.height
        + 1;
}
";
        let formatted = format(source).unwrap();
        assert_eq!(formatted, expected);
        assert_eq!(format(&formatted).unwrap(), formatted);
    }

    #[test]
    fn keeps_brace_comments_raw_strings_and_minus_signs() {
        let source = "fn main() {
    if a { // why
        f(match x { 1 => 2, default => 3 }, last_argument);
    }
    var s = `one
two`; var n = - - 3;
}
";
        let expected = "fn main() {
    if a { // why
        f(
            match x {
                1 => 2,
                default => 3,
            },
            last_argument,
        );
    }
    var s = `one
two`;
    var n = - -3;
}
";
        let formatted = format(source).unwrap();
        assert_eq!(formatted, expected);
        assert_eq!(format(&formatted).unwrap(), formatted);
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
