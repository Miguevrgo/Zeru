use ariadne::{Color, Label, Report, ReportKind};
use inkwell::support::LLVMString;
use std::ops::Range;
use thiserror::Error;

/// Represents a span in the source code (byte offsets)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn merge(self, other: Span) -> Span {
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self {
        span.start..span.end
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorKind {
    Syntax,
    Semantic,
    Warning,
}

/// A problem found in the program: what is wrong, where, what the spot
/// itself shows when that says more, other spots that explain it, and how
/// to fix it, with the edit that does when it is a single one.
#[derive(Debug, Clone)]
pub struct ZeruError {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Span,
    pub label: Option<String>,
    pub notes: Vec<(String, Span)>,
    pub help: Option<String>,
    pub fix: Option<(Span, String)>,
}

/// Sort `errors` by where they point and drop the repeats, and those an
/// earlier one already says how to fix, with the same help and edit.
pub fn tidy(errors: &mut Vec<ZeruError>) {
    errors.sort_by(|a, b| (a.span.start, &a.message).cmp(&(b.span.start, &b.message)));
    errors.dedup_by(|a, b| a.span == b.span && a.message == b.message);
    let mut fixes = Vec::new();
    errors.retain(|error| {
        let Some(fix) = &error.fix else {
            return true;
        };
        let fix = (fix.clone(), error.help.clone());
        let new = !fixes.contains(&fix);
        fixes.push(fix);
        new
    });
}

impl ZeruError {
    fn new(kind: ErrorKind, message: impl Into<String>, span: Span) -> Self {
        Self {
            kind,
            message: message.into(),
            span,
            label: None,
            notes: Vec::new(),
            help: None,
            fix: None,
        }
    }

    pub fn syntax(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Syntax, message, span)
    }

    pub fn semantic(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Semantic, message, span)
    }

    /// Something that builds but is likely a mistake. It never stops a build.
    pub fn warning(message: impl Into<String>, span: Span) -> Self {
        Self::new(ErrorKind::Warning, message, span)
    }

    pub fn label(&mut self, label: impl Into<String>) -> &mut Self {
        self.label = Some(label.into());
        self
    }

    pub fn note(&mut self, note: impl Into<String>, span: Span) -> &mut Self {
        if span != self.span && self.notes.iter().all(|(_, at)| *at != span) {
            self.notes.push((note.into(), span));
        }
        self
    }

    pub fn help(&mut self, help: impl Into<String>) -> &mut Self {
        self.help = Some(help.into());
        self
    }

    /// The edit `help` describes: `replacement` in place of what `span`
    /// covers, which may be nothing, to insert it.
    pub fn fix(&mut self, span: Span, replacement: impl Into<String>) -> &mut Self {
        self.fix = Some((span, replacement.into()));
        self
    }

    fn prefix(&self) -> &'static str {
        match self.kind {
            ErrorKind::Syntax => "Syntax Error",
            ErrorKind::Semantic => "Semantic Error",
            ErrorKind::Warning => "Warning",
        }
    }

    fn report(&self, sources: &Sources) {
        let message = &self.message;

        let Some((name, text, range)) = sources
            .locate(self.span)
            .filter(|(_, _, range)| !range.is_empty())
        else {
            let (red, reset) = paint("\x1b[31m");
            eprintln!("{red}{}{reset}: {message}", self.prefix());
            return;
        };

        let (kind, color) = match self.kind {
            ErrorKind::Syntax => (ReportKind::Error, Color::Red),
            ErrorKind::Semantic => (ReportKind::Error, Color::Magenta),
            ErrorKind::Warning => (ReportKind::Warning, Color::Yellow),
        };
        let mut files = vec![(name.to_string(), text.to_string())];
        let mut report =
            Report::<(String, Range<usize>)>::build(kind, (name.to_string(), range.clone()))
                .with_config(
                    ariadne::Config::default()
                        .with_color(colored())
                        .with_index_type(ariadne::IndexType::Byte),
                )
                .with_message(message);
        report = report.with_label(
            Label::new((name.to_string(), range))
                .with_message(self.label.as_ref().unwrap_or(message))
                .with_color(color)
                .with_order(self.span.start as i32),
        );
        for (note, span) in &self.notes {
            let Some((name, text, range)) = sources.locate(*span) else {
                continue;
            };
            if !files.iter().any(|(file, _)| file == name) {
                files.push((name.to_string(), text.to_string()));
            }
            report = report.with_label(
                Label::new((name.to_string(), range))
                    .with_message(note)
                    .with_color(Color::Blue)
                    .with_order(span.start as i32),
            );
        }
        if let Some(help) = &self.help {
            report = report.with_help(match self.fixed(sources) {
                Some(code) => format!("{help}\n{code}"),
                None => help.clone(),
            });
        }
        report.finish().eprint(ariadne::sources(files)).unwrap();
    }

    /// The lines the fix touches as they read once it is made, or the lines
    /// it adds, indented under the help that describes it.
    fn fixed(&self, sources: &Sources) -> Option<String> {
        let (span, replacement) = self.fix.as_ref()?;
        let (_, text, range) = sources.locate(*span)?;
        let start = text[..range.start].rfind('\n').map_or(0, |at| at + 1);
        let end = text[range.end..]
            .find('\n')
            .map_or(text.len(), |at| range.end + at);
        let lines = match replacement.contains('\n') {
            true => replacement.clone(),
            false => format!(
                "{}{replacement}{}",
                &text[start..range.start],
                &text[range.end..end]
            ),
        };
        let indent = lines
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| line.len() - line.trim_start().len())
            .min()
            .unwrap_or(0);
        let shown: Vec<String> = lines
            .lines()
            .map(|line| format!("    {}", line.get(indent..).unwrap_or("").trim_end()))
            .collect();
        Some(shown.join("\n"))
    }
}

/// The program as one buffer, plus which file each part came from, so an error
/// is shown against its own source with its own line numbers.
#[derive(Default)]
pub struct Sources {
    text: String,
    files: Vec<SourceFile>,
}

struct SourceFile {
    name: String,
    std: bool,
    start: usize,
    end: usize,
    lines: Vec<usize>,
}

impl Sources {
    /// Append a file and return where it starts, so it can be lexed on its own
    /// while still reporting spans as offsets into the whole program.
    pub fn push(&mut self, name: impl Into<String>, source: &str, std: bool) -> usize {
        let start = self.text.len();
        self.text.push_str(source);
        self.text.push('\n');
        let lines = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(at, _)| at + 1))
            .collect();
        self.files.push(SourceFile {
            name: name.into(),
            std,
            start,
            end: self.text.len(),
            lines,
        });
        start
    }

    pub fn end(&self) -> usize {
        self.text.len()
    }

    /// The source a span covers.
    pub fn text(&self, span: Span) -> &str {
        &self.text[span.start..span.end]
    }

    /// The file, line and column a span starts at, found by binary search
    /// in each file's table of line starts.
    pub fn position(&self, span: Span) -> Option<(&str, u32, u32)> {
        let file = self.file(span)?;
        let offset = span.start - file.start;
        let line = file.lines.partition_point(|&start| start <= offset);
        let column = offset - file.lines[line - 1] + 1;
        Some((&file.name, line as u32, column as u32))
    }

    /// Where `name` is first written as a whole word inside `within`.
    pub fn word_in(&self, within: Span, name: &str) -> Option<Span> {
        let text = self.text(within);
        text.match_indices(name)
            .find(|&(at, _)| whole(text, at, name.len()))
            .map(|(at, _)| Span::new(within.start + at, within.start + at + name.len()))
    }

    /// Where `name` is last written as a whole word inside `within`.
    pub fn last_word_in(&self, within: Span, name: &str) -> Option<Span> {
        let text = self.text(within);
        text.rmatch_indices(name)
            .find(|&(at, _)| whole(text, at, name.len()))
            .map(|(at, _)| Span::new(within.start + at, within.start + at + name.len()))
    }

    /// The part of `span` on its first line, as the head of a declaration.
    pub fn first_line(&self, span: Span) -> Span {
        let text = self.text(span);
        let end = text
            .find('\n')
            .map_or(text.len(), |at| text[..at].trim_end().len());
        Span::new(span.start, span.start + end)
    }

    /// Where the file a span points into starts.
    pub fn file_start(&self, span: Span) -> Option<usize> {
        self.file(span).map(|file| file.start)
    }

    /// Whether a span points into the standard library.
    pub fn in_std(&self, span: Span) -> bool {
        self.file(span).is_some_and(|file| file.std)
    }

    fn file(&self, span: Span) -> Option<&SourceFile> {
        let at = self.files.partition_point(|file| file.end <= span.start);
        self.files.get(at).filter(|file| file.start <= span.start)
    }

    /// The file a span points into, its text, and the span rebased onto it.
    fn locate(&self, span: Span) -> Option<(&str, &str, Range<usize>)> {
        let file = self.file(span)?;
        let text = &self.text[file.start..file.end];
        let end = span.end.min(file.end) - file.start;
        Some((&file.name, text, span.start - file.start..end))
    }
}

/// Whether the `len` bytes at `at` in `text` are a whole word there.
fn whole(text: &str, at: usize, len: usize) -> bool {
    let word = |c: char| c.is_alphanumeric() || c == '_';
    !text[..at].ends_with(word) && !text[at + len..].starts_with(word)
}

/// Whether stderr is a terminal, so output to it may be coloured.
pub fn colored() -> bool {
    use std::io::IsTerminal;
    std::io::stderr().is_terminal()
}

/// `code` and the reset after it, or nothing when stderr is not a terminal.
pub fn paint(code: &str) -> (&str, &str) {
    match colored() {
        true => (code, "\x1b[0m"),
        false => ("", ""),
    }
}

impl ZeruError {
    /// The error as one JSON object: where it is, with 1-based lines and
    /// byte columns, the end exclusive, its message and its notes alike.
    /// The fix, when there is one, says where its replacement goes, the end
    /// exclusive so that an insertion starts and ends at the same place.
    pub fn json(&self, sources: &Sources) -> String {
        let notes: Vec<String> = self
            .notes
            .iter()
            .map(|(message, span)| format!("{{{}}}", json_at(sources, *span, message)))
            .collect();
        let at = json_at(sources, self.span, &self.message);
        let severity = match self.kind {
            ErrorKind::Warning => "warning",
            _ => "error",
        };
        let text = |text: &Option<String>| text.as_deref().map_or("null".into(), json_string);
        let fix = self.fix.as_ref().map_or("null".into(), |(span, replacement)| {
            let (file, line, column) = sources.position(*span).unwrap_or(("", 0, 0));
            let end = Span::new(span.end, span.end);
            let (_, end_line, end_column) = sources.position(end).unwrap_or((file, line, column));
            format!(
                "{{\"file\":{},\"line\":{line},\"column\":{column},\"end_line\":{end_line},\"end_column\":{end_column},\"text\":{}}}",
                json_string(file),
                json_string(replacement)
            )
        });
        format!(
            "{{{at},\"severity\":\"{severity}\",\"label\":{},\"notes\":[{}],\"help\":{},\"fix\":{fix}}}",
            text(&self.label),
            notes.join(","),
            text(&self.help)
        )
    }
}

fn json_at(sources: &Sources, span: Span, message: &str) -> String {
    let (file, line, column) = sources.position(span).unwrap_or(("", 0, 0));
    let last = Span::new(span.end.max(span.start + 1) - 1, span.end);
    let (_, end_line, end_column) = sources.position(last).unwrap_or((file, line, column));
    format!(
        "\"file\":{},\"line\":{line},\"column\":{column},\"end_line\":{end_line},\"end_column\":{},\"message\":{}",
        json_string(file),
        end_column + 1,
        json_string(message)
    )
}

fn json_string(text: &str) -> String {
    let mut out = String::from('"');
    for c in text.chars() {
        match c {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            c if u32::from(c) < 0x20 => out.push_str(&format!("\\u{:04x}", u32::from(c))),
            c => out.push(c),
        }
    }
    out.push('"');
    out
}

pub fn report_errors(errors: &[ZeruError], sources: &Sources) {
    for error in errors {
        error.report(sources);
    }
}

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("[IO]: {0}")]
    Io(#[from] std::io::Error),
    #[error("aborting due to previous error")]
    Unknown,
    #[error("LLVM error: {0}")]
    Llvm(#[from] LLVMString),
    #[error("invalid path: must point to a .zr file with a valid UTF-8 name")]
    InvalidPath,
    #[error("linking failed: cc exited with {0}")]
    Link(std::process::ExitStatus),
    #[error("LLVM rejected the program: {0}")]
    Rejected(String),
    #[error("{0} file(s) not formatted; zeru fmt formats them")]
    Unformatted(usize),
}
