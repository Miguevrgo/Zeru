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
}

#[derive(Debug, Clone)]
pub struct ZeruError {
    pub kind: ErrorKind,
    pub message: String,
    pub span: Span,
    pub notes: Vec<(String, Span)>,
}

impl ZeruError {
    pub fn syntax(message: impl Into<String>, span: Span) -> Self {
        Self {
            kind: ErrorKind::Syntax,
            message: message.into(),
            span,
            notes: Vec::new(),
        }
    }

    pub fn semantic(message: impl Into<String>, span: Span) -> Self {
        Self {
            kind: ErrorKind::Semantic,
            message: message.into(),
            span,
            notes: Vec::new(),
        }
    }

    fn prefix(&self) -> &'static str {
        match self.kind {
            ErrorKind::Syntax => "Syntax Error",
            ErrorKind::Semantic => "Semantic Error",
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

        let color = match self.kind {
            ErrorKind::Syntax => Color::Red,
            ErrorKind::Semantic => Color::Magenta,
        };
        let mut files = vec![(name.to_string(), text.to_string())];
        let mut report = Report::<(String, Range<usize>)>::build(
            ReportKind::Error,
            (name.to_string(), range.clone()),
        )
        .with_config(
            ariadne::Config::default()
                .with_color(colored())
                .with_index_type(ariadne::IndexType::Byte),
        )
        .with_message(message)
        .with_label(
            Label::new((name.to_string(), range))
                .with_message(message)
                .with_color(color),
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
                    .with_color(Color::Blue),
            );
        }
        report.finish().eprint(ariadne::sources(files)).unwrap();
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
    start: usize,
    end: usize,
    lines: Vec<usize>,
}

impl Sources {
    /// Append a file and return where it starts, so it can be lexed on its own
    /// while still reporting spans as offsets into the whole program.
    pub fn push(&mut self, name: impl Into<String>, source: &str) -> usize {
        let start = self.text.len();
        self.text.push_str(source);
        self.text.push('\n');
        let lines = std::iter::once(0)
            .chain(source.match_indices('\n').map(|(at, _)| at + 1))
            .collect();
        self.files.push(SourceFile {
            name: name.into(),
            start,
            end: self.text.len(),
            lines,
        });
        start
    }

    pub fn end(&self) -> usize {
        self.text.len()
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
    pub fn json(&self, sources: &Sources) -> String {
        let notes: Vec<String> = self
            .notes
            .iter()
            .map(|(message, span)| format!("{{{}}}", json_at(sources, *span, message)))
            .collect();
        let at = json_at(sources, self.span, &self.message);
        format!("{{{at},\"notes\":[{}]}}", notes.join(","))
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
}

#[cfg(test)]
mod tests {
    use super::{Sources, Span};

    #[test]
    fn locates_a_span_in_the_file_it_came_from() {
        let mut sources = Sources::default();
        sources.push("first.zr", "fn a() { }");
        sources.push("second.zr", "fn bb() { }");

        let (name, text, range) = sources.locate(Span::new(3, 4)).expect("inside first.zr");
        assert_eq!(name, "first.zr");
        assert_eq!(&text[range], "a");

        let (name, text, range) = sources.locate(Span::new(14, 16)).expect("inside second.zr");
        assert_eq!(name, "second.zr");
        assert_eq!(&text[range], "bb");
    }

    #[test]
    fn positions_count_lines_and_columns_from_one() {
        let mut sources = Sources::default();
        sources.push("first.zr", "ab\ncd\n");
        sources.push("second.zr", "x\n  y");
        assert_eq!(sources.position(Span::new(4, 5)), Some(("first.zr", 2, 2)));
        let y = sources.end() - 2;
        assert_eq!(
            sources.position(Span::new(y, y + 1)),
            Some(("second.zr", 2, 3))
        );
    }

    #[test]
    fn a_span_past_every_file_has_no_home() {
        let mut sources = Sources::default();
        sources.push("only.zr", "fn a() { }");

        assert!(sources.locate(Span::new(999, 1000)).is_none());
    }

    #[test]
    fn a_span_running_past_its_file_is_clamped() {
        let mut sources = Sources::default();
        sources.push("first.zr", "fn a() { }");
        sources.push("second.zr", "fn bb() { }");

        let (name, text, range) = sources.locate(Span::new(3, 500)).expect("inside first.zr");
        assert_eq!(name, "first.zr");
        assert!(range.end <= text.len());
    }
}
