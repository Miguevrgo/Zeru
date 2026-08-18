use ariadne::{Color, Label, Report, ReportKind};
use inkwell::support::LLVMString;
use std::ops::Range;
use thiserror::Error;

/// Represents a span in the source code (byte offsets)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
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
    #[allow(dead_code)]
    pub line: usize,
}

impl ZeruError {
    pub fn syntax(message: impl Into<String>, span: Span, line: usize) -> Self {
        Self {
            kind: ErrorKind::Syntax,
            message: message.into(),
            span,
            line,
        }
    }

    pub fn semantic(message: impl Into<String>, span: Span, line: usize) -> Self {
        Self {
            kind: ErrorKind::Semantic,
            message: message.into(),
            span,
            line,
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
            eprintln!("\x1b[31m{}\x1b[0m: {message}", self.prefix());
            return;
        };

        let color = match self.kind {
            ErrorKind::Syntax => Color::Red,
            ErrorKind::Semantic => Color::Magenta,
        };
        let name = name.to_string();

        Report::<(String, Range<usize>)>::build(ReportKind::Error, (name.clone(), range.clone()))
            .with_message(message)
            .with_label(
                Label::new((name.clone(), range))
                    .with_message(message)
                    .with_color(color),
            )
            .finish()
            .eprint(ariadne::sources([(name, text.to_string())]))
            .unwrap();
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
}

impl Sources {
    /// Append a file and return where it starts, so it can be lexed on its own
    /// while still reporting spans as offsets into the whole program.
    pub fn push(&mut self, name: impl Into<String>, source: &str) -> usize {
        let start = self.text.len();
        self.text.push_str(source);
        self.text.push('\n');
        self.files.push(SourceFile {
            name: name.into(),
            start,
            end: self.text.len(),
        });
        start
    }

    /// The file a span points into, its text, and the span rebased onto it.
    fn locate(&self, span: Span) -> Option<(&str, &str, Range<usize>)> {
        let file = self
            .files
            .iter()
            .find(|file| (file.start..file.end).contains(&span.start))?;

        let text = &self.text[file.start..file.end];
        let end = span.end.min(file.end) - file.start;
        Some((&file.name, text, span.start - file.start..end))
    }
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
    #[error("std library not found: install zeru or set ZERU_STD_PATH for local development")]
    StdNotFound,
    #[error("module '{0}' not found")]
    ModuleNotFound(String),
    #[error("invalid path: must point to a .zr file with a valid UTF-8 name")]
    InvalidPath,
    #[error("linking failed: clang exited with {0}")]
    Link(std::process::ExitStatus),
}

#[cfg(test)]
mod tests {
    use super::{Sources, Span};

    #[test]
    fn locates_a_span_in_the_file_it_came_from() {
        let mut sources = Sources::default();
        sources.push("first.zr", "fn a() { }");
        sources.push("second.zr", "fn bb() { }");

        // "a" sits at offset 3 of the first file, which starts the buffer.
        let (name, text, range) = sources.locate(Span::new(3, 4)).expect("inside first.zr");
        assert_eq!(name, "first.zr");
        assert_eq!(&text[range], "a");

        // "bb" sits at offset 3 of the second, so 14 in the buffer: the first
        // file is ten characters plus the newline push adds one.
        let (name, text, range) = sources.locate(Span::new(14, 16)).expect("inside second.zr");
        assert_eq!(name, "second.zr");
        assert_eq!(&text[range], "bb");
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

        // A span that starts in the first file may not reach into the second.
        let (name, text, range) = sources.locate(Span::new(3, 500)).expect("inside first.zr");
        assert_eq!(name, "first.zr");
        assert!(range.end <= text.len());
    }
}
