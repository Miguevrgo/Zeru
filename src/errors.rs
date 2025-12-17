use ariadne::{Color, Label, Report, ReportKind, Source};

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

    pub fn report(&self, _filename: &str, source: &str) {
        let kind = match self.kind {
            ErrorKind::Syntax => ReportKind::Error,
            ErrorKind::Semantic => ReportKind::Error,
        };

        let color = match self.kind {
            ErrorKind::Syntax => Color::Red,
            ErrorKind::Semantic => Color::Magenta,
        };

        let label_msg = match self.kind {
            ErrorKind::Syntax => "syntax error here",
            ErrorKind::Semantic => "error here",
        };

        let span_start = self.span.start.min(source.len());
        let span_end = self.span.end.min(source.len()).max(span_start);

        if span_start == span_end || source.is_empty() {
            let prefix = match self.kind {
                ErrorKind::Syntax => "Syntax Error",
                ErrorKind::Semantic => "Semantic Error",
            };
            eprintln!("\x1b[31m{}\x1b[0m: {}", prefix, self.message);
            return;
        }

        Report::build(kind, self.span.start..self.span.end)
            .with_message(&self.message)
            .with_label(
                Label::new(span_start..span_end)
                    .with_message(label_msg)
                    .with_color(color),
            )
            .finish()
            .eprint(Source::from(source))
            .unwrap();
    }
}

pub fn report_errors(errors: &[ZeruError], _filename: &str, source: &str) {
    for error in errors {
        error.report(_filename, source);
    }
}
