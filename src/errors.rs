use ariadne::{Color, Label, Report, ReportKind};

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

    pub fn report(&self, filename: &str, source: &str, offset: usize) {
        let kind = match self.kind {
            ErrorKind::Syntax => ReportKind::Error,
            ErrorKind::Semantic => ReportKind::Error,
        };

        let color = match self.kind {
            ErrorKind::Syntax => Color::Red,
            ErrorKind::Semantic => Color::Magenta,
        };

        let (display_source, mut span_start, mut span_end, display_filename) =
            if self.span.start >= offset {
                let src = if offset <= source.len() {
                    &source[offset..]
                } else {
                    source
                };
                (
                    src,
                    self.span.start - offset,
                    self.span.end - offset,
                    filename.to_string(),
                )
            } else {
                (
                    source,
                    self.span.start,
                    self.span.end,
                    format!("{} (in std library)", filename),
                )
            };

        let source_len = display_source.len();
        span_start = span_start.min(source_len);
        span_end = span_end.min(source_len).max(span_start);

        if span_start == span_end || display_source.is_empty() {
            let prefix = match self.kind {
                ErrorKind::Syntax => "Syntax Error",
                ErrorKind::Semantic => "Semantic Error",
            };
            eprintln!(
                "\x1b[31m{}\x1b[0m: {}",
                prefix,
                self.message.replace("__", "::")
            );
            return;
        }

        let message = self.message.replace("__", "::");
        let display_source = display_source.replace("__", "::");

        Report::<(String, std::ops::Range<usize>)>::build(
            kind,
            (display_filename.clone(), span_start..span_end),
        )
        .with_message(&message)
        .with_label(
            Label::new((display_filename.clone(), span_start..span_end))
                .with_message(&message)
                .with_color(color),
        )
        .finish()
        .eprint(ariadne::sources([(display_filename, display_source)]))
        .unwrap();
    }
}

pub fn report_errors(errors: &[ZeruError], filename: &str, source: &str, offset: usize) {
    for error in errors {
        error.report(filename, source, offset);
    }
}
