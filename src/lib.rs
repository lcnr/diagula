//! A hacked together diagnostics library.
//!
//! Written because figuring out how the existing libraries
//! work was too much effort for me.

use std::{
    fmt,
    ops::{Deref, Range},
};

/// A guarantee that an error was emitted to standard error.
///
/// This cannot be constructed outside of this crate.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ErrorEmitted(());

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span<'x> {
    range: (u32, u32),
    file: &'x str,
    source: &'x str,
}

impl fmt::Debug for Span<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Span")
            .field("data", &self.span_str())
            .field("file", &self.file)
            .field("line", &self.line())
            .field("column", &self.column())
            .finish()
    }
}

impl<'x> Span<'x> {
    pub fn new(file: &'x str, source: &'x str, range: Range<usize>) -> Span<'x> {
        Span {
            range: (range.start as u32, range.end as u32),
            file,
            source,
        }
    }

    pub fn dummy() -> Span<'x> {
        Span::new("<dummy file>", "<dummy span>", 0..11)
    }

    fn line_str(self) -> &'x str {
        let line_start = self.source[..self.range.0 as usize]
            .rfind('\n')
            .map_or(0, |v| v + 1);
        self.source[line_start..].split('\n').next().unwrap()
    }

    pub fn span_str(self) -> &'x str {
        &self.source[self.range()]
    }

    fn line(self) -> u32 {
        self.source[..self.range.0 as usize].lines().count() as u32
    }

    fn column(self) -> u32 {
        let mut offset = 0;
        for c in self.source[..self.range.0 as usize].chars().rev() {
            if c == '\n' {
                break;
            } else {
                offset += 1;
            }
        }

        offset
    }

    fn range(self) -> Range<usize> {
        (self.range.0 as usize)..(self.range.1 as usize)
    }

    pub fn extend_right(self, other: usize) -> Span<'x> {
        assert!(self.range.1 <= other as u32);
        Span {
            range: (self.range.0, other as u32),
            ..self
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Spanned<'x, T> {
    pub value: T,
    pub span: Span<'x>,
}

impl<'x, T> Deref for Spanned<'x, T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

#[derive(Debug, Clone, Copy)]
enum ContextKind {
    Note,
    Help,
}

impl ContextKind {
    fn descr(self) -> &'static str {
        match self {
            ContextKind::Note => "note",
            ContextKind::Help => "help",
        }
    }
}
struct Context<'x> {
    kind: ContextKind,
    msg: String,
    span: Option<Span<'x>>,
}

pub struct Diagnostic<'x> {
    msg: String,
    span: Span<'x>,
    context: Vec<Context<'x>>,
    emitted: bool,
}

impl Drop for Diagnostic<'_> {
    fn drop(&mut self) {
        if !self.emitted {
            panic!("`Diagnostic` must be emitted");
        }
    }
}

impl From<Diagnostic<'_>> for ErrorEmitted {
    fn from(diag: Diagnostic<'_>) -> Self {
        diag.emit()
    }
}

impl<'x> Diagnostic<'x> {
    pub fn build<M: Into<String>>(span: Span<'x>, msg: M) -> Diagnostic<'x> {
        Diagnostic {
            msg: msg.into(),
            span: span,
            context: Vec::new(),
            emitted: false,
        }
    }

    pub fn with_span_note<M: Into<String>>(mut self, span: Span<'x>, msg: M) -> Diagnostic<'x> {
        self.context.push(Context {
            kind: ContextKind::Note,
            msg: msg.into(),
            span: Some(span),
        });
        self
    }

    pub fn with_note<M: Into<String>>(mut self, msg: M) -> Diagnostic<'x> {
        self.context.push(Context {
            kind: ContextKind::Note,
            msg: msg.into(),
            span: None,
        });
        self
    }

    pub fn with_help<M: Into<String>>(mut self, msg: M) -> Diagnostic<'x> {
        self.context.push(Context {
            kind: ContextKind::Help,
            msg: msg.into(),
            span: None,
        });
        self
    }

    fn emit_span(&self, span: Span<'x>) {
        let offset = span.column();
        let line = span.line();
        let pos = format!("{}:{}", line, offset);
        println!("  --> {}:{}", span.file, pos);
        println!("({}): {}", pos, span.line_str());
        for _ in 0..(offset as usize + pos.len() + 4) {
            print!(" ");
        }
        for _ in 0..span.range().len().max(1) {
            print!("^");
        }
        println!();
    }

    pub fn emit(mut self) -> ErrorEmitted {
        println!("[ERROR]: {}", self.msg);
        self.emit_span(self.span);
        for context in &self.context {
            println!("  {}: {}", context.kind.descr(), context.msg);
            if let Some(span) = context.span {
                self.emit_span(span);
            }
        }
        self.emitted = true;
        ErrorEmitted(())
    }
}
