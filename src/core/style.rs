use std::cmp::{max, min};
use std::fmt::{Formatter, Result, Display};

use super::source::{Span, Source, SourceMap};


//-------------------------------------------------------------------------------------------------

pub trait Styleable {
    fn write<S: LineStyle>(&self, f: &mut Formatter, indent: u16, style: &S) -> Result;

    fn styled<'a, S: LineStyle>(&'a self, indent: u16, style: &'a S) -> Styled<'a, Self, S> {
        Styled { item: self, indent, style }
    }

    fn fmt_styled(&self, f: &mut Formatter) -> Result {
        self.write(f, 0, &IndentedStyle::new(2))
    }
}


pub struct Styled<'a, S: Styleable + ?Sized, LS: LineStyle> {
    item:                   &'a S,
    indent:                 u16,
    style:                  &'a LS,
}

impl<S: Styleable, LS: LineStyle> Display for Styled<'_, S, LS> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        self.item.write(f, self.indent, self.style)
    }
}


//-------------------------------------------------------------------------------------------------

pub trait LineStyle {
    fn write(&self, f: &mut Formatter, indent: u16, span: Option<Span>, line: &str) -> Result;
    fn writeln(&self, f: &mut Formatter, indent: u16, span: Option<Span>, line: &str) -> Result {
        self.write(f, indent, span, line)?;
        writeln!(f)
    }
}


//-------------------------------------------------------------------------------------------------

pub struct IndentedStyle {
    tab:                    u16,
}

impl IndentedStyle {
    pub fn new(tab: u16) -> Self { Self { tab } }
}

impl LineStyle for IndentedStyle {
    fn write(&self, f: &mut Formatter, indent: u16, _span: Option<Span>, line: &str) -> Result {
        write!(f, "{}{line}", " ".repeat((indent * self.tab) as usize))
    }
}


//-------------------------------------------------------------------------------------------------

pub struct SourceStyle<'a, S: Source> {
    tab:                    u16,
    width:                  u16,
    highlight:              bool,
    map:                    &'a SourceMap<S>,
}


impl <'a, S: Source> SourceStyle<'a, S> {
    pub fn new(tab: u16, width: u16, highlight: bool, map: &'a SourceMap<S>) -> Self {
        Self{tab, width, highlight, map}
    }
}


impl<S: Source> LineStyle for SourceStyle<'_, S> {
    fn write(&self, f: &mut Formatter, indent: u16, span: Option<Span>, line: &str) -> Result {
        let (yellow, stop) = ("\x1b[1;33m", "\x1b[0m");

        let width = self.width as usize;
        let indented_line = format!("{}{line}", " ".repeat((indent * self.tab) as usize));

        if let Some(span) = span {
            let (_, line_span) = self.map.line_span_from_span(span);
            let source_line = self.map.span(line_span);
            write!(f, "{indented_line:width$} # {source_line}")?;
            if self.highlight {
                write!(f, "\n{} # {}{yellow}{}{stop}",
                    " ".repeat(width),
                    " ".repeat(span.start() - line_span.start()),
                    "^".repeat(max(1, min(span.len(), line_span.end() - span.start()))))
            } else { Ok(() )}
        } else {
            write!(f, "{indented_line:width$}")
        }
    }
}


//-------------------------------------------------------------------------------------------------
