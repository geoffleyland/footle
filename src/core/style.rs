use std::fmt::{Formatter, Result, Display};

use super::source::Span;


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
    tab: u16,
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
