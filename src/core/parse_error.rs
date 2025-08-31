use std::{convert::From, fmt};

use super::source::{Source, SourceMap, Span};

//-------------------------------------------------------------------------------------------------

pub enum ErrorPart {
    Message(String),
    Span(Span),
}


impl From<&str> for ErrorPart           { fn from(item: &str) -> Self           { Self::Message(item.to_string()) }}
impl From<String> for ErrorPart         { fn from(item: String) -> Self         { Self::Message(item) }}
impl From<(usize, usize)> for ErrorPart { fn from(item: (usize, usize)) -> Self { Self::Span(item.into()) }}
impl From<Span> for ErrorPart           { fn from(item: Span) -> Self           { Self::Span(item) }}


pub struct ParseError {
    pub parts:                          Vec<ErrorPart>,
}

#[macro_export]
macro_rules! parse_error {
    ( $self: expr, $( $m_or_l: expr ),+ ) => {{
        $self.push_error($crate::core::ParseError{parts: vec![$( $crate::core::ErrorPart::from($m_or_l) ),+]})
    }};
}


impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for m_or_l in &self.parts {
            match m_or_l {
                ErrorPart::Message(msg) => { write!(f, "{msg} ")?; }
                ErrorPart::Span(span)   => { write!(f, "{span} ")?; }
            }
        }
        Ok(())
    }
}


impl ParseError {
    pub fn show_in_source<'a, S: Source>(&'a self, map: &'a SourceMap<S>) -> ShowParseError<'a, S> {
        ShowParseError::new(self, map)
    }
}


#[cfg(test)]
impl ParseError {
    pub fn messages(&self) -> String {
        self.parts.iter().filter_map(|p| {
                if let ErrorPart::Message(message) = p { Some(message.to_owned()) } else { None }
            })
            .collect::<Vec<_>>().join("\n")
    }
}


//-------------------------------------------------------------------------------------------------

pub struct ShowParseError<'a, S: Source> {
    error:                              &'a ParseError,
    map:                                &'a SourceMap<S>,
}


impl<'a, S: Source> ShowParseError<'a, S> {
    pub fn new(error: &'a ParseError, map: &'a SourceMap<S>) -> Self { Self { error, map } }
}


impl<S: Source> fmt::Display for ShowParseError<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\x1b[1;31merror\x1b[0m: ")?;
        for m_or_l in &self.error.parts {
            match m_or_l {
                ErrorPart::Message(msg) => { writeln!(f, "{msg}")?; }
                ErrorPart::Span(span)   => { self.map.show_span(*span, true).fmt(f)?; }
            }
        }
        writeln!(f)
    }
}


//-------------------------------------------------------------------------------------------------
