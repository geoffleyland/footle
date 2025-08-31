use std::{cmp::max, fmt};

//-------------------------------------------------------------------------------------------------

/// A span of characters in a source.
///
/// Usually this holds the bounds of a token or an AST node.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    start:              usize,
    end:                usize
}


impl Span {
    #[cfg(test)]
    pub const fn new(start: usize, end: usize) -> Self { Self { start, end } }
    pub const fn start(&self) -> usize { self.start }
    pub const fn end(self) -> usize { self.end }
    pub const fn len(&self) -> usize { self.end - self.start }
    pub const fn sub(&self, other: &Self) -> usize { self.start.saturating_sub(other.start) }
    pub const fn first(&self) -> Self { Self { start: self.start, end: self.start } }
    pub fn union(&self, other: &Self) -> Self {
        Self { start: self.start.min(other.start), end: self.end.max(other.end) }
    }
}


impl From<(usize, usize)> for Span {
    fn from(s: (usize, usize)) -> Self {
        Self {
            start:      s.0,
            end:        s.1,
        }
    }
}


impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}


//-------------------------------------------------------------------------------------------------

/// A source of characters for a Lexer
pub trait Source {
    /// Return the character at the offset (if there is one).
    fn at(&self, offset: usize) -> Option<char>;
    /// Return the string of characters between start and end.
    fn slice(&self, start: usize, end: usize) -> &str;
    /// Return the string of characters in the span.
    fn span(&self, span: Span) -> &str { self.slice(span.start, span.end) }
    /// Return the start of the character after the one at offset (for UTF-8).
    fn next(&self, offset: usize) -> Option<usize>;
}


impl Source for String {
    fn slice(&self, start: usize, end: usize) -> &str { &self[start..end] }
    fn at(&self, offset: usize) -> Option<char> { self[offset..].chars().next() }
    fn next(&self, offset: usize) -> Option<usize> {
        self[offset..].chars().next().map(|ch| offset + ch.len_utf8())
    }
}


impl Source for &str {
    fn slice(&self, start: usize, end: usize) -> &str { &self[start..end] }
    fn at(&self, offset: usize) -> Option<char> { self[offset..].chars().next() }
    fn next(&self, offset: usize) -> Option<usize> {
        self[offset..].chars().next().map(|ch| offset + ch.len_utf8())
    }
}

//-------------------------------------------------------------------------------------------------

/// A map of where lines start and end in a file.
///
/// I've kept this separate from Chars in an attempt to make it available to users after the parse
/// has finished, so that error reporting is less weird.  Haven't got there yet.
pub struct LineMap {
    starts:         Vec<usize>,
    ends:           Vec<usize>
}


impl LineMap {
    pub fn new() -> Self { Self { starts: vec![0], ends: vec![0] } }

    pub fn count(&mut self, c: char, nextpos: usize) {
        match c {
            '\n' => {
                self.starts.push(nextpos);
                self.ends.push(nextpos);
            }
            '\r' => {}
            _ => {
                if let Some(last) = self.ends.last_mut() {
                    *last = nextpos;
                }
            }
        }
    }

    pub fn line_span_from_pos(&self, pos: usize) -> (usize, Span) {
        let ln = self.ends.iter().position(|&e| e >= pos).unwrap();
        (ln + 1, (self.starts[ln], self.ends[ln]).into())
    }

    pub fn line_span_from_span(&self, span: Span) -> (usize, Span) {
        self.line_span_from_pos(span.start())
    }
}


//-------------------------------------------------------------------------------------------------

pub struct SourceMap<S: Source> {
    file_name:          String,
    source:             S,
    map:                LineMap,
}


impl<S: Source> SourceMap<S> {
    pub fn new<Str: Into<String>>(file_name: Str, source: S, map: LineMap) -> Self {
        Self { file_name: file_name.into(), map, source }
    }

    pub fn show_span<T: Into<Span>>(&self, span: T, colour: bool) -> SpanToShow<'_, S> {
        SpanToShow { map: self, span: span.into(), colour }
    }
}


//-------------------------------------------------------------------------------------------------

/// A temporary object (constructed with `show_span`) for showing source location.
///
/// To use it, try `format!("{}", scanner.show_span((start, end)))`.
pub struct SpanToShow<'a, S: Source> {
    span:               Span,
    colour:             bool,
    map:                &'a SourceMap<S>,
}

/// Display a token in context in the source file or string.
impl<S: Source> std::fmt::Display for SpanToShow<'_, S> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (blue, yellow, stop) =
            if self.colour { ("\x1b[1;34m", "\x1b[1;33m", "\x1b[0m") } else { ("", "", "") };
        let (line_number, line_span) = self.map.map.line_span_from_span(self.span);
        let linenumlen = format!("{line_number}").len();
        let line = self.map.source.span(line_span);
        writeln!(f, "{}{}-->{} {}:{}:{}",
                " ".repeat(linenumlen), blue, stop, self.map.file_name, line_number, self.span.sub(&line_span) + 1)?;
        writeln!(f, "{}{} |{}", " ".repeat(linenumlen), blue, stop)?;
        writeln!(f, "{line_number}{blue} |{stop}  {line}")?;
        writeln!(f, "{}{} |{}  {}{}{}{}",
                " ".repeat(linenumlen), blue, stop,
                " ".repeat(self.span.sub(&line_span)), yellow,
                "^".repeat(max(1, self.span.len())), stop)
    }
}


//-------------------------------------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_source() {
        let source = "12345";
        assert_eq!(source.at(0), Some('1'));
        assert_eq!(source.at(5), None);
        assert_eq!(source.slice(1, 3), "23");
        assert_eq!(source.next(1), Some(2));
    }
}


//-------------------------------------------------------------------------------------------------
