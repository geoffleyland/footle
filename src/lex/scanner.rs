use crate::core::{Source, LineMap};


//-------------------------------------------------------------------------------------------------

/// Something to keep track of the characters we're reading from the Source.
///
/// We need a current position, and since we're going to want to display things nicely later, we
/// keep track of the starts and ends of lines as we go.
pub struct Scanner<S: Source> {
    source:         S,
    pos:            usize,
    map:            LineMap
}


impl <S: Source> Scanner<S> {
    pub fn new(s: S) -> Self {
        Self {
            source: s,
            pos:    0,
            map:    LineMap::new()
        }
    }

    /// Close a Scanner, returning the `LineMap` it created.
    pub fn close(self) -> (S, LineMap) { (self.source, self.map) }


    /// Return the position in the file.
    ///
    /// This is only used at the end of the file as an 'end-of-file' position.  It would be nice to
    /// find another way to do that.
    pub const fn pos(&self) -> usize { self.pos }


    /// Return text from start to the character before end.
    pub fn slice(&self, start: usize, end: usize) -> String { self.source.slice(start, end).into() }
}


impl<S:Source> Iterator for Scanner<S> {
    type Item = (char, usize);

    /// Produce the next character.
    ///
    /// We keep track of the current line, and of the starts and ends of lines so far, so each time
    /// we move advance, we have to check if we've seen a newline (or not) and update our counters
    /// accordingly.
    fn next(&mut self) -> Option<(char, usize)> {
        let p = self.pos;
        let next = self.source.next(self.pos);

        if let Some(p) = next { self.pos = p; }

        if let Some(c) = self.source.at(p) {
            self.map.count(c, next.unwrap_or(self.pos+1));
            Some((c, p))
        } else { None }
    }
}


//-------------------------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::Span;

    #[test]
    fn test_scanner() {
        let mut scanner = Scanner::new("12345");
        assert_eq!(scanner.pos(), 0);
        assert_eq!(scanner.next(), Some(('1', 0)));
        assert_eq!(scanner.pos(), 1);
        assert_eq!(scanner.next(), Some(('2', 1)));
        assert_eq!(scanner.next(), Some(('3', 2)));
        assert_eq!(scanner.slice(0, scanner.pos()), "123");
        assert_eq!(scanner.next(), Some(('4', 3)));
        assert_eq!(scanner.next(), Some(('5', 4)));
        assert_eq!(scanner.next(), None);
        assert_eq!(scanner.slice(1, scanner.pos()), "2345");
    }

    #[test]
    fn test_line_map() {
        let mut scanner = Scanner::new("abc\ndef");
        scanner.next();
        scanner.next();
        scanner.next();
        scanner.next();
        scanner.next();
        scanner.next();
        scanner.next();
        assert_eq!(scanner.map.line_span_from_pos(1), (1, Span::new(0, 3)));
        assert_eq!(scanner.map.line_span_from_pos(4), (2, Span::new(4, 7)));

        let mut scanner = Scanner::new("\nabc\ndef");
        scanner.next();
        scanner.next();
        scanner.next();
        scanner.next();
        scanner.next();
        assert_eq!(scanner.map.line_span_from_pos(2), (2, Span::new(1, 4)));
    }
}


//-------------------------------------------------------------------------------------------------
