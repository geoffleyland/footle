use crate::core::LineMap;


//-------------------------------------------------------------------------------------------------

/// Something to keep track of the characters we're reading from the Source.
///
/// We need a current position, and since we're going to want to display things nicely later, we
/// keep track of the starts and ends of lines as we go.
pub struct Scanner {
    source:         String,
    pos:            usize,
    map:            LineMap
}


impl Scanner {
    pub fn new<S: Into<String>>(s: S) -> Self {
        Self {
            source: s.into(),
            pos:    0,
            map:    LineMap::new()
        }
    }

    /// Close a Scanner, returning the `LineMap` it created.
    pub fn close(self) -> (String, LineMap) { (self.source, self.map) }


    /// Return the position in the file.
    ///
    /// This is only used at the end of the file as an 'end-of-file' position.  It would be nice to
    /// find another way to do that.
    pub const fn pos(&self) -> usize { self.pos }


    /// Return text from start to the character before end.
    pub fn slice(&self, start: usize, end: usize) -> String { self.source[start..end].into() }
}


impl Iterator for Scanner  {
    type Item = (char, usize);

    /// Produce the next character.
    ///
    /// We keep track of the current line, and of the starts and ends of lines so far, so each time
    /// we move advance, we have to check if we've seen a newline (or not) and update our counters
    /// accordingly.
    fn next(&mut self) -> Option<(char, usize)> {
        let start = self.pos;
        let c = self.source[start..].chars().next()?;
        self.pos = start + c.len_utf8();
        self.map.count(c, self.pos);
        Some((c, start))
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
    fn test_utf8() {
        // Positions are byte offsets, so each character moves us on by its UTF-8 length:
        // 'é' is 2 bytes, '€' is 3 and '𝄞' is 4.
        let mut scanner = Scanner::new("aé€𝄞1\né");
        assert_eq!(scanner.next(), Some(('a', 0)));
        assert_eq!(scanner.next(), Some(('é', 1)));
        assert_eq!(scanner.next(), Some(('€', 3)));
        assert_eq!(scanner.next(), Some(('𝄞', 6)));
        assert_eq!(scanner.next(), Some(('1', 10)));
        assert_eq!(scanner.slice(1, scanner.pos()), "é€𝄞1");
        assert_eq!(scanner.next(), Some(('\n', 11)));
        assert_eq!(scanner.next(), Some(('é', 12)));
        assert_eq!(scanner.pos(), 14);
        assert_eq!(scanner.next(), None);

        // Line ends are byte offsets too.
        assert_eq!(scanner.map.line_span_from_pos(6), (1, Span::new(0, 11)));
        assert_eq!(scanner.map.line_span_from_pos(12), (2, Span::new(12, 14)));
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
