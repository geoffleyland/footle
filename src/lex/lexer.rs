use std::collections::VecDeque;

use crate::core::{Span, Source, LineMap};
use super::token::{Token, match_reserved_word};
use super::scanner::{Scanner};


//-------------------------------------------------------------------------------------------------

macro_rules! start_token {
    ( $self: expr $(, $char_match: pat)? ) => {{
        #[allow(unused_variables)]
        if let Some((c, start)) = $self.lookahead(0) {
            $( debug_assert!(matches!(c, $char_match)); )?
            $self.advance();
            (c, start)
        } else { panic!("internal compiler error: expected token, got None"); }
    }};
}


macro_rules! eat_one {
    ( $self: expr, $char_match: pat ) => {{
        if let Some(tk @ ($char_match, _)) = $self.lookahead(0) {
            $self.advance();
            Some(tk.0)
        } else { None }
    }};
}


macro_rules! eat_all {
    ( $self: expr, $char_match: pat $(, $f: expr)? ) => {{
        let mut count = 0;
        #[allow(unused)] // if there's no $f, tk doesn't get used
        while let Some(tk @ ($char_match, _)) = $self.lookahead(0) {
            $( $f(tk.0); )?
            $self.advance();
            count += 1;
        }
        count
    }};
}


macro_rules! eat_until {
    ( $self: expr, $char_match: pat $(, $f: expr )? ) => {{
        let mut count = 0;
        loop {
            let tk = $self.lookahead(0);
            if let Some(($char_match, _)) = tk { break; }
            $( $f(tk.0); )?
            $self.advance();
            count += 1;
        }
        count
    }};
}


//-------------------------------------------------------------------------------------------------

pub type LexerResult = (Result<Token, String>, Span);

//-------------------------------------------------------------------------------------------------

pub struct Lexer<S: Source> {
    scanner:                    Scanner<S>,
    lookahead_buffer:           VecDeque<(char, usize)>,
}


impl<S: Source> Lexer<S> {
    pub fn new(s: S) -> Self {
        Self {
            scanner:            Scanner::new(s),
            lookahead_buffer:   VecDeque::new(),
        }
    }


    pub fn close(self) -> (S, LineMap) { self.scanner.close() }


    fn lookahead(&mut self, n: usize) -> Option<(char, usize)> {
        while self.lookahead_buffer.len() < n + 1 {
            self.lookahead_buffer.push_back(self.scanner.next()?);
        }
        Some(self.lookahead_buffer[n])
    }


    fn advance(&mut self) {
        self.lookahead(0);
        if !self.lookahead_buffer.is_empty() { self.lookahead_buffer.pop_front(); }
    }


    fn token_end(&mut self) -> usize {
        self.lookahead(0).map_or(self.scanner.pos(), |(_, pos)| pos)
    }


    /// Construct a `Token` (with location) from a `Token`, a start and the current position.
    fn token(&mut self, t: Token, start: usize) -> LexerResult {
        (Ok(t), (start, self.token_end()).into())
    }


    fn char_token(&mut self, t: Token, start: usize) -> LexerResult {
        self.advance();
        self.token(t, start)
    }


    fn error(&mut self, message: &str, start: usize) -> LexerResult {
        (Err(message.into()), (start, self.token_end()).into())
    }


    fn token_text(&mut self, start: usize) -> String {
        let end = self.token_end();
        self.scanner.slice(start, end)
    }


//-------------------------------------------------------------------------------------------------

    pub fn next_token(&mut self) -> LexerResult {
        use Token::*;
        if let Some((c, start)) = self.lookahead(0) {
            match c {
                'a'..='z' | 'A'..='Z' | '_'     => self.identifier(),

                // Math Operators
                '+'                             => self.char_token(Plus, start),
                '-'                             => self.char_token(Minus, start),
                '*'                             => self.char_token(Times, start),
                '/'                             => self.char_token(Divide, start),
                '^'                             => self.char_token(Power, start),
                // Parentheses
                '('                             => self.char_token(LeftParenthesis, start),
                ')'                             => self.char_token(RightParenthesis, start),
                // Punctuation
                ','                             => self.char_token(Comma, start),

                // Comparison
                '<' => {
                    self.advance();
                    let token = eat_one!(self, '=').map_or(LessThan, |_| LessEqual);
                    self.token(token, start)
                }
                '>' => {
                    self.advance();
                    let token = eat_one!(self, '=').map_or(GreaterThan, |_| GreaterEqual);
                    self.token(token, start)
                }

                // Assignment, equality and not equality
                '=' => {
                    self.advance();
                    let token = eat_one!(self, '=').map_or(Assign, |_| Equal);
                    self.token(token, start)
                }
                '!' => {
                    self.advance();
                    if eat_one!(self, '=').is_some() {
                        self.token(Token::NotEqual, start)
                    } else {
                        self.error("Expected '=' after '!'", start)
                    }
                }

                // Spaces
                ' ' | '\t' | '\r' | '\n' => {
                    eat_all!(self, ' ' | '\t' | '\r' | '\n');
                    self.next_token()
                }

                // Comments
                '#' => {
                    match self.comment() {
                        r @ (Err(..), ..)       => r,
                        _                       => self.next_token()
                    }
                }

                // Anything else is an error.
                _                               => self.error(&format!("unexpected character '{c}'"), start)
            }
        } else { self.token(Eof, self.scanner.pos()) }
    }


//-------------------------------------------------------------------------------------------------

    /// Parse an identifier
    ///
    /// Identifiers start with a letter or underscore, and continue with letters, numbers and
    /// underscores.
    /// They can also contain an `@`, and after than have to have digits, an underscore and digits.
    /// This is a kind of silly trick we use so that we can read code with variable names
    /// decorated with name and value versions, and then re-output code that looks the same.
    /// Later on (in the parser) we enforce that only immutable variables can contain an `@`.
    /// User code shouldn't contain at identifier with an `@`, only re-generated code gets to do
    /// that.
    fn identifier(&mut self) -> LexerResult {
        let (_, start) = start_token!(self, 'a'..='z' | 'A'..='Z' | '_');
        eat_all!(self, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9');
        let word = self.token_text(start);
        self.token(match_reserved_word(&word).unwrap_or(Token::Identifier(word)), start)
    }


    /// Parse a comment
    ///
    /// Single-line comments start with `#` and go to the end of the line.
    /// Multi-line comments start with `#(` and end with `#)`, and can be nested.
    fn comment(&mut self) -> LexerResult {
        let (_, start) = start_token!(self, '#');
        if eat_one!(self, '(').is_some() {
            // Multi-line comment
            let mut depth = 1;
            while let Some((c, _)) = self.lookahead(0) {
                self.advance();
                if c == '#' {
                    match self.lookahead(0) {
                        Some(('(', _)) => {
                            self.advance();
                            depth += 1;
                        }
                        Some((')', _)) => {
                            self.advance();
                            depth -= 1;
                            if depth == 0 { break; }
                        }
                        Some(_)             => {}
                        None                => break
                    }
                }
            }
            if depth == 0 {
                self.token(Token::BlockComment, start)
            } else {
                self.error("unfinished block comment", start)
            }
        } else {
            eat_until!(self, '\n' | '\r');
            self.token(Token::Comment, start)
        }
    }
}


//-------------------------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use super::super::token::Token;
    use crate::core::Span;
    use crate::core::SourceMap;


    fn expect_token(s: &str, expected: Token) {
        assert_eq!(expected, Lexer::new(s)
            .next_token().0
            .map_or_else(
                |e| panic!("expected {expected}, got '{e:?}'"),
                |obtained| obtained));
    }


    fn expect_identifier(s: &str, expected: &str) {
        let t = Lexer::new(s).next_token();
        if let (Ok(Token::Identifier(obtained)), ..) = t {
            assert_eq!(obtained, expected);
        } else {
            panic!("Expected `Identifier('{expected}')`, got {:?}", t.0);
        }
    }


    fn expect_error(s: &str, expected: &str) {
        let t = Lexer::new(s).next_token();
        if let (Err(obtained), ..) = t {
            assert_eq!(obtained, expected);
        } else {
            panic!("Expected error '{expected}', got {:?}", t.0);
        }
    }


    impl<S: Source> Iterator for Lexer<S> {
        type Item = LexerResult;

        fn next(&mut self) -> Option<LexerResult> {
            let t = self.next_token();
            if let (Ok(Token::Eof), ..) = t {
                None
            } else {
                Some(t)
            }
        }
    }

    fn collect_lex(s: &str) -> Vec<Token> {
        Lexer::new(s)
            .map(|(tk, ..)|
                match tk {
                    Ok(token)       => token,
                    Err(message)    => panic!("expected a valid token, got: '{message}'")
                })
            .collect::<Vec<Token>>()
    }


    fn span_to_string<S: Source, T: Into<Span>>(lex: Lexer<S>, span: T) -> String {
        let (source, map) = lex.close();
        format!("{}", SourceMap::new("(string)", source, map).show_span(span, false))
    }


    #[test]
    fn test_lookahead() {
        let mut l = Lexer::new("01234");
        assert_eq!(l.lookahead(0), Some(('0', 0)));
        assert_eq!(l.lookahead(1), Some(('1', 1)));
        assert_eq!(l.lookahead(0), Some(('0', 0)));
        assert_eq!(l.lookahead(2), Some(('2', 2)));
        assert_eq!(l.lookahead(0), Some(('0', 0)));
        l.advance();
        assert_eq!(l.lookahead(0), Some(('1', 1)));
        assert_eq!(l.lookahead(1), Some(('2', 2)));
        assert_eq!(l.lookahead(0), Some(('1', 1)));
        l.advance();
        assert_eq!(l.lookahead(0), Some(('2', 2)));
        assert_eq!(l.lookahead(1), Some(('3', 3)));
        assert_eq!(l.lookahead(2), Some(('4', 4)));
        assert_eq!(l.lookahead(3), None);
        l.advance();
        assert_eq!(l.lookahead(0), Some(('3', 3)));
        assert_eq!(l.lookahead(1), Some(('4', 4)));
        assert_eq!(l.lookahead(2), None);
        assert_eq!(l.lookahead(3), None);
        l.advance();
        assert_eq!(l.lookahead(0), Some(('4', 4)));
        assert_eq!(l.lookahead(1), None);
        assert_eq!(l.lookahead(2), None);
        l.advance();
        assert_eq!(l.lookahead(0), None);
        assert_eq!(l.lookahead(1), None);
        assert_eq!(l.lookahead(2), None);
        l.advance();
    }


    #[test]
    fn test_unknown() {
        expect_error("&", "unexpected character '&'");
    }


    #[test]
    fn test_identifier() {
        expect_identifier("token", "token");
        expect_identifier(" token", "token");
        expect_identifier("token ", "token");
        expect_identifier("token+", "token");
    }


    #[test]
    fn test_reserved_word() {
        use Token::*;
        expect_token("local", Local);
        expect_token("mutable", Mutable);
        expect_token("argument", Argument);
        expect_token("begin", Begin);
        expect_token("end", End);
        expect_token("return", Return);
    }


    #[test]
    fn test_identifiers_and_spaces() {
        assert_eq!(collect_lex("Hello \tworld"),
            vec!(
                Token::Identifier("Hello".to_string()),
                Token::Identifier("world".to_string())));
    }


    #[test]
    fn test_operators() {
        use Token::*;
        // Math
        expect_token("+",    Plus);
        expect_token("-",    Minus);
        expect_token("*",    Times);
        expect_token("/",    Divide);
        expect_token("^",    Power);

        // Parentheses
        expect_token("(",    LeftParenthesis);
        expect_token(")",    RightParenthesis);

        // Punctiation
        expect_token(",",    Comma);

        // Comparison
        expect_token("==",   Equal);
        expect_token("=",    Assign);
        expect_token("!=",   NotEqual);
        expect_token("<",    LessThan);
        expect_token("<=",   LessEqual);
        expect_token(">",    GreaterThan);
        expect_token(">=",   GreaterEqual);
        expect_error("!x",   "Expected '=' after '!'");

        // All of them
        assert_eq!(collect_lex("+-*/^()===!=<<=>>=,"),
            vec!(Plus, Minus, Times, Divide, Power,
                LeftParenthesis, RightParenthesis,
                Equal, Assign, NotEqual, LessThan, LessEqual, GreaterThan, GreaterEqual,
                Comma));
    }


    #[test]
    fn test_single_line_comment() {
        assert_eq!(collect_lex("Hello # comment\nworld"),
            vec!(
                Token::Identifier("Hello".to_string()),
                Token::Identifier("world".to_string())));
    }


    #[test]
    fn test_block_comment() {
        use Token::Identifier;
        assert_eq!(collect_lex("Hello #( comment\n comment #) world"),
            vec!(
                Identifier("Hello".to_string()),
                Identifier("world".to_string())));
        assert_eq!(collect_lex("Hello #( comment #( ###\n #) comment #) world"),
            vec!(
                Identifier("Hello".to_string()),
                Identifier("world".to_string())));

        let mut lex = Lexer::new("hello #( broken block\ncomment");
        let token = lex.next_token().0.unwrap();
        assert_eq!(token, Token::Identifier("hello".to_string()));
        let message = lex.next_token().0.unwrap_err();
        assert_eq!(message, "unfinished block comment");

        let mut lex = Lexer::new("Hello #( comment #( ###\n comment #) world");
        let token = lex.next_token().0.unwrap();
        assert_eq!(token, Token::Identifier("Hello".to_string()));
        let message = lex.next_token().0.unwrap_err();
        assert_eq!(message, "unfinished block comment");
    }


    #[test]
    fn test_token_context() {
        let mut lex = Lexer::new("  Hello");
        let (token, span) = lex.next_token();
        assert_eq!(token.unwrap(), Token::Identifier("Hello".to_string()));
        assert_eq!(span_to_string(lex, span),
" --> (string):1:3
  |
1 |    Hello
  |    ^^^^^
");

        let mut lex = Lexer::new("\n  Hello");
        let (token, span) = lex.next_token();
        assert_eq!(token.unwrap(), Token::Identifier("Hello".to_string()));
        assert_eq!(span_to_string(lex, span),
" --> (string):2:3
  |
2 |    Hello
  |    ^^^^^
");

        let mut lex = Lexer::new("a\n  Hello");
        let _ = lex.next_token();
        let (token, span) = lex.next_token();
        assert_eq!(token.unwrap(), Token::Identifier("Hello".to_string()));
        assert_eq!(span_to_string(lex, span),
" --> (string):2:3
  |
2 |    Hello
  |    ^^^^^
");
    }


    #[test]
    fn test_comment() {
        use Token::Identifier;
        assert_eq!(collect_lex(
r"
hello # comment
world #( block comment #) and # comment
everyone #( block comment #( nested
and with a newline #) continues #) elsewhere"),
            vec![
                Identifier("hello".to_string()),
                Identifier("world".to_string()),
                Identifier("and".to_string()),
                Identifier("everyone".to_string()),
                Identifier("elsewhere".to_string()),
            ]);
    }
}

//-------------------------------------------------------------------------------------------------
