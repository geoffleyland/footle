use std::{collections::VecDeque, convert::From};

use super::expr::{Expr, ExprKind};
use super::stmt::{Stmt, StmtKind};

use crate::core::{BinaryOperator, Declaration, LineMap, Nev, ParseError, Source, SourceMap, Span, parse_error};
use crate::lex::{Lexer, LexerResult, Token};


//-------------------------------------------------------------------------------------------------

/// Convert a Collection of `Result<T, Span>`s into an `Option<Nev<T>>`.
///
/// Returns `None` if there are any `Err`ors, or `Some(Nev<T>)` if all elements are `Ok`. It
/// assumes that the collection is non-empty, and unwraps the result of the `Nev`'s `try_into`.
fn try_collect_nev<C: IntoIterator<Item = Result<T, Span>>, T>(c: C) -> Option<Nev<T>> {
    c.into_iter().map(Result::ok).collect::<Option<Vec<_>>>().map(|v| v.try_into().unwrap())
}


//-------------------------------------------------------------------------------------------------

macro_rules! expect {
    ( $self: expr, $expected: expr $(, $m_or_l: expr )* ) => {{
        match $self.lookahead(0) {
            (Ok(found), span) if found == $expected => {
                $self.advance();
                span
            }
            (Ok(_), span) => {
                parse_error!($self, format!("expected '{}'", $expected), span $(, $m_or_l )* );
                span.first()
            }
            (Err(message), span) => {
                parse_error!($self, message, span $(, $m_or_l )* );
                $self.advance();
                span
            }
        }
    }}
}


//-------------------------------------------------------------------------------------------------

pub fn parse<S, T>(file_name: T, s: S) -> (Vec<Stmt>, Vec<ParseError>, SourceMap<S>)
where
    S: Source,
    T: Into<String>,
{
    let mut p = Parser::new(s);
    let stmts = p.parse_stmts();
    let (errors, source, map) = p.close();
    (stmts, errors, SourceMap::new(file_name, source, map))
}

//-------------------------------------------------------------------------------------------------

struct Parser<S: Source> {
    lexer:                                  Lexer<S>,
    lookahead_buffer:                       VecDeque<LexerResult>,
    errors:                                 Vec<ParseError>,
}


impl<S: Source> Parser<S> {
    fn new(s: S) -> Self {
        Self {
            lexer:                          Lexer::new(s),
            lookahead_buffer:               VecDeque::new(),
            errors:                         vec![],
        }
    }


    fn close(self) -> (Vec<ParseError>, S, LineMap) {
        let (source, map) = self.lexer.close();
        (self.errors, source, map)
    }


    fn lookahead(&mut self, n: usize) -> LexerResult {
        while self.lookahead_buffer.len() < n + 1 {
            self.lookahead_buffer.push_back(self.lexer.next_token());
        }
        self.lookahead_buffer[n].clone()
    }


    fn advance(&mut self) {
        _ = self.lookahead(0);
        if !self.lookahead_buffer.is_empty() {
            self.lookahead_buffer.pop_front();
        }
    }


    fn expect_identifier(&mut self) -> (Option<String>, Span) {
        let (token, span) = self.lookahead(0);
        match token {
            Ok(Token::Identifier(name)) => {
                self.advance();
                (Some(name), span)
            }
            Ok(_) => {
                parse_error!(self, "expected identifier", span);
                (None, span.first())
            }
            Err(message) => {
                parse_error!(self, message, span);
                self.advance();
                (None, span)
            }
        }
    }


    fn push_error(&mut self, error: ParseError) { self.errors.push(error); }


//-------------------------------------------------------------------------------------------------

    fn parse_stmts(&mut self) -> Vec<Stmt> {
        let mut statements = vec![];
        loop {
            if let (Ok(Token::End | Token::Eof), ..) = self.lookahead(0) {
                break;
            }
            if let Some(s) = self.parse_stmt() {
                statements.push(s);
            }
        }
        statements
    }


    fn parse_stmt(&mut self) -> Option<Stmt> {
        use Token::*;
        let (token, span) = self.lookahead(0);
        match token {
            Ok(Argument)                        => self.parse_arguments(),
            Ok(Number(_) | LeftParenthesis)     => self.parse_exprs(),
            Ok(Identifier(_))                   => self.parse_assignments_or_exprs(),
            Ok(Return)                          => self.parse_return(),
            Ok(Begin)                           => Some(self.parse_block()),
            Ok(Local) => {
                self.advance();
                self.parse_local(Declaration::Local, span)
            }
            Ok(Mutable) => {
                self.advance();
                let span2 = expect!(self, Local);
                self.parse_local(Declaration::MutableLocal, span.union(&span2))
            }
            Ok(_) => {
                self.advance();
                parse_error!(self, "expected statement", span);
                None
            }
            Err(message) => {
                self.advance();
                parse_error!(self, message, span);
                None
            }
        }
    }


//-------------------------------------------------------------------------------------------------

    /// Parse an expression using a Pratt Parser.
    ///
    /// <https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html> gives a great
    /// description (in Rust) of how these work.
    fn parse_priority_expr(&mut self, min_priority: u8) -> Result<Expr, Span> {
        use Token::*;

        let (token, span) = self.lookahead(0);
        let mut lhs = match token {
            Ok(Number(value)) => {
                self.advance();
                Ok(Expr::number(value, span))
            }
            Ok(Identifier(name)) => {
                self.advance();
                Ok(Expr::identifier(name, span))
            }
            Ok(LeftParenthesis) => {
                self.advance();
                let lhs = self.parse_priority_expr(0);
                expect!(self, RightParenthesis, "to match opening '(' here:", span);
                lhs
            }

            // If we don't get a number, an identifier or a opening parenthesis, it's an error, but
            // we'll spend a bit of time figuring out what the problem is and being informative
            // about it.
            Ok(token) => {
                parse_error!(self, format!("expected number or identifier, got '{token}'"), span);
                Err(span.first())
            }
            Err(message) => {
                self.advance();
                parse_error!(self, message, span);
                Err(span)
            }
        };

        while let Ok(op) = self.lookahead(0).0 {
            let operator = match BinaryOperator::try_from(op) {
                Err(()) => break,
                Ok(operator) => operator,
            };

            let (left_priority, right_priority) = operator.priority();
            if left_priority < min_priority {
                break;
            }

            self.advance();
            let rhs = self.parse_priority_expr(right_priority);
            lhs = lhs.and_then(|lhs| rhs.map(|rhs| Expr::binary(operator, lhs, rhs)));
        }

        lhs
    }


    /// Get started parsing an expression.
    ///
    /// Just calls `parse_priority_expr`, but it's handy to have a member function with no
    /// arguments.
    fn parse_expr(&mut self) -> Result<Expr, Span> { self.parse_priority_expr(0) }


    /// Parse a comma-separated list of items.
    ///
    /// Where the items are defined by `f`.
    fn parse_comma_separated_list<R, F: FnMut(&mut Self) -> R>(&mut self, mut f: F) -> Nev<R> {
        std::iter::successors(Some(f(self)), |_| {
            if self.lookahead(0).0 == Ok(Token::Comma) {
                self.advance();
                Some(f(self))
            } else {
                None
            }
        })
        .collect::<Vec<_>>().try_into().unwrap()
    }


    /// Parse a list of identifiers.
    ///
    /// This strictly parses only identifiers, so we're only expecting this after a variable
    /// declaration (local, mutable local or argument). The return vector contains the identifier
    /// (a string) and its location in the source file.
    fn parse_identifier_list(&mut self) -> Nev<(Option<String>, Span)> {
        self.parse_comma_separated_list(Self::expect_identifier)
    }


    /// Parse a list of expressions.
    ///
    /// This could be the right hand side of an assignment, a list of values to return, or just a
    /// list of expressions.  Because when we start parsing an expression list, we don't know if it
    /// might be followed by an equals (if this proves to be an assignment), then, in that case
    /// (see `parse_assignments_or_exprs` below) we might have to convert it into a list of
    /// identifiers.
    fn parse_expr_list(&mut self) -> Nev<Result<Expr, Span>> {
        self.parse_comma_separated_list(Self::parse_expr)
    }


    /// Parse 'argument' followed by a list of variable names.
    fn parse_arguments(&mut self) -> Option<Stmt> {
        let start = expect!(self, Token::Argument);
        let names: Nev<_> = self.parse_identifier_list().into_iter()
            .map(|(maybe_name, span)| Some((maybe_name?, span)))
            .collect::<Option<Vec<_>>>()?.try_into().unwrap();

        let span = start.union(&names.last().1);
        Some(Stmt::arguments(names, span))
    }


    /// Check that a list of names and of values (to assign to the names) are the same length.
    ///
    /// This is as straightforward as checking the lengths are the same, except that we want to
    /// generate error messages for any mismatches.
    fn check_assignment_lengths(
        &mut self,
        lhs:                                &Nev<(Option<String>, Span)>,
        rhs:                                &Nev<Result<Expr, Span>>) -> Result<(), ()> {
        for (maybe_name, span) in lhs.iter().skip(rhs.len()) {
            let message = maybe_name.as_ref()
                .map_or("no right-hand-side value for this left-hand-side".to_string(),
                    |name| { format!("no right-hand-side value for '{name}'") });
            let rhs_span = rhs.first().as_ref().map_or_else(|s| s, |e| e.span())
                .union(rhs.last().as_ref().map_or_else(|s| s, |e| e.span()));
            parse_error!(self, message, *span, "the right hand side is here:", rhs_span);
        }
        for result in rhs.iter().skip(lhs.len()) {
            let span = result.as_ref().map_or_else(|s| s, |e| e.span());
            let lhs_span = lhs.first().1.union(&lhs.last().1);
            parse_error!(self,
                "no left-hand-side name for this right-hand-side value", *span,
                "the left hand side is here:", lhs_span);
        }
        if lhs.len() == rhs.len() { Ok(()) } else { Err(()) }
    }


    fn just_parse_block(&mut self) -> (Vec<Stmt>, Span) {
        // Parse the block.  The caller already covered the "begin" to get the correct start point
        // (if it's an assignment, the start is at the start of the  variable list).
        let statements = self.parse_stmts();
        let end = expect!(self, Token::End);
        (statements, end)
    }


    fn get_last_exprs(&mut self, end_span: Span, stmts: &mut Vec<Stmt>)
            -> Option<Nev<Result<Expr, Span>>> {
        if let Some(last_statement) = stmts.pop() {
            if let StmtKind::Exprs(values) = last_statement.kind {
                return Some(values.into_iter().map(Ok).collect::<Vec<_>>().try_into().unwrap());
            }
            parse_error!(self,
                "the last statement in a block assignment must be a list of expressions",
                last_statement.span);
        } else {
            parse_error!(self,
                "block assignments must contain at least a list of expressions to assign",
                end_span.first());
        }
        None
    }


    /// Do the work of parsing an assignment.
    ///
    /// It could be "local a = 1" or just "a = 1".
    #[allow(clippy::single_match_else)]
    fn parse_assignment(
            &mut self,
            start:                          Span,
            declaration:                    Declaration,
            maybe_lhs:                      Nev<(Option<String>, Span)>) -> Option<Stmt> {
        let (maybe_rhs, statements, end) = match self.lookahead(0).0 {
            Ok(Token::Begin) => {
                self.advance();
                let (mut statements, end) = self.just_parse_block();
                // The last line of the block has to be a list of expressions to assign to the
                // names, and we have to check there's the same number of items on the left and
                // right-hand sides.
                let maybe_rhs = self.get_last_exprs(end, &mut statements)?;
                (maybe_rhs, statements, end)
            }
            _ => {
                let maybe_rhs = self.parse_expr_list();
                let end = *maybe_rhs.last().as_ref().map_or_else(|s| s, |e| e.span());
                (maybe_rhs, vec![], end)
            }
        };

        self.check_assignment_lengths(&maybe_lhs, &maybe_rhs).ok()?;
        let rhs = try_collect_nev(maybe_rhs)?;

        let span = start.union(&end);
        let assignments = maybe_lhs
            .into_iter()
            .zip(rhs)
            .map(|((maybe_name, span), expr)| maybe_name.map(|name| (name, span, expr)))
            .collect::<Option<Vec<_>>>()?;
        Some(Stmt::block(declaration, assignments, statements, span))
    }


    /// Parse "local a, b = 1, 2" etc, including "local a, b = begin 1, 2 end"
    fn parse_local(&mut self, declaration: Declaration, span: Span) -> Option<Stmt> {
        let lhs = self.parse_identifier_list();
        expect!(self, Token::Assign);
        self.parse_assignment(span, declaration, lhs)
    }


    /// Convert a list of expressions to a list of identifiers for the left hand side of an
    /// assignment.
    ///
    /// When we have "expr1, expr2, expr3", we could have a list of expressions or, if the
    /// expressions are all identifiers and are followed by an "=" we have an assignment.  In both
    /// cases we have to start by parsing a list of expressions, and, if we find an "=" after that,
    /// we have to convert the list of expressions to a list of identifiers. If any of the elements
    /// in the list are not identifiers (ie, they're expressions) then return None.
    fn try_make_identifier_list(
        &mut self,
        lhs: Nev<Result<Expr, Span>>,
    ) -> Nev<(Option<String>, Span)> {
        lhs.into_iter()
            .map(|r| {
                r.map_or_else(
                    |span| (None, span),
                    |e| {
                        if let ExprKind::Identifier(name) = &e.kind() {
                            (Some(name.clone()), *e.span())
                        } else {
                            // TODO: Improve error message to match rust's one with more hints.
                            parse_error!(self, "invalid left-hand side of assignment", *e.span());
                            (None, *e.span())
                        }
                    },
                )
            })
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    }


    /// Parse either a list of expressions or assignments.
    ///
    /// We can't tell until after we're seen the "=" at the end of the the left-hand side list.
    fn parse_assignments_or_exprs(&mut self) -> Option<Stmt> {
        let maybe_lhs = self.parse_expr_list();
        if self.lookahead(0).0 == Ok(Token::Assign) {
            // We got an "=".  Turn our lhs list into a list of names, and parse an
            // assignment.
            self.advance();
            let start = *maybe_lhs.first().as_ref().map_or_else(|s| s, |e| e.span());
            let maybe_lhs = self.try_make_identifier_list(maybe_lhs);
            self.parse_assignment(start, Declaration::Undeclared, maybe_lhs)
        } else {
            let lhs = try_collect_nev(maybe_lhs)?;
            let span = lhs.first().span().union(lhs.last().span());
            Some(Stmt::exprs(lhs, span))
        }
    }


    /// Parse a list of expressions or assignments.
    fn parse_exprs(&mut self) -> Option<Stmt> {
        let lhs = try_collect_nev(self.parse_expr_list())?;
        let span = lhs.first().span().union(lhs.last().span());
        Some(Stmt::exprs(lhs, span))
    }


    fn parse_return(&mut self) -> Option<Stmt> {
        let start = expect!(self, Token::Return);
        let values = try_collect_nev(self.parse_expr_list())?;
        let span = start.union(values.last().span());
        Some(Stmt::return_stmt(values, span))
    }


    /// Parse a plain block without any assignments.
    fn parse_block(&mut self) -> Stmt {
        let start = expect!(self, Token::Begin);
        let (statements, end) = self.just_parse_block();
        Stmt::block(Declaration::Undeclared, vec![], statements, start.union(&end))
    }
}


//-------------------------------------------------------------------------------------------------

#[cfg(test)]
mod test {
    use super::*;
    use std::fmt;

    fn test_parse<R, F>(f: F, input: &str, expected: &str)
    where
        R: fmt::Display,
        F: FnOnce(Parser<&str>) -> R,
    {
        let p = Parser::new(input);
        let result = format!("{}", f(p));
        assert_eq!(result, expected)
    }

    fn test_fallible_parse<R, F>(f: F, input: &str, expected: &str)
    where
        R: fmt::Display,
        F: FnOnce(Parser<&str>) -> Option<R>,
    {
        let p = Parser::new(input);
        if let Some(n) = f(p) {
            let result = format!("{}", n);
            assert_eq!(result, expected)
        } else {
            assert_eq!(expected, "#parse error#")
        }
    }

    fn display_parse_result(p: Parser<&str>, stmts: Vec<Stmt>) -> String {
        let (errors, ..) = p.close();
        if errors.is_empty() {
            stmts.iter().map(|s| format!("{}", s)).collect::<Vec<_>>().join("\n")
        } else {
            errors.iter().map(|e| e.messages()).collect::<Vec<_>>().join("\n")
        }
    }

    #[test]
    fn test_math_operators() {
        let test =
            |i, e| test_parse(&|mut p: Parser<&str>| p.parse_priority_expr(0).unwrap(), i, e);
        test("1 + 2", "(1 + 2)");
        test("1 - 2", "(1 - 2)");
        test("1 * 2", "(1 * 2)");
        test("1 / 2", "(1 / 2)");
        test("1 ^ 2", "(1 ^ 2)");
    }

    #[test]
    fn test_comparison_operators() {
        let test =
            |i, e| test_parse(&|mut p: Parser<&str>| p.parse_priority_expr(0).unwrap(), i, e);
        test("1 == 2", "(1 == 2)");
        test("1 != 2", "(1 != 2)");
        test("1 < 2", "(1 < 2)");
        test("1 <= 2", "(1 <= 2)");
        test("1 > 2", "(1 > 2)");
        test("1 >= 2", "(1 >= 2)");
    }

    #[test]
    fn test_exprs() {
        let test =
            |i, e| test_parse(&|mut p: Parser<&str>| p.parse_priority_expr(0).unwrap(), i, e);
        test("2.0", "2");
        test("2.0 + 1", "(2 + 1)");
        test("1.0 + (2.0 + 3.0)", "(1 + (2 + 3))");
        test("(1.0 + 2.0) + 3.0", "((1 + 2) + 3)");
        test("1.0 + 2.0 + 3.0", "(1 + (2 + 3))");
        test("1.0 * 2.0 + 3.0", "((1 * 2) + 3)");
        test("1.0 + 2.0 * 3.0", "(1 + (2 * 3))");
        test("1.0 + 2.0 ^ 3.0", "(1 + (2 ^ 3))");
        test("1.0 ^ 2.0 * 3.0", "((1 ^ 2) * 3)");
    }

    #[test]
    fn test_single_statement() {
        let test =
            |i, e| test_fallible_parse(&|mut p: Parser<&str>| p.parse_stmt().map(|s| s), i, e);
        test("1 + 2", "(1 + 2)");
        test("local a = 3", "local a = 3");
        test("local a = 1 + 2", "local a = (1 + 2)");
    }

    #[test]
    fn test_declarations() {
        let test =
            |i, e| test_fallible_parse(&|mut p: Parser<&str>| p.parse_stmt().map(|s| s), i, e);
        test("local a = 3", "local a = 3");
        test("mutable local a = 3", "mutable local a = 3");
        test("a = 3", "a = 3");
    }

    #[test]
    fn test_statements() {
        let test = |i, e| {
            test_parse(
                &|mut p: Parser<&str>| {
                    let r = p.parse_stmts();
                    display_parse_result(p, r)
                },
                i,
                e,
            )
        };
        test("local a = 3\nlocal b = a + 2", "local a = 3\nlocal b = (a + 2)");
        test("local a = 3\nlocal b = c + 2", "local a = 3\nlocal b = (c + 2)");
        test("local a = 3\na = 4", "local a = 3\na = 4");
        test("local a = 3\nb = 4", "local a = 3\nb = 4");
        test("mutable local a = 3\na = 4", "mutable local a = 3\na = 4");
        test("local a = 3\nreturn a + 4", "local a = 3\nreturn (a + 4)");
        test(
            "mutable local a = 3\na = 4\nlocal b = a + 3",
            "mutable local a = 3\na = 4\nlocal b = (a + 3)",
        );
    }

    #[test]
    fn test_multiple_items() {
        let test = |i, e| {
            test_parse(
                &|mut p: Parser<&str>| {
                    let r = p.parse_stmts();
                    display_parse_result(p, r)
                },
                i,
                e,
            )
        };
        test("argument a, b", "argument a, b");
        test("return 1 + 2, 3 * 4", "return (1 + 2), (3 * 4)");
        test("argument a, a, b", "argument a, a, b");
        test("local a = 1 return a + 1, a + 2", "local a = 1\nreturn (a + 1), (a + 2)");
    }

    #[test]
    fn test_comments() {
        let test =
            |i, e| test_parse(&|mut p: Parser<&str>| p.parse_priority_expr(0).unwrap(), i, e);
        test("2.0 # comment\n + 3.0", "(2 + 3)");
        test("2.0 #(comment#) + 3.0", "(2 + 3)");
    }

    #[test]
    fn test_block() {
        let test =
            |i, e| test_fallible_parse(&|mut p: Parser<&str>| p.parse_stmt().map(|s| s), i, e);
        test("begin\nlocal a = 3\nend", "begin\n  local a = 3\nend");
        test("begin begin local a = 3 end end", "begin\n  begin\n    local a = 3\n  end\nend");
    }
}

//-------------------------------------------------------------------------------------------------
