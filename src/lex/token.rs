use std::fmt;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Identifier(String),
    Number(f64),

    // Math Operators
    Plus,
    Minus,
    Times,
    Divide,
    Power,

    // Comparison Operators
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,

    // Other expression parts
    Assign,
    LeftParenthesis,
    RightParenthesis,

    // Punctuation
    Comma,

    // Keywords
    Local,
    Mutable,
    Argument,
    Begin,
    End,
    Return,
    Eof,

    // Skipped Tokens
    Comment,
    BlockComment,
}


impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Token::*;
        match self {
            Identifier(v)                   => write!(f, "{v}"),
            Number(v)                       => write!(f, "{v}"),

            // Math operators
            Plus                            => write!(f, "+"),
            Minus                           => write!(f, "-"),
            Times                           => write!(f, "*"),
            Divide                          => write!(f, "/"),
            Power                           => write!(f, "^"),

            // Comparison operators
            Equal                           => write!(f, "=="),
            NotEqual                        => write!(f, "!="),
            LessThan                        => write!(f, "<"),
            LessEqual                       => write!(f, "<="),
            GreaterThan                     => write!(f, ">"),
            GreaterEqual                    => write!(f, ">="),

            // Other expression parts
            Assign                          => write!(f, "="),
            LeftParenthesis                 => write!(f, "("),
            RightParenthesis                => write!(f, ")"),

            // Punctuation
            Comma                           => write!(f, ","),

            // Keywords
            Local                           => write!(f, "local"),
            Mutable                         => write!(f, "mutable"),
            Argument                        => write!(f, "argument"),
            Begin                           => write!(f, "begin"),
            End                             => write!(f, "end"),
            Return                          => write!(f, "return"),
            Eof                             => write!(f, "EOF"),

            // Skipped Tokens
            Comment | BlockComment => write!(f, " "),
        }
    }
}


//-------------------------------------------------------------------------------------------------

pub fn match_reserved_word(word: &str) -> Option<Token> {
    use Token::*;
    match word {
        "local"                             => Some(Local),
        "mutable"                           => Some(Mutable),
        "argument"                          => Some(Argument),
        "begin"                             => Some(Begin),
        "end"                               => Some(End),
        "return"                            => Some(Return),
        _                                   => None
    }
}


//-------------------------------------------------------------------------------------------------
