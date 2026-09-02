use std::fmt;

use crate::lex::Token;


//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinaryOperator {
    // Math operators
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Modulo,

    // Comparison operators
    Equal,
    NotEqual,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
}


impl BinaryOperator {
    pub const fn priority(self) -> (u8, u8) {
        use BinaryOperator::*;
        match self {
            // Copied from https://github.com/lua/lua/blob/master/lparser.c

            // Math operators
            Add | Subtract              => (10, 10),
            Multiply | Divide | Modulo  => (11, 11),
            Power                       => (14, 13), // Right associative

            Equal | NotEqual | LessThan | LessEqual | GreaterThan | GreaterEqual
                                        => (3, 3),
        }
    }


    fn as_str(&self) -> &str {
        use BinaryOperator::*;
        match self {
            // Math operators
            Add                         => "+",
            Subtract                    => "-",
            Multiply                    => "*",
            Divide                      => "/",
            Power                       => "^",
            Modulo                      => "%",

            // Comparison operators
            Equal                       => "==",
            NotEqual                    => "!=",
            LessThan                    => "<",
            LessEqual                   => "<=",
            GreaterThan                 => ">",
            GreaterEqual                => ">=",
        }
    }


    pub const fn is_commutable(self) -> bool {
        use BinaryOperator::*;
        matches!(self, Add | Multiply | Equal | NotEqual)
    }


    pub const fn should_reverse(self) -> Option<Self> {
        use BinaryOperator::*;
        match self {
            GreaterThan                     => Some(LessThan),
            GreaterEqual                    => Some(LessEqual),
            _                               => None,
        }
    }
}


impl TryFrom<Token> for BinaryOperator {
    type Error = ();

    fn try_from(token: Token) -> Result<Self, Self::Error> {
        use Token::*;
        match token {
            // Math operators
            Plus                        => Ok(Self::Add),
            Minus                       => Ok(Self::Subtract),
            Times                       => Ok(Self::Multiply),
            Divide                      => Ok(Self::Divide),
            Power                       => Ok(Self::Power),
            Modulo                      => Ok(Self::Modulo),

            // Comparison operators
            Equal                       => Ok(Self::Equal),
            NotEqual                    => Ok(Self::NotEqual),
            LessThan                    => Ok(Self::LessThan),
            LessEqual                   => Ok(Self::LessEqual),
            GreaterThan                 => Ok(Self::GreaterThan),
            GreaterEqual                => Ok(Self::GreaterEqual),

            _                           => Err(()),
        }
    }
}


impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { f.write_str(self.as_str()) }
}

//-------------------------------------------------------------------------------------------------
