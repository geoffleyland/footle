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
            Multiply | Divide           => (11, 11),
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

            // Comparison operators
            Equal                       => "==",
            NotEqual                    => "!=",
            LessThan                    => "<",
            LessEqual                   => "<=",
            GreaterThan                 => ">",
            GreaterEqual                => ">=",
        }
    }


    #[allow(clippy::float_cmp)]
    pub fn eval_constants(self, lhs: f64, rhs: f64) -> f64 {
        use BinaryOperator::*;
        match self {
            // Math operators
            Add                             => lhs + rhs,
            Subtract                        => lhs - rhs,
            Multiply                        => lhs * rhs,
            Divide                          => lhs / rhs,
            Power                           => f64::powf(lhs, rhs),

            // Comparison operators
            Equal                           => if lhs == rhs { 1.0 } else { 0.0 },
            NotEqual                        => if lhs == rhs { 0.0 } else { 1.0 },
            LessThan                        => if lhs < rhs  { 1.0 } else { 0.0 },
            LessEqual                       => if lhs <= rhs { 1.0 } else { 0.0 },
            GreaterThan                     => if lhs > rhs  { 1.0 } else { 0.0 },
            GreaterEqual                    => if lhs >= rhs { 1.0 } else { 0.0 },
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


#[cfg(test)]

mod test {
    use super::*;

    #[test]
    fn test_eval_constants() {
        use BinaryOperator::*;

        // Math operators
        assert_eq!(Add.eval_constants(3.0, 4.0),      7.0);
        assert_eq!(Subtract.eval_constants(3.0, 4.0), -1.0);
        assert_eq!(Multiply.eval_constants(3.0, 4.0), 12.0);
        assert_eq!(Divide.eval_constants(9.0, 3.0),   3.0);
        assert_eq!(Power.eval_constants(2.0, 10.0),   1024.0);

        // Comparison operators — return 1.0 (true) or 0.0 (false)
        assert_eq!(Equal.eval_constants(1.0, 1.0),        1.0);
        assert_eq!(Equal.eval_constants(1.0, 2.0),        0.0);
        assert_eq!(NotEqual.eval_constants(1.0, 2.0),     1.0);
        assert_eq!(NotEqual.eval_constants(1.0, 1.0),     0.0);
        assert_eq!(LessThan.eval_constants(1.0, 2.0),     1.0);
        assert_eq!(LessThan.eval_constants(2.0, 1.0),     0.0);
        assert_eq!(LessEqual.eval_constants(1.0, 1.0),    1.0);
        assert_eq!(LessEqual.eval_constants(2.0, 1.0),    0.0);
        assert_eq!(GreaterThan.eval_constants(2.0, 1.0),  1.0);
        assert_eq!(GreaterThan.eval_constants(1.0, 2.0),  0.0);
        assert_eq!(GreaterEqual.eval_constants(1.0, 1.0), 1.0);
        assert_eq!(GreaterEqual.eval_constants(1.0, 2.0), 0.0);

        // a > b  ≡  b < a  (not b <= a)
        assert_eq!(1.0_f64 > 2.0, LessThan.eval_constants(2.0, 1.0) != 0.0);
        assert_eq!(2.0_f64 > 1.0, LessThan.eval_constants(1.0, 2.0) != 0.0);
        assert_eq!(1.0_f64 > 1.0, LessThan.eval_constants(1.0, 1.0) != 0.0);

        // a >= b  ≡  b <= a  (not b < a)
        assert_eq!(1.0_f64 >= 2.0, LessEqual.eval_constants(2.0, 1.0) != 0.0);
        assert_eq!(2.0_f64 >= 1.0, LessEqual.eval_constants(1.0, 2.0) != 0.0);
        assert_eq!(1.0_f64 >= 1.0, LessEqual.eval_constants(1.0, 1.0) != 0.0);
    }
}

//-------------------------------------------------------------------------------------------------
