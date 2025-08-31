use std::fmt;

use crate::lex::Token;

//-------------------------------------------------------------------------------------------------

#[derive(Debug, Clone, Copy)]
pub enum Declaration {
    Local,
    MutableLocal,
    Undeclared,
}


impl std::fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use Declaration::*;
        let declaration = match self {
            Local                   => &format!("{} ", Token::Local),
            MutableLocal            => &format!("{} {} ", Token::Mutable, Token::Local),
            Undeclared              => "",
        };
        write!(f, "{declaration}")
    }
}


//-------------------------------------------------------------------------------------------------
