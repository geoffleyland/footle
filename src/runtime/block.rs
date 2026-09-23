use std::fmt;
use std::sync::Arc;

use crate::core::{ParseError, SourceMap};
use crate::ast;
use crate::env;
use crate::vir;
use crate::codegen;


//-------------------------------------------------------------------------------------------------

pub fn load(file_name: &str, source: String) -> Result<Block, Diagnostics> {
    let (stmts, errors, source_map) = ast::parse(file_name, source);
    let source_map = Arc::new(source_map);
    if !errors.is_empty() {
        #[allow(clippy::redundant_clone)]
        return Err(Diagnostics { errors, source: source_map.clone() });
    }

    let env = env::Env::new();
    let vir_block = match vir::run(&env, &stmts) {
        Ok(block) => block,
        Err(errors) => {
            #[allow(clippy::redundant_clone)]
            return Err(Diagnostics { errors, source: source_map.clone() });
        }
    };

    let types = match vir::infer_types(&vir_block.exprs, &vir_block.reassignments) {
        Ok(types) => types,
        Err(errors) => {
            #[allow(clippy::redundant_clone)]
            return Err(Diagnostics { errors, source: source_map.clone() });
        }
    };

    Ok(Block{
        types,
        vir:                vir_block,
        #[cfg(feature = "dogfood")]
        stmts,
        #[cfg(feature = "dogfood")]
        source:             source_map,
    })
}


//-------------------------------------------------------------------------------------------------

pub enum Value {
    Bool(bool),
    F64(f64),
}


impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Bool(b)           => write!(f, "{b}"),
            Self::F64(v)            => write!(f, "{v}"),
        }
    }
}


//-------------------------------------------------------------------------------------------------

pub struct Block {
    pub vir:            vir::Block,
    pub types:          Vec<vir::TypeInfo>,

    #[cfg(feature = "dogfood")]
    pub stmts:          Vec<ast::Stmt>,
    #[cfg(feature = "dogfood")]
    pub source:         Arc<SourceMap<String>>,
}


impl Block {
    pub fn call(&self, arguments: &[f64]) -> anyhow::Result<Vec<Value>> {
        let func = codegen::run(&self.vir, &self.types);
        let results = func.call(arguments)?;
        Ok(results)
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Diagnostics {
    pub errors:         Vec<ParseError>,
    source:             Arc<SourceMap<String>>,
}

impl std::error::Error for Diagnostics {}

impl fmt::Display for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for e in &self.errors {
            write!(f, "{}", e.show_in_source(&*self.source))?;
        }
        Ok(())
    }
}


//-------------------------------------------------------------------------------------------------
