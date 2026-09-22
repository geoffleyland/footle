use std::fmt;
use std::sync::Arc;

use anyhow::Result;

use crate::core::{ParseError, SourceMap};
use crate::ast;
use crate::env;
use crate::vir;
use crate::codegen;


//-------------------------------------------------------------------------------------------------

pub fn load(file_name: &str, source: String) -> Result<Block> {
    let (stmts, errors, source_map) = ast::parse(file_name, source);
    let source_map = Arc::new(source_map);
    if !errors.is_empty() {
        Err(Diagnostics { errors, source: source_map.clone() })?;
    }

    let env = env::Env::new();
    let (vir_block, errors) = vir::run(&env, &stmts);
    if !errors.is_empty() {
        Err(Diagnostics { errors, source: source_map.clone() })?;
    }

    Ok(Block{ _source: source_map, vir: vir_block })
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
    _source:             Arc<SourceMap<String>>,
    vir:                vir::Block,
}


impl Block {
    pub fn call(&self, arguments: &[f64]) -> Result<Vec<Value>> {
        let func = codegen::run(&self.vir);
        let results = func.call(arguments)?;
        Ok(results)
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Diagnostics {
    errors:             Vec<ParseError>,
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
