use std::fmt;
use std::sync::Arc;

use crate::core::{ParseError, SourceMap};
use crate::ast;
use crate::env;
use crate::vir;
use crate::codegen;


//-------------------------------------------------------------------------------------------------

pub trait Observer {
    fn source_map(&mut self, _map: Arc<SourceMap>) {}
    fn stmts(&mut self, _stmts: &[ast::Stmt]) {}
    fn vir(&mut self, _vir: &vir::Block) {}
    fn schedule(&mut self, _block: &codegen::scheduler::Block) {}
    fn assembler(&mut self, _block: &codegen::assembler::Block) {}
    fn func(&mut self, _func: &codegen::CompiledFn) {}
}

pub struct Silent;
impl Observer for Silent {}

//-------------------------------------------------------------------------------------------------

pub fn load(file_name: &str, source: String) -> Result<Block, Diagnostics> {
    load_observed(file_name, source, &mut Silent{})
}


pub fn load_observed<O: Observer>(
    file_name:              &str,
    source:                 String,
    observer:               &mut O,
) -> Result<Block, Diagnostics> {
    let (stmts, errors, source_map) = ast::parse(file_name, source);
    let source_map = Arc::new(source_map);
    if !errors.is_empty() {
        #[allow(clippy::redundant_clone)]
        return Err(Diagnostics { errors, source: source_map.clone() });
    }

    observer.source_map(source_map.clone());
    observer.stmts(&stmts);

    let env = env::Env::new();
    let vir_block = match vir::run(&env, &stmts) {
        Ok(block) => block,
        Err(errors) => {
            #[allow(clippy::redundant_clone)]
            return Err(Diagnostics { errors, source: source_map.clone() });
        }
    };
    observer.vir(&vir_block);

    let types = match vir::infer_types(&vir_block.exprs, &vir_block.arguments, &vir_block.reassignments) {
        Ok(types) => types,
        Err(errors) => {
            #[allow(clippy::redundant_clone)]
            return Err(Diagnostics { errors, source: source_map.clone() });
        }
    };

    Ok(Block{
        types,
        vir:                vir_block,
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


impl std::str::FromStr for Value {
    type Err = ParseValueError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true"  => Ok(Self::Bool(true)),
            "false" => Ok(Self::Bool(false)),
            _       => s.parse::<f64>().map(Self::F64).map_err(|_| ParseValueError(s.to_string())),
        }
    }
}

#[derive(Debug)]
pub struct ParseValueError(String);

impl fmt::Display for ParseValueError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "'{}' is not a valid value (expected a number or a bool)", self.0)
    }
}

impl std::error::Error for ParseValueError {}


//-------------------------------------------------------------------------------------------------

pub struct Block {
    pub vir:            vir::Block,
    pub types:          Vec<vir::TypeInfo>,
}


impl Block {
    pub fn call(&self, arguments: &[Value]) -> anyhow::Result<Vec<Value>> {
        self.call_observed(arguments, &mut Silent{})
    }

    pub fn call_observed<O: Observer>(
    &self,
    arguments:          &[Value],
    observer:           &mut O,
) -> anyhow::Result<Vec<Value>> {
        let func = codegen::run(&self.vir, &self.types, observer);
        observer.func(&func);
        let results = func.call(arguments)?;
        Ok(results)
    }
}


//-------------------------------------------------------------------------------------------------

#[derive(Debug)]
pub struct Diagnostics {
    pub errors:         Vec<ParseError>,
    source:             Arc<SourceMap>,
}

impl std::error::Error for Diagnostics {}

impl fmt::Display for Diagnostics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for e in &self.errors {
            write!(f, "{}", e.show_in_source(&self.source))?;
        }
        Ok(())
    }
}


//-------------------------------------------------------------------------------------------------
