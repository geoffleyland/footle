use std::collections::{HashMap, hash_map::Entry};
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
    fn signature(&mut self, _sig: &[vir::TypeInfo]) {}
    fn schedule(&mut self, _block: &codegen::scheduler::Block) {}
    fn assembly(&mut self, _block: &codegen::assembler::Block) {}
    fn func(&mut self, _func: &codegen::CompiledFn) {}
}

pub struct Silent;
impl Observer for Silent {}

//-------------------------------------------------------------------------------------------------

pub fn load<O: Observer>(
    file_name:              &str,
    source:                 String,
    observer:               &mut O,
) -> Result<Block, Diagnostics> {
    let (stmts, errors, source_map) = ast::parse(file_name, source);
    let source_map = Arc::new(source_map);
    if !errors.is_empty() {
        return Err(Diagnostics { errors, source: source_map });
    }

    observer.source_map(source_map.clone());
    observer.stmts(&stmts);

    let env = env::Env::new();
    let vir_block = match vir::run(&env, &stmts) {
        Ok(block) => block,
        Err(errors) => {
            return Err(Diagnostics { errors, source: source_map });
        }
    };
    observer.vir(&vir_block);

    let argument_types = vec![vir::TypeInfo::Unknown; vir_block.arguments.len()];
    // match vir::infer_types(&vir_block.exprs,
    //     &vir_block.arguments, &argument_types, &vir_block.reassignments) {
    match vir::infer_types(&vir_block, &argument_types) {
        Ok(..) => {},
        Err(errors) => {
            return Err(Diagnostics { errors, source: source_map });
        }
    }

    Ok(Block{
        vir:                vir_block,
        source:             source_map,
        funcs:              HashMap::new(),
    })
}


//-------------------------------------------------------------------------------------------------

pub enum Value {
    Bool(bool),
    F64(f64),
}


impl From<&Value> for vir::TypeInfo {
    fn from(v: &Value) -> Self {
        match v {
            Value::Bool(..)     => Self::Bool,
            Value::F64(..)      => Self::F64,
        }
    }
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
    source:             Arc<SourceMap>,
    funcs:              HashMap<Vec<vir::TypeInfo>, codegen::CompiledFn>,
}


impl Block {
    pub fn call<O: Observer>(
        &mut self,
        arguments:          &[Value],
        observer:           &mut O,
    ) -> anyhow::Result<Vec<Value>> {
        let signature: Vec<vir::TypeInfo> = arguments.iter().map(Into::into).collect();
        let func = match self.funcs.entry(signature) {
            Entry::Occupied(entry)  => entry.into_mut(),
            Entry::Vacant(entry) => {
                observer.signature(entry.key());
                let types = match vir::infer_types(&self.vir, entry.key()) {
                    Ok(types) => types,
                    Err(errors) => {
                        return Err(Diagnostics { errors, source: self.source.clone() }.into());
                    }
                };
                let func = codegen::run(&self.vir, &types, observer);
                entry.insert(func)
            }
        };
        func.call(arguments)
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
