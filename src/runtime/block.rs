use std::fmt;
use std::sync::Arc;

use anyhow::bail;

use crate::core::{ParseError, SourceMap};
use crate::ast;
use crate::env;
use crate::vir;
use crate::codegen::{self, scheduler, assembler, Signature, Pool, CompiledFn};


//-------------------------------------------------------------------------------------------------

pub trait Observer {
    fn source_map(&mut self, _map: Arc<SourceMap>) {}
    fn stmts(&mut self, _stmts: &[ast::Stmt]) {}
    fn vir(&mut self, _vir: &vir::Block) {}
    fn signature(&mut self, _sig: &[vir::TypeInfo]) {}
    fn schedule(&mut self, _instrs: &scheduler::Block, _signature: &Signature, _pool: &Pool) {}
    fn assembly(&mut self, _block: &assembler::Block, _pool: &Pool) {}
    fn func(&mut self, _func: &CompiledFn) {}
    fn safe_to_run(&self) -> bool { true }
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
    let diagnostics = |errors: Vec<ParseError>| Diagnostics { errors, source: source_map.clone() };
    if !errors.is_empty() { return Err(diagnostics(errors)); }

    observer.source_map(source_map.clone());
    observer.stmts(&stmts);

    let env = env::Env::new();
    let vir_block = vir::run(&env, &stmts).map_err(diagnostics)?;
    observer.vir(&vir_block);

    // Run type inference without any argument types.
    // This way we see any errors we can find at this point nice and early.
    vir::check_types(&vir_block, &env).map_err(diagnostics)?;

    Ok(Block{
        env,
        vir:                vir_block,
        source:             source_map,
        funcs:              vec![],
    })
}


//-------------------------------------------------------------------------------------------------

#[derive(derive_more::Display)]
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


impl std::str::FromStr for Value {
    type Err = anyhow::Error;

    fn from_str(s: &str) -> anyhow::Result<Self> {
        match s {
            "true"  => Ok(Self::Bool(true)),
            "false" => Ok(Self::Bool(false)),
            _       => s.parse::<f64>().map(Self::F64)
                .or_else(|_| bail!("'{s}' is not a valid value (expected a number or a bool)")),
        }
    }
}


//-------------------------------------------------------------------------------------------------

pub struct Block {
    env:                env::Env,
    vir:                vir::Block,
    source:             Arc<SourceMap>,
    funcs:              Vec<(Vec<vir::TypeInfo>, CompiledFn)>,
}


impl Block {
    pub fn call<O: Observer>(
        &mut self,
        args:               &[Value],
        observer:           &mut O,
    ) -> anyhow::Result<Vec<Value>> {
        if args.len() != self.vir.args.len() {
            bail!("wrong number of arguments: expected {}, got {}",
                self.vir.args.len(), args.len())
        }
        let signature: Vec<vir::TypeInfo> = args.iter().map(Into::into).collect();
        if let Some((_, func)) = self.funcs.iter().find(|(sig, _)| *sig == signature) {
            return func.call(args);
        }

        // Observers only get called when we actually have to compile something, not every
        // time it's called.
        observer.signature(&signature);
        let types = vir::infer_types(&self.vir, &signature, &self.env)
            .map_err(|errors| Diagnostics { errors, source: self.source.clone() })?;
        let func = codegen::run(&self.vir, &types, observer);
        if !observer.safe_to_run() { bail!("not run: disassembly doesn't match intended assembly") }
        let results = func.call(args);
        self.funcs.push((signature, func));
        results
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
