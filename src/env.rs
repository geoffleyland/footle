use std::collections::BTreeMap;

use crate::vir::TypeInfo;
use crate::vir::TypeInfo::F64;


pub struct Env {
    pub module:         Module,
}

impl Env {
    pub fn new() -> Self { Self { module: Module::new()} }
}


pub struct Module {
    pub functions:      BTreeMap<String, FunctionDef>,
}

impl Module {
    fn new() -> Self {
        Self {
            functions: BTreeMap::from(
                FUNCTIONS.map(|(n, a, r, f)| (n.to_string(),
                    FunctionDef{ arg_types: a.to_vec(), result_types: r.to_vec(), const_fold: f}))
            ),
        }
    }
}

type ConstFnFolder = fn(&[f64]) -> f64;

pub struct FunctionDef {
    pub arg_types:          Vec<TypeInfo>,
    pub result_types:       Vec<TypeInfo>,
    pub const_fold:         Option<ConstFnFolder>,
}

impl FunctionDef {
    // FIXME: any functions we can fold are pure... for now.
    pub fn is_pure(&self) -> bool { self.const_fold.is_some() }
}


#[allow(clippy::type_complexity)]
const FUNCTIONS: [(&str, &[TypeInfo], &[TypeInfo], Option<ConstFnFolder>); 1] = [
    ("sin", &[F64], &[F64], Some(|args| args[0].sin()))
];
