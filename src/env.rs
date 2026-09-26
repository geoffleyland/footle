use std::collections::HashMap;

use crate::vir::TypeInfo;
use crate::vir::TypeInfo::F64;


pub struct Env {
    pub module:         Module,
}

impl Env {
    pub fn new() -> Self { Self { module: Module::new()} }
}


pub struct Module {
    pub functions:      HashMap<String, FunctionDef>,
}

impl Module {
    fn new() -> Self {
        Self {
            functions: HashMap::from(
                FUNCTIONS.map(|(n, a, r, f)| (n.to_string(),
                    FunctionDef{ argument_types: a.to_vec(), result_types: r.to_vec(), const_fold: f}))
            ),
        }
    }
}

type ConstFnFolder = fn(&[f64]) -> f64;

pub struct FunctionDef {
    pub argument_types:     Vec<TypeInfo>,
    pub result_types:       Vec<TypeInfo>,
    pub const_fold:         Option<ConstFnFolder>,
}

#[allow(clippy::type_complexity)]
const FUNCTIONS: [(&str, &[TypeInfo], &[TypeInfo], Option<ConstFnFolder>); 1] = [
    ("sin", &[F64], &[F64], Some(|args| args[0].sin()))
];
