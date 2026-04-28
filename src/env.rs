use std::collections::HashMap;


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
                FUNCTIONS.map(|(n, a, f)| (n.to_string(),
                    FunctionDef{ arguments: a, const_fold: f}))
            ),
        }
    }
}

type ConstFnFolder = fn(&[f64]) -> f64;

pub struct FunctionDef {
    pub arguments:      u8,
    pub const_fold:     Option<ConstFnFolder>,
}

const FUNCTIONS: [(&str, u8, Option<ConstFnFolder>); 1] = [
    ("sin", 1, Some(|args| args[0].sin()))
];
