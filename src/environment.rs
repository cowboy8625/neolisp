use crate::builtins::*;
use crate::parser::Expr;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Env {
    pub data: HashMap<String, Expr>,
    pub outer: Option<Box<Env>>,
}

macro_rules! init_builtin {
    ($doc:ident, $symbol:expr, $name:ident) => {
        (
            $symbol.to_string(),
            crate::parser::Expr::Builtin($name, $doc.get($symbol).unwrap().to_string()),
        )
    };
    ($doc:ident, $name:ident) => {
        (
            stringify!($name).to_string(),
            Expr::Builtin($name, $doc.get(stringify!($name)).unwrap().to_string()),
        )
    };
}

impl Env {
    pub fn new() -> Self {
        let docs = load_doc();
        let data = HashMap::from([
            init_builtin!(docs, "+", add),
            init_builtin!(docs, "-", sub),
            init_builtin!(docs, "=", eq),
            init_builtin!(docs, ">", gt),
            init_builtin!(docs, "<", lt),
            init_builtin!(docs, ">=", gte),
            init_builtin!(docs, "<=", lte),
            init_builtin!(docs, "typeof", type_of), // typeof is a keyword in rust
            init_builtin!(docs, and),
            init_builtin!(docs, or),
            init_builtin!(docs, not),
            init_builtin!(docs, help),
            init_builtin!(docs, print),
            init_builtin!(docs, list),
            init_builtin!(docs, cons),
            init_builtin!(docs, car),
            init_builtin!(docs, cdr),
            init_builtin!(docs, append),
            init_builtin!(docs, reverse),
            init_builtin!(docs, nth),
            init_builtin!(docs, length),
            init_builtin!(docs, map),
            init_builtin!(docs, fold),
        ]);
        Self { data, outer: None }
    }
}
