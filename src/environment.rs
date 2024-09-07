use crate::ast::{Expr, Spanned};
use crate::builtins::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    pub data: HashMap<String, Spanned<Expr>>,
    pub outer: Option<Box<Env>>,
}

macro_rules! init_builtin {
    ($doc:ident, $symbol:expr, $name:ident) => {
        (
            $symbol.to_string(),
            Spanned {
                expr: crate::ast::Expr::Builtin(
                    $name,
                    $doc.get($symbol)
                        .expect(&format!("missing docs for builtin function: {}", $symbol))
                        .to_string(),
                ),
                span: 0..0,
            },
        )
    };
    ($doc:ident, $name:ident) => {
        (
            stringify!($name).to_string(),
            Spanned {
                expr: Expr::Builtin(
                    $name,
                    $doc.get(stringify!($name))
                        .expect(&format!(
                            "missing docs for builtin function: {}",
                            stringify!($name)
                        ))
                        .to_string(),
                ),
                span: 0..0,
            },
        )
    };
}

impl Env {
    pub fn new() -> Self {
        let docs = load_doc();
        let data = HashMap::from([
            init_builtin!(docs, "+", add),
            init_builtin!(docs, "-", sub),
            init_builtin!(docs, "*", mul),
            init_builtin!(docs, "/", div),
            init_builtin!(docs, "mod", r#mod),
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
            init_builtin!(docs, filter),
            init_builtin!(docs, "assert", assertnl),
            init_builtin!(docs, "assert-eq", assert_eqnl),
            init_builtin!(docs, "loop", r#loop),
            init_builtin!(docs, sleep),
            init_builtin!(docs, "to-string", to_string),
            init_builtin!(docs, "split", split_string),
            init_builtin!(docs, "number?", is_number),
            init_builtin!(docs, "do", do_statement),
            init_builtin!(docs, slice),
        ]);
        Self { data, outer: None }
    }
}
