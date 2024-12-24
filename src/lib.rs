const OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "=", ">", "<", ">=", "<=", "and", "or", "not", "mod",
];
const BUILTINS: &[&str] = &[
    "sleep",
    "atom?",
    "number?",
    "slice",
    "join",
    "split",
    "to-string",
    "filter",
    "fold-right",
    "fold",
    "map",
    "nth",
    "reverse",
    "append",
    "last",
    "cdr",
    "typeof",
    "print",
    "length",
    "assert-eq",
    "assert",
    "list",
    "cons",
    "car",
];
const KEYWORDS: &[&str] = &["var", "let", "fn", "if", "lambda", "loop", "quote", "test"];

pub mod ast;
pub mod cli;
pub mod compiler;
pub mod debugger;
pub mod docs;
pub mod emitter;
pub mod error;
pub mod expr_walker;
pub mod instruction;
pub mod intrinsic;
pub mod machine;
pub mod parser;
pub mod repl;
pub mod symbol_table;
#[cfg(test)]
mod tests;
