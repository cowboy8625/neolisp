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
    "nth",
    "length",
    "assert-eq",
    "assert",
    "list",
    "cons",
    "car",
];
const KEYWORDS: &[&str] = &["var", "let", "fn", "if", "lambda", "loop"];

pub mod ast;
pub mod cli;
pub mod compiler;
pub mod docs;
pub mod error;
pub mod expr_walker;
pub mod parser;
pub mod repl;
pub mod symbol_table;
#[cfg(test)]
mod tests;
pub mod vm;
