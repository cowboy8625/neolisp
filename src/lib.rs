/// Function Operators
const OPERATORS: &[&str] = &[
    "+", "-", "*", "/", "=", ">", "<", ">=", "<=", "and", "or", "not", "mod",
];

/// Builtins functions
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
    "type?",
    "print",
    "input",
    "length",
    "assert-eq",
    "assert",
    "list",
    "cons",
    "car",
    "string-trim-left",
    "string-trim-right",
    "string-trim",
];

/// Keywords that are not builtins functions, but are
/// more of a compile time functionality
/// i.e. `ffi-bind`
const KEYWORDS: &[&str] = &[
    "var", "set", "let", "fn", "if", "lambda", "loop", "quote", "test", "ffi-bind", "struct",
];

pub mod ast;
pub mod builtin;
pub mod cli;
pub mod compiler;
pub mod debugger;
pub mod docs;
pub mod emitter;
pub mod error;
pub mod expr_walker;
pub mod instruction;
pub mod machine;
pub mod parser;
pub mod repl;
pub mod symbol_table;
#[cfg(test)]
mod tests;
pub mod widgets;
