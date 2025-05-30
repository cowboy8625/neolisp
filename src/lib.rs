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
    "print-fmt",
    "string->number",
    "random-int",
    "max",
    "min",
    "floor",
];

/// Keywords that are not builtins functions, but are
/// more of a compile time functionality
/// i.e. `ffi-bind`
const KEYWORDS: &[&str] = &[
    "var", "set", "let", "fn", "if", "lambda", "loop",
    // TODO: move quote to macro expansion function instead.  I dont think it should be in here.
    // also changed the name to __QUOTE__ in parser.
    "quote", "test", "ffi-bind", "struct", "return",
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
