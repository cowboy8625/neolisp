use super::{Callee, Function, IState, Stage1Compiler, Stage1Instruction::*, Value};
use crate::parser::parser;
use crate::symbol_table::SymbolWalker;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

#[test]
fn test_main_var() {
    let src = "
(var num (+ 1 2))
(fn main () (print num))
";
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let mut stage1_compiler = Stage1Compiler::new(symbol_table);
    stage1_compiler.compiler(&ast);
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, vec![], "main function params");
    assert_eq!(
        main.prelude,
        vec![
            Push(Value::F64(1.0)),
            Push(Value::F64(2.0)),
            Add,
            LoadGlobal,
        ],
        "main function prelude"
    );
    assert_eq!(
        main.body,
        vec![
            GetGlobal(IState::Set(0)),
            Call(Callee::Builtin("print".to_string()), 1),
            Halt,
        ],
        "main function body"
    );
}

#[test]
fn test_main_var_lambda() {
    let src = "
(var add (lambda (x y) (+ x y)))
(fn main () (print (add 123 321)))
";
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let mut stage1_compiler = Stage1Compiler::new(symbol_table);
    stage1_compiler.compiler(&ast);
    // Checking function
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, vec![], "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        vec![
            Push(Value::Callable(IState::Unset("lambda_0".to_string()))),
            LoadGlobal
        ],
        "main function prelude"
    );
    assert_eq!(
        main.body,
        vec![
            Push(Value::F64(123.0)),
            Push(Value::F64(321.0)),
            GetGlobal(IState::Set(0)),
            Call(Callee::Function, 2),
            Call(Callee::Builtin("print".to_string()), 1),
            Halt,
        ],
        "main function body"
    );

    // Check that we have a lambda
    assert_eq!(stage1_compiler.lambdas.len(), 1, "one lambda");
    let lambda = &stage1_compiler.lambdas[0];
    assert_eq!(lambda.name, "lambda_0", "lambda name");
    assert_eq!(
        lambda.params,
        vec![Rot, LoadLocal, Rot, LoadLocal,],
        "lambda params"
    );
    assert_eq!(
        lambda.body,
        vec![GetLocal(IState::Set(0)), GetLocal(IState::Set(1)), Add,],
        "lambda body"
    );
}
