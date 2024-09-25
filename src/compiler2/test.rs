use super::{IState, Stage1Callee, Stage1Compiler, Stage1Instruction::*, Stage1Value};
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
    let stage1_compiler = Stage1Compiler::new(symbol_table).compiler(&ast);
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, vec![], "main function params");
    assert_eq!(
        main.prelude,
        vec![
            Push(Stage1Value::F64(1.0)),
            Push(Stage1Value::F64(2.0)),
            Add,
            LoadGlobal,
        ],
        "main function prelude"
    );
    assert_eq!(
        main.body,
        vec![
            GetGlobal(IState::Set(0)),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
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
    let stage1_compiler = Stage1Compiler::new(symbol_table).compiler(&ast);
    // Checking function
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, vec![], "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        vec![
            Push(Stage1Value::Callable(IState::Unset("lambda_0".to_string()))),
            LoadGlobal
        ],
        "main function prelude"
    );
    assert_eq!(
        main.body,
        vec![
            Push(Stage1Value::F64(123.0)),
            Push(Stage1Value::F64(321.0)),
            GetGlobal(IState::Set(0)),
            Call(Stage1Callee::Function, 2),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
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
        vec![
            GetLocal(IState::Set(0)),
            GetLocal(IState::Set(1)),
            Add,
            Rot,
            Return
        ],
        "lambda body"
    );
}

#[test]
fn test_main_var_lambda_curry() {
    let src = "
(var add (lambda (x) (lambda (y) (+ x y))))
(fn main () (print ((add 123) 321)))
";
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let stage1_compiler = Stage1Compiler::new(symbol_table).compiler(&ast);
    // Checking function
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, vec![], "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        vec![
            Push(Stage1Value::Callable(IState::Unset("lambda_0".to_string()))),
            LoadGlobal
        ],
        "main function prelude"
    );
    assert_eq!(
        main.body,
        vec![
            Push(Stage1Value::F64(321.0)),
            Push(Stage1Value::F64(123.0)),
            GetGlobal(IState::Set(0)),
            Call(Stage1Callee::Function, 1),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
            Halt,
        ],
        "main function body"
    );

    assert_eq!(stage1_compiler.lambdas.len(), 2, "two lambda");

    // Check that we have a lambda 1
    let lambda = &stage1_compiler.lambdas[1];
    assert_eq!(lambda.name, "lambda_0", "lambda_0 name");
    assert_eq!(lambda.params, vec![Rot, LoadLocal], "lambda_0 params");
    assert_eq!(
        lambda.body,
        vec![
            Push(Stage1Value::Callable(IState::Unset("lambda_1".to_string()))),
            Rot,
            Return
        ],
        "lambda_0 body"
    );

    // Check that we have a lambda 2
    let lambda = &stage1_compiler.lambdas[0];
    assert_eq!(lambda.name, "lambda_1", "lambda name");
    assert_eq!(lambda.params, vec![Rot, LoadLocal], "lambda params");
    assert_eq!(
        lambda.body,
        vec![
            GetFree(IState::Set(0)),
            GetLocal(IState::Set(1)),
            Add,
            Rot,
            Return
        ],
        "lambda_1 body"
    );
}
