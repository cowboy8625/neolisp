use super::{
    Chunk, CompilerOptions, IState, Stage1Callee, Stage1Compiler, Stage1Instruction::*, Stage1Value,
};
use crate::parser::parser;
use crate::symbol_table::SymbolTableBuilder;
use crate::vm::Direction;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

#[test]
fn test_main_var() {
    let src = "
(var num (+ 1 2))
(fn main () (print num))
";
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_compiler =
        Stage1Compiler::new(&mut symbol_table).compile(&ast, &CompilerOptions::default());
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, Chunk::new(), "main function params");
    assert_eq!(
        main.prelude,
        Chunk::from(vec![
            Push(Stage1Value::F64(1.0)),
            Push(Stage1Value::F64(2.0)),
            Add(2),
            LoadGlobal,
        ]),
        "main function prelude"
    );
    assert_eq!(
        main.body,
        Chunk::from(vec![
            GetGlobal(IState::Set(0)),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
            Halt,
        ]),
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
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_compiler =
        Stage1Compiler::new(&mut symbol_table).compile(&ast, &CompilerOptions::default());
    // Checking function
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, Chunk::new(), "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        Chunk::from(vec![
            Push(Stage1Value::Callable(IState::Unset("lambda_0".to_string()))),
            LoadGlobal
        ]),
        "main function prelude"
    );
    assert_eq!(
        main.body,
        Chunk::from(vec![
            Push(Stage1Value::F64(321.0)),
            Push(Stage1Value::F64(123.0)),
            GetGlobal(IState::Set(0)),
            Call(Stage1Callee::Function, 2),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
            Halt,
        ]),
        "main function body"
    );

    // Check that we have a lambda
    assert_eq!(stage1_compiler.lambdas.len(), 1, "one lambda");
    let lambda = &stage1_compiler.lambdas[0];
    assert_eq!(lambda.name, "lambda_0", "lambda name");
    assert_eq!(
        lambda.params,
        Chunk::from(vec![Rot, LoadLocal, Rot, LoadLocal]),
        "lambda params"
    );
    assert_eq!(
        lambda.body,
        Chunk::from(vec![
            GetLocal(IState::Set(0)),
            GetLocal(IState::Set(1)),
            Add(2),
            Rot,
            Return
        ]),
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
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_compiler =
        Stage1Compiler::new(&mut symbol_table).compile(&ast, &CompilerOptions::default());
    // Checking function
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, Chunk::new(), "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        Chunk::from(vec![
            Push(Stage1Value::Callable(IState::Unset("lambda_0".to_string()))),
            LoadGlobal
        ]),
        "main function prelude"
    );
    assert_eq!(
        main.body,
        Chunk::from(vec![
            Push(Stage1Value::F64(321.0)),
            Push(Stage1Value::F64(123.0)),
            GetGlobal(IState::Set(0)),
            Call(Stage1Callee::Function, 1),
            Call(Stage1Callee::Function, 1),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
            Halt,
        ]),
        "main function body"
    );

    assert_eq!(stage1_compiler.lambdas.len(), 2, "two lambda");

    // Check that we have a lambda 1
    let lambda = &stage1_compiler.lambdas[1];
    assert_eq!(lambda.name, "lambda_0", "lambda_0 name");
    assert_eq!(
        lambda.params,
        Chunk::from(vec![Rot, LoadLocal]),
        "lambda_0 params"
    );
    assert_eq!(
        lambda.body,
        Chunk::from(vec![
            GetLocal(IState::Unset("x".to_string())),
            LoadFree,
            Push(Stage1Value::Callable(IState::Unset("lambda_1".to_string()))),
            Rot,
            Return
        ]),
        "lambda_0 body"
    );

    // Check that we have a lambda 2
    let lambda = &stage1_compiler.lambdas[0];
    assert_eq!(lambda.name, "lambda_1", "lambda name");
    assert_eq!(
        lambda.params,
        Chunk::from(vec![Rot, LoadLocal]),
        "lambda params"
    );
    assert_eq!(
        lambda.body,
        Chunk::from(vec![
            GetFree(IState::Set(0)),
            GetLocal(IState::Set(0)),
            Add(2),
            Rot,
            Return
        ]),
        "lambda_1 body"
    );
}

#[test]
fn test_main_call_lambda() {
    let src = r#"
(fn main () (print ((lambda (x y) (+ x y)) 123 321) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_compiler =
        Stage1Compiler::new(&mut symbol_table).compile(&ast, &CompilerOptions::default());
    // Checking function
    assert_eq!(stage1_compiler.functions.len(), 1, "one function");
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, Chunk::new(), "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        Chunk::new(),
        "main function prelude"
    );
    assert_eq!(
        main.body,
        Chunk::from(vec![
            Push(Stage1Value::F64(321.0)),
            Push(Stage1Value::F64(123.0)),
            Push(Stage1Value::Callable(IState::Unset("lambda_0".to_string()))),
            Call(Stage1Callee::Function, 2),
            Push(Stage1Value::String("\n".to_string())),
            Call(Stage1Callee::Builtin("print".to_string()), 2),
            Halt,
        ]),
        "main function body"
    );

    assert_eq!(stage1_compiler.lambdas.len(), 1, "two lambda");

    // Check that we have a lambda 1
    let lambda = &stage1_compiler.lambdas[0];
    assert_eq!(lambda.name, "lambda_0", "lambda_0 name");
    assert_eq!(
        lambda.params,
        Chunk::from(vec![Rot, LoadLocal, Rot, LoadLocal]),
        "lambda_0 params"
    );
    assert_eq!(
        lambda.body,
        Chunk::from(vec![
            GetLocal(IState::Set(0)),
            GetLocal(IState::Set(1)),
            Add(2),
            Rot,
            Return
        ]),
        "lambda_0 body"
    );
}

#[test]
fn test_main_applying_lambda() {
    let src = r#"
(fn apply (f x) (f x))
(fn main () (print (apply (lambda (x) (+ x 321)) 123) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_compiler =
        Stage1Compiler::new(&mut symbol_table).compile(&ast, &CompilerOptions::default());

    // Checking function apply
    assert_eq!(stage1_compiler.functions.len(), 2, "two functions");
    let apply = &stage1_compiler.functions[0];
    assert_eq!(apply.name, "apply", "apply function name");
    assert_eq!(
        apply.params,
        Chunk::from(vec![Rot, LoadLocal, Rot, LoadLocal]),
        "apply function params"
    );
    assert_eq!(
        apply.prelude,
        // This is the global variable being loaded
        Chunk::new(),
        "apply function prelude"
    );
    assert_eq!(
        apply.body,
        Chunk::from(vec![
            // FIXME: Swap ordering
            GetLocal(IState::Set(1)),
            GetLocal(IState::Set(0)),
            Call(Stage1Callee::Function, 1),
            Rot,
            Return
        ]),
        "apply function body"
    );

    // Checking function main
    let main = &stage1_compiler.functions[1];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, Chunk::new(), "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        Chunk::new(),
        "main function prelude"
    );
    assert_eq!(
        main.body,
        Chunk::from(vec![
            Push(Stage1Value::F64(123.0)),
            Push(Stage1Value::Callable(IState::Unset("lambda_0".to_string()))),
            Push(Stage1Value::Callable(IState::Unset("apply".to_string()))),
            Call(Stage1Callee::Function, 2),
            Push(Stage1Value::String("\n".to_string())),
            Call(Stage1Callee::Builtin("print".to_string()), 2),
            Halt,
        ]),
        "main function body"
    );

    assert_eq!(stage1_compiler.lambdas.len(), 1, "two lambda");

    // Check that we have a lambda 1
    let lambda = &stage1_compiler.lambdas[0];
    assert_eq!(lambda.name, "lambda_0", "lambda_0 name");
    assert_eq!(
        lambda.params,
        Chunk::from(vec![Rot, LoadLocal]),
        "lambda_0 params"
    );
    assert_eq!(
        lambda.body,
        Chunk::from(vec![
            GetLocal(IState::Set(0)),
            Push(Stage1Value::F64(321.0)),
            Add(2),
            Rot,
            Return
        ]),
        "lambda_0 body"
    );
}

#[test]
fn test_main_if_else() {
    let src = r#"
(fn main () (if true (print "then\n") (print "else\n")))
"#;
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_compiler =
        Stage1Compiler::new(&mut symbol_table).compile(&ast, &CompilerOptions::default());

    // Checking function apply
    assert_eq!(stage1_compiler.functions.len(), 1, "one functions");
    // Checking function main
    let main = &stage1_compiler.functions[0];
    assert_eq!(main.name, "main", "main function name");
    assert_eq!(main.params, Chunk::new(), "main function params");
    assert_eq!(
        main.prelude,
        // This is the global variable being loaded
        Chunk::new(),
        "main function prelude"
    );
    assert_eq!(
        main.body,
        Chunk::from(vec![
            Push(Stage1Value::Bool(true)),
            JumpIf(26),
            Push(Stage1Value::String("then\n".to_string())),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
            Jump(Direction::Forward(20)),
            Push(Stage1Value::String("else\n".to_string())),
            Call(Stage1Callee::Builtin("print".to_string()), 1),
            Halt,
        ]),
        "main function body"
    );
}
