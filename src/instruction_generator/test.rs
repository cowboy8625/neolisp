use super::compiler::Compiler;
use crate::hir_generator::HirCompiler;
use crate::parser::parser;
use crate::symbol_table::SymbolWalker;
use crate::vm::{
    Callee, IState,
    Instruction::{self, *},
    Value,
};
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

fn show_program(program: &[Instruction]) {
    for (i, int) in program.iter().enumerate() {
        eprintln!("{i:02X} {i:>2}  {:?}", int);
    }
}

#[test]
fn test_generate_instructions_code() {
    let src = r#"
(var add (lambda (x y) (+ x y)))
(fn main () (print (add 123 321) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let hir = HirCompiler::new(symbol_table.clone())
        .compile(&ast)
        .unwrap();
    let instructions = Compiler::new(symbol_table).compile(&hir).unwrap();
    show_program(&instructions);
    assert_eq!(
        instructions,
        vec![
            StartAt(10),
            // Add Lambda
            // Prelude
            Rot,
            LoadLocal,
            Rot,
            LoadLocal,
            // ADD
            GetLocal(IState::Set(0)),
            GetLocal(IState::Set(1)),
            Add,
            Rot,
            Return,
            // MAIN PRELUDE
            Push(Value::Callable(IState::Set(1))),
            LoadGlobal,
            // MAIN
            Push(Value::F64(123.0)),
            Push(Value::F64(321.0)),
            GetGlobal(IState::Set(0)),
            Call(Callee::Function, 2),
            Push(Value::String("\n".to_string())),
            Call(Callee::Builtin("print".to_string()), 2),
            Halt,
        ]
    );
}

#[test]
fn test_generate_instructions_for_lambda_returning_a_lambda() {
    let src = r#"
(var add (lambda (x) (lambda (y) (+ x y))))
(fn main () (print ((add 123) 321) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let hir = HirCompiler::new(symbol_table.clone())
        .compile(&ast)
        .unwrap();
    let instructions = Compiler::new(symbol_table).compile(&hir).unwrap();
    show_program(&instructions);
    let program = vec![
        StartAt(17),
        // lambda 0
        Rot,
        LoadLocal,
        GetLocal(IState::Set(0)),
        LoadFree,
        Push(Value::Callable(IState::Set(8))),
        Rot,
        Return,
        // lambda 1
        Rot,
        LoadLocal,
        GetFree(IState::Set(0)),
        LoadLocal,
        GetLocal(IState::Set(0)),
        GetLocal(IState::Set(1)),
        Add,
        Rot,
        Return,
        // var add
        Push(Value::Callable(IState::Set(1))),
        LoadGlobal,
        // main
        Push(Value::F64(321.0)),
        Push(Value::F64(123.0)),
        GetGlobal(IState::Set(0)),
        Call(Callee::Function, 1),
        Call(Callee::Function, 1),
        Push(Value::String("\n".to_string())),
        Call(Callee::Builtin("print".to_string()), 2),
        Halt,
    ];
    assert_eq!(instructions, program,);
}