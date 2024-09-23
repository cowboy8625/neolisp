use super::{CompiledHir, Function, Hir, HirCompiler, Lambda, Operator, Test, Value, Var};
use crate::parser::parser;
use crate::symbol_table::SymbolWalker;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

#[test]
fn test_generate_hir_code() {
    let src = r#"
(var add (lambda (x y) (+ x y)))
(fn main () (print (add 123 321) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let ir = HirCompiler::new(symbol_table).compile(&ast).unwrap();
    assert_eq!(
        ir,
        vec![
            CompiledHir::Functions(vec![Function {
                name: "main".to_string(),
                params: vec![],
                body: vec![
                    Hir::BuiltIn(
                        "print".to_string(),
                        vec![
                            Hir::Call(
                                "add".to_string(),
                                vec![Hir::Value(Value::F64(123.0)), Hir::Value(Value::F64(321.0)),],
                            ),
                            Hir::Value(Value::String("\n".to_string())),
                        ],
                    ),
                    Hir::Halt,
                ],
            }]),
            CompiledHir::Lambdas(vec![Lambda {
                name: "lambda_0".to_string(),
                params: vec!["x".to_string(), "y".to_string()],
                instruction: vec![
                    Hir::Operator(
                        Operator::Add,
                        vec![
                            Hir::Value(Value::Id("x".to_string())),
                            Hir::Value(Value::Id("y".to_string()))
                        ],
                    ),
                    Hir::Return,
                ],
                captured: vec![],
            }]),
            CompiledHir::Tests(vec![]),
            CompiledHir::GlobalVar(vec![Var {
                name: "add".to_string(),
                instruction: vec![Hir::LoadLambda("lambda_0".to_string())],
            },],),
        ]
    );
}
