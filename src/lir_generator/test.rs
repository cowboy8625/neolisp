use super::compiler::Compiler;
use crate::hir_generator::HirCompiler;
use crate::parser::parser;
use crate::symbol_table::SymbolWalker;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

#[test]
fn test_generate_lir_code() {
    let src = r#"
(var add (lambda (x) (lambda (y) (+ x y))))
; (fn add (x y) (+ x y))
(fn main () (print ((add 123) 321)))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let hir = HirCompiler::new(symbol_table.clone())
        .compile(&ast)
        .unwrap();
    let lir = Compiler::new(symbol_table).compile(&hir).unwrap();
    assert_eq!(lir, vec![]);
}
