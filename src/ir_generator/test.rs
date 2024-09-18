use super::compiler::Compiler;
use crate::parser::parser;
use crate::symbol_table::SymbolWalker;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

#[test]
fn test_generate_ir_code() {
    let src = r#"
(var add (lambda (x) (lambda (y) (+ x y))))
; (fn add (x y) (+ x y))
(fn main () (print ((add 123) 321)))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let ir = Compiler::new(symbol_table).compile(&ast).unwrap();
    assert_eq!(ir, vec![]);
}
