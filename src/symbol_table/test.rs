#![allow(dead_code)]
#![allow(unused)]
use super::*;
use crate::parser::parser;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

#[test]
fn test_table_creation() {
    let src = r#"
(var add (lambda (x) (lambda (y) (+ x y))))
(fn main () (print ((add 123) 321) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    eprintln!("{:#?}", symbol_table);
    assert!(false);
}
