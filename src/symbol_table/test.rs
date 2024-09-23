#![allow(dead_code)]
#![allow(unused)]
use super::*;
use crate::parser::parser;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;
use std::collections::HashMap;

#[test]
fn test_table_creation() {
    let src = r#"
(var add (lambda (x) (lambda (y) (+ x y))))
(fn main () (print ((add 123) 321) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    eprintln!("{:#?}", symbol_table);
    assert_eq!(
        symbol_table.lookup("add"),
        Some(&Symbol {
            id: 0,
            name: "add".to_string(),
            symbol_type: SymbolType::Dynamic,
            kind: SymbolKind::Variable,
            scope: Scope::Global,
            scope_level: 0,
            location: None,
        })
    );
    assert_eq!(
        symbol_table.lookup("main"),
        Some(&Symbol {
            id: 0,
            name: "main".to_string(),
            symbol_type: SymbolType::Function(vec![], Box::new(SymbolType::Dynamic)),
            kind: SymbolKind::Function,
            scope: Scope::Global,
            scope_level: 1,
            location: None,
        })
    );
    assert_eq!(
        symbol_table.lookup("lambda_0"),
        Some(&Symbol {
            id: 0,
            name: "lambda_0".to_string(),
            symbol_type: SymbolType::Function(
                vec![SymbolType::Dynamic],
                Box::new(SymbolType::Dynamic)
            ),
            kind: SymbolKind::Lambda,
            scope: Scope::Global,
            scope_level: 1,
            location: None,
        })
    );
    assert_eq!(
        symbol_table.lookup("lambda_1"),
        Some(&Symbol {
            id: 1,
            name: "lambda_1".to_string(),
            symbol_type: SymbolType::Function(
                vec![SymbolType::Dynamic],
                Box::new(SymbolType::Dynamic)
            ),
            kind: SymbolKind::Lambda,
            scope: Scope::Function,
            scope_level: 2,
            location: None,
        })
    );
}
