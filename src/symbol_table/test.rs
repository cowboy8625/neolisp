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
    let symbol_table = SymbolTableBuilder::default().build(&ast);
    eprintln!("{:#?}", symbol_table);
    assert_eq!(
        symbol_table.lookup("add"),
        Some(&Symbol {
            id: 0,
            name: "add".to_string(),
            symbol_type: SymbolType::Dynamic,
            kind: SymbolKind::Variable,
            scope: SymbolScope::Global,
            scope_level: 0,
            location: None,
        })
    );
    assert_eq!(
        symbol_table.lookup("main"),
        Some(&Symbol {
            id: 0,
            name: "main".to_string(),
            symbol_type: SymbolType::Function {
                self_reference: false,
                params: vec![],
                return_type: Box::new(SymbolType::Dynamic)
            },
            kind: SymbolKind::Function,
            scope: SymbolScope::Global,
            scope_level: 0,
            location: None,
        })
    );
    assert_eq!(
        symbol_table.lookup("lambda_0"),
        Some(&Symbol {
            id: 0,
            name: "lambda_0".to_string(),
            symbol_type: SymbolType::Function {
                self_reference: false,
                params: vec![SymbolType::Dynamic],
                return_type: Box::new(SymbolType::Dynamic)
            },
            kind: SymbolKind::Lambda,
            scope: SymbolScope::Global,
            scope_level: 0,
            location: None,
        })
    );
    assert_eq!(
        symbol_table.lookup("lambda_1"),
        Some(&Symbol {
            id: 0,
            name: "lambda_1".to_string(),
            symbol_type: SymbolType::Function {
                self_reference: false,
                params: vec![SymbolType::Dynamic],
                return_type: Box::new(SymbolType::Dynamic)
            },
            kind: SymbolKind::Lambda,
            scope: SymbolScope::Global,
            scope_level: 1,
            location: None,
        })
    );
}

#[test]
fn detecting_recursion() {
    let src = r#"
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fn main () (print (fib 42) "\n"))
"#;
    let ast = parser().parse(src).unwrap();
    let symbol_table = SymbolTableBuilder::default().build(&ast);
    eprintln!("{:#?}", symbol_table);
    assert_eq!(
        symbol_table.lookup("fib"),
        Some(&Symbol {
            id: 0,
            name: "fib".to_string(),
            symbol_type: SymbolType::Function {
                self_reference: true,
                params: vec![SymbolType::Dynamic],
                return_type: Box::new(SymbolType::Dynamic)
            },
            kind: SymbolKind::Function,
            scope: SymbolScope::Global,
            scope_level: 0,
            location: None,
        })
    );
}
