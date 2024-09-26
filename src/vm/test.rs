use super::*;
use crate::compiler2;
use crate::parser::parser;
use crate::symbol_table::SymbolWalker;
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

fn compile(src: &str) -> anyhow::Result<Vec<Instruction>> {
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let stage1_data = compiler2::Stage1Compiler::new(symbol_table.clone()).compiler(&ast);
    let instructions = compiler2::compile_to_instructions(&mut symbol_table, &stage1_data);
    Ok(instructions)
}

// #[test]
// fn test_to_bytecode() {
//     let src = r#"(fn main () (print (+ 1 2)))"#;
//
//     let instructions = compile(src).unwrap();
//     for i in instructions.iter() {
//         eprintln!("{:?}", i);
//     }
//     let bytecode = instructions
//         .into_iter()
//         .flat_map(|i| i.to_bytecode())
//         .collect::<Vec<u8>>();
//     assert_eq!(bytecode, vec![]);
// }
