use super::*;
use crate::compiler;
use crate::parser::parser;
use crate::symbol_table::SymbolWalker;
use crate::vm::{Instruction, OpCode};
use chumsky::prelude::Parser;
use pretty_assertions::assert_eq;

fn compile(src: &str) -> anyhow::Result<Vec<Instruction>> {
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let stage1_data = compiler::Stage1Compiler::new(symbol_table.clone()).compiler(&ast);
    let instructions = compiler::compile_to_instructions(&mut symbol_table, &stage1_data);
    Ok(instructions)
}

#[test]
fn test_to_bytecode() {
    let src = r#"(fn main () (print (* 1 2)))"#;

    let instructions = compile(src).unwrap();
    assert_eq!(
        instructions[0].to_bytecode(),
        vec![OpCode::StartAt as u8, 0x05, 0x00, 0x00, 0x00],
        "{:?}",
        instructions[0]
    );
    assert_eq!(
        instructions[1].to_bytecode(),
        vec![OpCode::Push as u8, 4, 0, 0, 0, 0, 0, 0, 240, 63],
        "{:?}",
        instructions[1]
    );
    assert_eq!(
        instructions[2].to_bytecode(),
        vec![OpCode::Push as u8, 4, 0, 0, 0, 0, 0, 0, 0, 64],
        "{:?}",
        instructions[2]
    );
    assert_eq!(
        instructions[3].to_bytecode(),
        vec![OpCode::Mul as u8],
        "{:?}",
        instructions[3]
    );
    #[rustfmt::skip]
    assert_eq!(
        instructions[4].to_bytecode(),
        vec![
            OpCode::Call as u8, // Callable type
            0x01, // Builtin type
            0x05, // Builtin name length
            0x70, 0x72, 0x69, 0x6E, 0x74, // "print"
            0x01, // count
        ],
        "{:?}",
        instructions[4]
    );
}
