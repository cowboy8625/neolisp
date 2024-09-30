mod stage1;
mod stage2;
#[cfg(test)]
mod test;

use super::{BUILTINS, KEYWORDS, OPERATORS};
use crate::parser::parser;
use crate::symbol_table::SymbolTableBuilder;
use chumsky::prelude::Parser;
pub use stage1::{
    Chunk, IState, Stage1Callee, Stage1Compiler, Stage1Data, Stage1Function, Stage1Instruction,
    Stage1Lambda, Stage1Value,
};

pub fn compile(src: &str) -> anyhow::Result<Vec<u8>> {
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_data = Stage1Compiler::new(symbol_table.clone()).compiler(&ast);
    let instructions = stage2::compile_to_instructions(&mut symbol_table, &stage1_data);
    let mut program = Vec::new();
    for i in instructions {
        program.extend(i.to_bytecode());
    }
    Ok(program)
}

#[cfg(test)]
pub fn compile_to_instructions(src: &str) -> anyhow::Result<Vec<crate::vm::Instruction>> {
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_data = Stage1Compiler::new(symbol_table.clone()).compiler(&ast);
    let instructions = stage2::compile_to_instructions(&mut symbol_table, &stage1_data);
    Ok(instructions)
}
