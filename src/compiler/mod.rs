mod stage1;
mod stage2;
#[cfg(test)]
mod test;

use crate::parser::parser;
use crate::symbol_table::SymbolTableBuilder;
use chumsky::prelude::Parser;
pub use stage1::{
    Chunk, IState, Stage1Callee, Stage1Compiler, Stage1Data, Stage1Function, Stage1Instruction,
    Stage1Lambda, Stage1Value,
};

pub fn compile(src: &str, options: &CompilerOptions) -> anyhow::Result<Vec<u8>> {
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_data = Stage1Compiler::new(&mut symbol_table).compile(&ast, options);
    let instructions = stage2::compile_to_instructions(&mut symbol_table, &stage1_data);
    let mut program = Vec::new();
    for i in instructions {
        program.extend(i.to_bytecode());
    }
    Ok(program)
}

#[cfg(test)]
pub fn compile_to_instructions(
    src: &str,
    options: &CompilerOptions,
) -> anyhow::Result<Vec<crate::vm::Instruction>> {
    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    let stage1_data = Stage1Compiler::new(&mut symbol_table).compile(&ast, options);
    let instructions = stage2::compile_to_instructions(&mut symbol_table, &stage1_data);
    Ok(instructions)
}

#[derive(Debug, Default)]
pub struct CompilerOptions {
    pub no_main: bool,
}
