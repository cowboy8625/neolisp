mod stage1;
mod stage2;
#[cfg(test)]
mod test;

use super::{BUILTINS, KEYWORDS, OPERATORS};
pub use stage1::{
    Chunk, IState, Stage1Callee, Stage1Compiler, Stage1Data, Stage1Function, Stage1Instruction,
    Stage1Lambda, Stage1Value,
};

pub use stage2::compile_to_instructions;

pub fn compile(src: &str) -> anyhow::Result<Vec<u8>> {
    use crate::parser::parser;
    use crate::symbol_table::SymbolWalker;
    use chumsky::prelude::Parser;

    let ast = parser().parse(src).unwrap();
    let mut symbol_table = SymbolWalker::default().walk(&ast).unwrap();
    let stage1_data = Stage1Compiler::new(symbol_table.clone()).compiler(&ast);
    let instructions = compile_to_instructions(&mut symbol_table, &stage1_data);
    let mut program = Vec::new();
    for i in instructions {
        program.extend(i.to_bytecode());
    }
    Ok(program)
}
