use super::{
    emitter::{Emitter, EmitterOptions},
    error::Error,
    instruction::Instruction,
    parser::parser,
    symbol_table::{SymbolTable, SymbolTableBuilder},
};

use chumsky::prelude::Parser;

#[derive(Debug, Default)]
pub struct Compiler {
    debug_ast: bool,
    decompile: bool,
    no_main: bool,
}

impl Compiler {
    pub fn debug_ast(mut self, value: bool) -> Self {
        self.debug_ast = value;
        self
    }

    pub fn decompile(mut self, value: bool) -> Self {
        self.decompile = value;
        self
    }

    pub fn no_main(mut self, value: bool) -> Self {
        self.no_main = value;
        self
    }

    pub fn compile(self, src: &str) -> Result<Option<(SymbolTable, Vec<Instruction>)>, Vec<Error>> {
        let ast = parser().parse(src)?;

        if self.debug_ast {
            eprintln!("{ast:#?}");
            return Ok(None);
        }

        let options = EmitterOptions {
            no_main: self.no_main,
        };
        let mut symbol_table = SymbolTableBuilder::default().build(&ast)?;
        let instructions = Emitter::new(&mut symbol_table, options).compile(&ast)?;

        if self.decompile {
            let mut offset = 0;
            for int in instructions.iter() {
                eprintln!("{offset:02X} {offset:>2}  {:?}", int);
                offset += int.size();
            }

            return Ok(None);
        }

        Ok(Some((symbol_table, instructions)))
    }
}
