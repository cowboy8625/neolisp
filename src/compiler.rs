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
    symbol_table: Option<SymbolTable>,
    emitter_offset: usize,
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

    pub fn with_maybe_a_symbol_table(mut self, symbol_table: Option<SymbolTable>) -> Self {
        self.symbol_table = symbol_table;
        self
    }
    pub fn with_offset(mut self, offset: usize) -> Self {
        self.emitter_offset = offset;
        self
    }

    pub fn compile(
        mut self,
        src: &str,
    ) -> Result<Option<(SymbolTable, Vec<Instruction>)>, Vec<Error>> {
        let ast = parser().parse(src)?;

        if self.debug_ast {
            eprintln!("{ast:#?}");
            return Ok(None);
        }

        let options = EmitterOptions {
            no_main: self.no_main,
        };
        let mut symbol_table = match self.symbol_table.take() {
            Some(mut table) => {
                SymbolTableBuilder::default().build_from_scope(&ast, &mut table)?;
                table
            }
            None => SymbolTableBuilder::default().build(&ast)?,
        };

        let offset = if self.symbol_table.is_some() {
            self.emitter_offset
        } else {
            0
        };

        let instructions = Emitter::new(&mut symbol_table, options)
            .with_offset(offset)
            .compile(&ast)?;

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
