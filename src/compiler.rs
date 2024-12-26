use super::{
    emitter::Emitter,
    error::Error,
    instruction::Instruction,
    parser::parser,
    symbol_table::{SymbolTable, SymbolTableBuilder},
};

use chumsky::prelude::Parser;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CompilerOptions {
    pub no_main: bool,
    pub test: bool,
}

impl CompilerOptions {
    pub fn with_no_main(mut self, no_main: bool) -> Self {
        self.no_main = no_main;
        self
    }

    pub fn with_test(mut self, test: bool) -> Self {
        self.test = test;
        self
    }
}

#[derive(Debug, Default)]
pub struct Compiler {
    debug_ast: bool,
    decompile: bool,
    no_main: bool,
    test: bool,
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

    pub fn with_test(mut self, value: bool) -> Self {
        self.test = value;
        self
    }

    pub fn with_offset(mut self, offset: usize) -> Self {
        self.emitter_offset = offset;
        self
    }

    pub fn compile(
        self,
        src: &str,
        symbol_table: &mut SymbolTable,
    ) -> Result<Option<Vec<Instruction>>, Vec<Error>> {
        let ast = parser().parse(src)?;

        if self.debug_ast {
            eprintln!("{ast:#?}");
            return Ok(None);
        }

        let options = CompilerOptions::default()
            .with_test(self.test)
            .with_no_main(self.no_main);

        SymbolTableBuilder::default()
            .with_options(options)
            .build(&ast, symbol_table)?;

        let offset = if self.symbol_table.is_some() {
            self.emitter_offset
        } else {
            0
        };

        let instructions = Emitter::new(symbol_table, options)
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

        Ok(Some(instructions))
    }
}
