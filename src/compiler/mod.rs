// TODO: Error Reporting using `ariadne`
#![allow(dead_code)]
#![allow(unused)]
mod compiler;
mod decompiler;
mod header;
mod instruction;
mod ir;
use super::{BUILTINS, OPERATORS};
use crate::ast::{Expr, Spanned};
use crate::parser::parser;
use crate::symbol_table::SymbolTable;
use chumsky::prelude::Parser;

use compiler::Compiler;
pub use decompiler::{decompile, decompile_chunk, display_chunk};
pub use header::Header;
pub use instruction::Instruction;
pub use ir::{Function, Ir, Value};

pub fn compile(src: &str) -> Result<Vec<u8>, Vec<String>> {
    let ast = get_ast(src)?;
    let mut compiler = Compiler::default();
    let bytes = compiler.compile(&ast)?;

    Ok(bytes)
}

pub fn compile_chunk(src: &str) -> Result<Vec<u8>, Vec<String>> {
    let ast = get_ast(src)?;
    let mut compiler = Compiler::new(None);
    let bytes = compiler.compile(&ast)?;

    Ok(bytes)
}

pub fn compile_to_instructions(src: &str) -> Result<(Vec<Ir>, Vec<Function>), Vec<String>> {
    let ast = get_ast(src)?;
    let mut compiler = Compiler::new(None);
    let instructions = compiler.generate_ir_code(&SymbolTable::new(), &ast);

    Ok((instructions, compiler.functions))
}

fn get_ast(src: &str) -> Result<Vec<Spanned<Expr>>, Vec<String>> {
    match parser().parse(src) {
        Ok(ast) => Ok(ast),
        Err(errors) => Err(errors
            .into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_compile_chunk() {
        let src = r#"
(var X 1234)
(var Y 4321)
(fn add (x y) (+ x y ))
(fn main () (print (add X Y)))
"#;
        let (globals, mut functions) = compile_to_instructions(src).unwrap();
        for global in globals {
            functions[1].instruction.insert(0, global);
        }
        eprintln!(
            "Add Function IR {}:\n{:#?}",
            functions[1].size(),
            functions[1]
        );
        let bytes = compile_chunk(src).unwrap();
        eprintln!("length: {}\n{:#?}", bytes.len(), bytes);
        display_chunk(&bytes);
        assert_eq!(bytes.len(), 8);
    }
}
