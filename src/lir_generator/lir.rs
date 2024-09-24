#![allow(dead_code)]
#![allow(unused)]

use crate::symbol_table::SymbolType;
use crate::vm::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String(u32),
    Bool,
}

impl From<SymbolType> for Type {
    fn from(symbol_kind: SymbolType) -> Self {
        match symbol_kind {
            SymbolType::Dynamic => unreachable!("I dont know what to do with a `Dynamic` type"),
            SymbolType::Bool => Type::Bool,
            SymbolType::Int => Type::Int,
            SymbolType::Float => Type::Float,
            SymbolType::String => Type::String(0),
            SymbolType::Function(_, _) => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lir {
    Swap,
    Dup,
    Rot,
    Return,
    Halt,
    Add { count: u32 },
    Push { value: Value },
    LoadLocalValue { index: u32 },
    Call { address: u32, is_lambda: bool },
    BuiltIn { arg_count: u32, name: String },
    CallTest { address: u32 },
    CallBuiltin { count_of_args: u32, name: String },
    JumpIfFalse { relative_offset: u32 },
    JumpForward { relative_offset: u32 },
}

impl Lir {
    fn size(&self) -> u32 {
        match self {
            Lir::Swap | Lir::Dup | Lir::Rot | Lir::Return | Lir::Halt => 1,
            Lir::Add { ty, count } => todo!(),
            Lir::Push { ty } => todo!(),
            Lir::Pop { ty } => todo!(),
            Lir::Call { address, is_lambda } => todo!(),
            Lir::BuiltIn { arg_count, name } => todo!(),
            Lir::CallTest { address } => todo!(),
            Lir::CallBuiltin {
                count_of_args,
                name,
            } => todo!(),
            Lir::JumpIfFalse { relative_offset } => todo!(),
            Lir::JumpForward { relative_offset } => todo!(),
        }
    }
}