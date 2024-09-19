#![allow(dead_code)]
#![allow(unused)]

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Int,
    Float,
    String(u32),
    Bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lir {
    Swap,
    Dup,
    Rot,
    Return,
    Halt,
    Add { ty: Type, count: u32 },
    Push { ty: Type },
    Pop { ty: Type },
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
