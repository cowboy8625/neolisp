#![allow(dead_code)]
#![allow(unused)]
mod builtin;
mod machine;
use std::fmt::Arguments;

use crate::compiler::Header;
use anyhow::Result;
pub use machine::Machine;

/// The byte code of a Neolisp program
/// OpCode is a single byte value
#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCode {
    /// No operation
    Noop,
    /// Halt the program
    Halt,
    /// Add two values and push the result on the stack
    AddF64,
    /// Sub two values and push the result on the stack
    Sub,
    /// Mul two values and push the result on the stack
    Mul,
    /// Div two values and push the result on the stack
    Div,
    /// Mod two values and push the result on the stack
    Mod,
    /// Compares values
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
    TypeOf,
    And,
    Or,
    Not,
    Print,
    /// Stack manipulation
    PushU8,
    PushF64,
    PushString,
    PopF64,
    /// Swap the top two items
    Swap,
    /// Duplicate the top most item and push it
    Dup,
    /// Take the top most items on stack and rotate them
    Rot,

    /// Load a value on to the local variable stack
    LoadLocalVar,
    /// Get a value on to the local variable stack
    GetLocalVar,
    /// Load a value on to the global variable stack
    LoadGlobalVar,
    /// Get a value on to the global variable stack
    GetGlobalVar,

    Call,
    Return,

    // List manipulation
    CreateList,
    // count of args
    // name length
    // name
    BuiltIn,
}

impl TryFrom<u8> for OpCode {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Noop),
            1 => Ok(Self::Halt),
            2 => Ok(Self::AddF64),
            3 => Ok(Self::Sub),
            4 => Ok(Self::Mul),
            5 => Ok(Self::Div),
            6 => Ok(Self::Mod),
            7 => Ok(Self::Eq),
            8 => Ok(Self::Gt),
            9 => Ok(Self::Lt),
            10 => Ok(Self::Gte),
            11 => Ok(Self::Lte),
            12 => Ok(Self::TypeOf),
            13 => Ok(Self::And),
            14 => Ok(Self::Or),
            15 => Ok(Self::Not),
            16 => Ok(Self::Print),
            17 => Ok(Self::PushU8),
            18 => Ok(Self::PushF64),
            19 => Ok(Self::PushString),
            20 => Ok(Self::PopF64),
            21 => Ok(Self::Swap),
            22 => Ok(Self::Dup),
            23 => Ok(Self::Rot),
            24 => Ok(Self::LoadLocalVar),
            25 => Ok(Self::GetLocalVar),
            26 => Ok(Self::LoadGlobalVar),
            27 => Ok(Self::GetGlobalVar),
            28 => Ok(Self::Call),
            29 => Ok(Self::Return),
            30 => Ok(Self::CreateList),
            31 => Ok(Self::BuiltIn),
            _ => Err(format!("unknown opcode: {value}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U8(u8),
    U32(u32),
    F64(f64),
    String(String),
    Bool(bool),
    List(Vec<Value>),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(value) => write!(f, "{value}"),
            Self::U32(value) => write!(f, "{value}"),
            Self::F64(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::List(value) => {
                write!(
                    f,
                    "({})",
                    value
                        .iter()
                        .map(|v| format!("{}", v))
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
        }
    }
}
