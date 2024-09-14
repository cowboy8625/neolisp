#![allow(dead_code)]
#![allow(unused)]
mod builtin;
mod machine;
mod opcode;
mod value;
use std::fmt::Arguments;

use crate::compiler::Header;
use anyhow::Result;
pub use machine::Machine;
pub use opcode::OpCode;
pub use value::Value;
