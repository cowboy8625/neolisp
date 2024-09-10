#![allow(dead_code)]
#![allow(unused)]
use std::fmt::Arguments;

use crate::compiler::Header;
use anyhow::Result;

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
            _ => Err(format!("unknown opcode: {value}")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Value {
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

/// Virtual Machine
///
/// Stack based virtual machine for evaluating LISP code
///
/// # Examples
///
/// ```rust
/// let mut vm = vm::Machine::new(program);
/// vm.run();
/// ```
#[derive(Debug)]
pub struct Machine {
    program: Vec<u8>,
    ip: usize,
    is_running: bool,
    stack: Vec<Value>,
    local_var: Vec<Value>,
    global_var: Vec<Value>,
}

impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        debug_assert!(program.len() > Header::SIZE as usize);
        let program_header = Header::from(&program[0..Header::SIZE as usize]);
        Self {
            program,
            ip: program_header.start as usize,
            is_running: true,
            stack: vec![],
            local_var: vec![],
            global_var: vec![],
        }
    }

    pub fn run_once(&mut self) -> Result<()> {
        let op = self.get_op_code()?;
        // eprintln!("OpCode::{op:?} {:02X}", op as u8);
        match op {
            OpCode::Noop => Ok(()),
            OpCode::Halt => Ok(self.shutdown()),
            OpCode::AddF64 => {
                let count = self.get_u32()?;
                let mut result = 0.;
                for _ in 0..count {
                    let Some(Value::F64(value)) = self.stack.pop() else {
                        panic!("expected value on stack for AddF64")
                    };
                    result += value;
                }
                self.stack.push(Value::F64(result));
                Ok(())
            }
            OpCode::PushF64 => {
                let value = self.get_f64()?;
                self.stack.push(Value::F64(value));
                Ok(())
            }
            OpCode::PushString => {
                let value = self.get_string()?;
                self.stack.push(Value::String(value));
                Ok(())
            }
            OpCode::Print => {
                use std::io::Write;
                let count = self.get_u32()?;
                let lock = std::io::stdout().lock();
                let mut writer = std::io::BufWriter::new(lock);
                let mut output = String::new();

                for _ in 0..count {
                    let Some(value) = self.stack.pop() else {
                        panic!("expected value on stack for print")
                    };

                    output.insert_str(0, &format!("{}", value));
                }
                writer.write_all(output.as_bytes())?;
                writer.flush()?;
                Ok(())
            }
            OpCode::Call => {
                let address = self.get_u32()? as usize;
                self.stack.push(Value::U32(self.ip as u32));
                self.ip = address;
                Ok(())
            }

            OpCode::LoadLocalVar => {
                // NOTE: I dont think this LocalVar(index) is used
                let _ = self.get_u32()? as usize;
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for LoadLocalVar")
                };
                self.local_var.push(value);
                Ok(())
            }
            OpCode::LoadGlobalVar => {
                let Some(value) = self.stack.pop() else {
                    panic!("expected value on stack for LoadGlobalVar")
                };
                self.global_var.push(value);
                Ok(())
            }
            OpCode::GetLocalVar => {
                let index = self.get_u32()? as usize;
                let Some(value) = self.local_var.get(index) else {
                    panic!("expected value on stack for GetLocalVar")
                };
                self.stack.push(value.clone());
                Ok(())
            }
            OpCode::GetGlobalVar => {
                let index = self.get_u32()? as usize;
                let Some(value) = self.global_var.get(index) else {
                    panic!("expected value on stack for GetGlobalVar")
                };
                self.stack.push(value.clone());
                Ok(())
            }
            OpCode::Rot => {
                let Some(right) = self.stack.pop() else {
                    panic!("expected value on stack for rot")
                };
                let Some(left) = self.stack.pop() else {
                    panic!("expected value on stack for rot")
                };
                self.stack.push(right);
                self.stack.push(left);
                Ok(())
            }
            OpCode::Return => {
                let Some(Value::U32(address)) = self.stack.pop() else {
                    panic!("expected value on stack for return")
                };
                self.ip = address as usize;
                self.local_var.clear();
                Ok(())
            }
            OpCode::CreateList => {
                let mut items = Vec::new();
                let count = self.get_u32()?;
                for _ in (0..count) {
                    let Some(item) = self.stack.pop() else {
                        panic!("expected value on stack for CreateList")
                    };
                    items.insert(0, item);
                }
                self.stack.push(Value::List(items));
                Ok(())
            }
            op => unimplemented!("{:?}", op),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        // eprintln!("start: {}", self.ip);
        // for (i, chunk) in self.program[Header::SIZE as usize..].chunks(4).enumerate() {
        //     let debug = chunk
        //         .iter()
        //         .map(|b| format!("{b:02X}"))
        //         .collect::<Vec<_>>()
        //         .join(" ");
        //     let start = i * 4 + Header::SIZE as usize;
        //     let end = i * 4 + (Header::SIZE as usize) + 4;
        //     let selected = if (start..end).contains(&self.ip) {
        //         "> "
        //     } else {
        //         "  "
        //     };
        //     eprintln!("{selected}{debug}");
        // }
        while self.is_running && self.ip < self.program.len() {
            self.run_once()?;
        }
        Ok(())
    }

    fn shutdown(&mut self) {
        self.is_running = false;
    }

    fn get_op_code(&mut self) -> Result<OpCode> {
        let byte = self.program[self.ip];
        let op = OpCode::try_from(byte).map_err(|e| anyhow::anyhow!(e))?;
        self.ip += 1;
        Ok(op)
    }

    fn get_u8(&mut self) -> Result<u8> {
        let byte = self.program[self.ip];
        self.ip += 1;
        Ok(byte)
    }

    fn get_u16(&mut self) -> Result<u16> {
        let byte2 = self.get_u8()? as u16;
        let byte1 = self.get_u8()? as u16;
        Ok(byte1 << 8 | byte2)
    }

    fn get_u32(&mut self) -> Result<u32> {
        let byte4 = self.get_u8()? as u32;
        let byte3 = self.get_u8()? as u32;
        let byte2 = self.get_u8()? as u32;
        let byte1 = self.get_u8()? as u32;
        Ok(byte1 << 24 | byte2 << 16 | byte3 << 8 | byte4)
    }

    fn get_u64(&mut self) -> Result<u64> {
        let byte8 = self.get_u8()? as u64;
        let byte7 = self.get_u8()? as u64;
        let byte6 = self.get_u8()? as u64;
        let byte5 = self.get_u8()? as u64;
        let byte4 = self.get_u8()? as u64;
        let byte3 = self.get_u8()? as u64;
        let byte2 = self.get_u8()? as u64;
        let byte1 = self.get_u8()? as u64;
        Ok(byte1 << 56
            | byte2 << 48
            | byte3 << 40
            | byte4 << 32
            | byte5 << 24
            | byte6 << 16
            | byte7 << 8
            | byte8)
    }

    fn get_f64(&mut self) -> Result<f64> {
        let byte1 = self.get_u8()? as u64;
        let byte2 = self.get_u8()? as u64;
        let byte3 = self.get_u8()? as u64;
        let byte4 = self.get_u8()? as u64;
        let byte5 = self.get_u8()? as u64;
        let byte6 = self.get_u8()? as u64;
        let byte7 = self.get_u8()? as u64;
        let byte8 = self.get_u8()? as u64;
        Ok(f64::from_le_bytes([
            byte1 as u8,
            byte2 as u8,
            byte3 as u8,
            byte4 as u8,
            byte5 as u8,
            byte6 as u8,
            byte7 as u8,
            byte8 as u8,
        ]))
    }

    fn get_string(&mut self) -> Result<String> {
        let len = self.get_u32()? as usize;
        let bytes = self.program[self.ip..self.ip + len].to_vec();
        self.ip += len;
        Ok(String::from_utf8(bytes)?)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn create_test_program(bytes: &[u8]) -> Vec<u8> {
        let header = Header::default();
        let mut program = header.to_bytecode();
        program.extend(bytes.to_vec());
        program
    }

    #[test]
    fn test_opcode() {
        assert_eq!(OpCode::try_from(0), Ok(OpCode::Noop));
        assert_eq!(OpCode::try_from(1), Ok(OpCode::Halt));
    }

    #[test]
    fn test_machine_get_op_code() -> Result<()> {
        let mut machine = Machine::new(create_test_program(&[0, 1]));
        assert_eq!(machine.get_op_code()?, OpCode::Noop);
        assert_eq!(machine.get_op_code()?, OpCode::Halt);
        Ok(())
    }

    // #[test]
    // fn test_get_u16() -> Result<()> {
    //     eprintln!("F64: {:?}", 10.0f64.to_le_bytes());
    //     let number: u16 = 0x0102;
    //     let bytes = number.to_le_bytes();
    //     let mut h = Header::new();
    //     let mut program = h.to_bytecode();
    //     program.extend(bytes.to_vec());
    //     let mut machine = Machine::new(program);
    //     let bytes_u16 = machine.get_u16()?;
    //     eprintln!("{:02X}", bytes_u16);
    //     assert_eq!(bytes_u16, 0x0102);
    //     Ok(())
    // }
}
