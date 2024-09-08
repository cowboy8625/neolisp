use std::fmt::Arguments;

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
    PushF64,
    PopF64,
    Swap,
    Dup,
    Rot,
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
            17 => Ok(Self::PushF64),
            18 => Ok(Self::PopF64),
            19 => Ok(Self::Swap),
            20 => Ok(Self::Dup),
            21 => Ok(Self::Rot),
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
}

impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        Self {
            program,
            ip: 0,
            is_running: true,
            stack: vec![],
        }
    }

    pub fn run_once(&mut self) -> Result<()> {
        let op = self.get_op_code()?;
        match op {
            OpCode::Noop => Ok(()),
            OpCode::Halt => Ok(self.shutdown()),
            OpCode::AddF64 => {
                let Some(Value::F64(left)) = self.stack.pop() else {
                    panic!("expected f64 on stack")
                };
                let Some(Value::F64(right)) = self.stack.pop() else {
                    panic!("expected f64 on stack")
                };
                self.stack.push(Value::F64(left + right));
                Ok(())
            }
            OpCode::PushF64 => {
                let value = self.get_f64()?;
                self.stack.push(Value::F64(value));
                Ok(())
            }
            op => unimplemented!("{:?}", op),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        // TODO: make a header
        // TODO: make halt maditory
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
        let byte1 = self.get_u8()? as u16;
        let byte2 = self.get_u8()? as u16;
        Ok(byte1 << 8 | byte2)
    }

    fn get_u32(&mut self) -> Result<u32> {
        let byte1 = self.get_u8()? as u32;
        let byte2 = self.get_u8()? as u32;
        let byte3 = self.get_u8()? as u32;
        let byte4 = self.get_u8()? as u32;
        Ok(byte1 << 24 | byte2 << 16 | byte3 << 8 | byte4)
    }

    fn get_u64(&mut self) -> Result<u64> {
        let byte1 = self.get_u8()? as u64;
        let byte2 = self.get_u8()? as u64;
        let byte3 = self.get_u8()? as u64;
        let byte4 = self.get_u8()? as u64;
        let byte5 = self.get_u8()? as u64;
        let byte6 = self.get_u8()? as u64;
        let byte7 = self.get_u8()? as u64;
        let byte8 = self.get_u8()? as u64;
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
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_opcode() {
        assert_eq!(OpCode::try_from(0), Ok(OpCode::Noop));
        assert_eq!(OpCode::try_from(1), Ok(OpCode::Halt));
    }

    #[test]
    fn test_machine_get_op_code() -> Result<()> {
        let mut machine = Machine::new(vec![0, 1]);
        assert_eq!(machine.get_op_code()?, OpCode::Noop);
        assert_eq!(machine.get_op_code()?, OpCode::Halt);
        Ok(())
    }
}
