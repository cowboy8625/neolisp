use super::{builtin, Header, OpCode, Value};
use crate::compiler::decompile;
use anyhow::Result;
const RED: &str = "\x1b[31m";
const GREEN: &str = "\x1b[32m";
const RESET: &str = "\x1b[0m";

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
    decompile: bool,
}

impl Machine {
    pub fn new(program: Vec<u8>, decompile: bool) -> Self {
        debug_assert!(program.len() > Header::SIZE as usize);
        let program_header = Header::from(&program[0..Header::SIZE as usize]);
        Self {
            program,
            ip: program_header.start as usize,
            is_running: true,
            stack: vec![],
            local_var: vec![],
            global_var: vec![],
            decompile,
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
            OpCode::Eq => {
                // TODO: Fix
                let count = self.get_u32()?;
                let all = self.stack.split_off(self.stack.len() - count as usize);
                let start = all.first().expect("expected value on stack for Eq").clone();
                let mut result = false;
                for value in all.iter() {
                    result = value == &start;
                }
                self.stack.push(Value::Bool(result));
                Ok(())
            }
            OpCode::PushBool => {
                let value = self.get_u8()?;
                self.stack.push(Value::Bool(value != 0));
                Ok(())
            }
            OpCode::PushU8 => {
                let value = self.get_u8()?;
                self.stack.push(Value::U8(value));
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
            OpCode::BuiltIn => self.builtins(),
            OpCode::LoadTest => {
                let name = self.get_string()?;
                let address = self.get_u32()? as usize;
                self.stack.push(Value::U32(self.ip as u32));
                self.stack.push(Value::String(name));
                self.ip = address;
                Ok(())
            }
            OpCode::JumpIfFalse => {
                let jump_offset = self.get_u32()? as usize;
                if self.stack.pop() == Some(Value::Bool(false)) {
                    self.ip += jump_offset;
                }
                Ok(())
            }
            OpCode::JumpForward => {
                let jump_offset = self.get_u32()? as usize;
                self.ip += jump_offset;
                Ok(())
            }
            op => unimplemented!("{:?}", op),
        }
    }

    fn debug(&self) {
        eprintln!("---- VM ----");
        eprintln!("ip: {:02X}", self.ip - Header::SIZE as usize);
        let instructions = match decompile(&self.program) {
            Ok((_, instructions)) => instructions,
            Err((e, instructions)) => {
                eprintln!("{}", e);
                instructions
            }
        };
        let mut program_counter = Header::SIZE as usize;
        for i in instructions {
            let selected = if self.ip == program_counter {
                format!("{GREEN}{:02X} ", program_counter - Header::SIZE as usize)
            } else if (program_counter..program_counter + i.size() as usize).contains(&self.ip) {
                format!("{RED}{:02X} ", program_counter - Header::SIZE as usize)
            } else {
                format!("{:02X} ", program_counter - Header::SIZE as usize)
            };
            eprintln!("{selected}{i}{RESET}");
            program_counter += i.size() as usize;
        }
    }

    pub fn run(&mut self) -> Result<()> {
        if self.decompile {
            self.debug();
        }
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

    fn builtins(&mut self) -> Result<()> {
        let count = self.get_u32()?;
        let name = self.get_string()?;
        match name.as_str() {
            "nth" => builtin::nth(self, count)?,
            "length" => builtin::length(self, count)?,
            "assert-eq" => builtin::nlvm_assert_eq(self, count)?,
            "list" => builtin::list(self, count)?,
            _ => unimplemented!("{}", name),
        }
        Ok(())
    }
}

impl Machine {
    pub fn push(&mut self, value: Value) {
        self.stack.push(value);
    }

    pub fn pop(&mut self) -> Option<Value> {
        self.stack.pop()
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
        let mut machine = Machine::new(create_test_program(&[0, 1]), false);
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
