use super::{Header, Value};
use crate::vm::OpCode;

#[derive(Debug)]
pub enum Instruction {
    Noop,
    Halt,
    AddF64 { count: u32 },
    PushF64,
    PushString { length: u32, value: String },
    Swap,
    Dup,
    Rot,
    LoadLocalVar { index: u32 },
    GetLocalVar { index: u32 },
    LoadGlobalVar,
    GetGlobalVar { index: u32 },
    Call { index: u32 },
    Return,
}

pub fn decompile(bytes: &[u8]) -> Result<(Header, Vec<Instruction>), (String, Vec<Instruction>)> {
    let header = Header::from(&bytes[0..Header::SIZE as usize]);
    let mut instructions = Vec::new();
    let mut ip = Header::SIZE as usize;
    while ip < bytes.len() {
        let op = match OpCode::try_from(bytes[ip]) {
            Ok(op) => op,
            Err(e) => return Err((e.to_string(), instructions)),
        };
        match op {
            OpCode::Noop => {
                instructions.push(Instruction::Noop);
                ip += 1;
            }
            OpCode::Halt => {
                instructions.push(Instruction::Halt);
                ip += 1;
            }
            OpCode::AddF64 => {
                let count = u32::from_le_bytes(bytes[ip + 1..ip + 5].try_into().unwrap());
                instructions.push(Instruction::AddF64 { count });
                ip += 5;
            }
            OpCode::Sub => todo!(),
            OpCode::Mul => todo!(),
            OpCode::Div => todo!(),
            OpCode::Mod => todo!(),
            OpCode::Eq => todo!(),
            OpCode::Gt => todo!(),
            OpCode::Lt => todo!(),
            OpCode::Gte => todo!(),
            OpCode::Lte => todo!(),
            OpCode::TypeOf => todo!(),
            OpCode::And => todo!(),
            OpCode::Or => todo!(),
            OpCode::Not => todo!(),
            OpCode::Print => todo!(),
            OpCode::PushU8 => todo!(),
            OpCode::PushF64 => {
                let value = f64::from_le_bytes(bytes[ip + 1..ip + 9].try_into().unwrap());
                instructions.push(Instruction::PushF64);
                ip += 9;
            }
            OpCode::PushString => todo!(),
            OpCode::PopF64 => todo!(),
            OpCode::Swap => {
                instructions.push(Instruction::Swap);
                ip += 1;
            }
            OpCode::Dup => todo!("Dup is not implemented"),
            OpCode::Rot => {
                instructions.push(Instruction::Rot);
                ip += 1;
            }
            OpCode::LoadLocalVar => {
                let index = u32::from_le_bytes(bytes[ip + 1..ip + 5].try_into().unwrap());
                instructions.push(Instruction::LoadLocalVar { index });
                ip += 5;
            }
            OpCode::GetLocalVar => {
                let index = u32::from_le_bytes(bytes[ip + 1..ip + 5].try_into().unwrap());
                instructions.push(Instruction::GetLocalVar { index });
                ip += 5;
            }
            OpCode::LoadGlobalVar => {
                instructions.push(Instruction::LoadGlobalVar);
                ip += 5;
            }
            OpCode::GetGlobalVar => {
                let index = u32::from_le_bytes(bytes[ip + 1..ip + 5].try_into().unwrap());
                instructions.push(Instruction::GetGlobalVar { index });
                ip += 5;
            }
            OpCode::Call => {
                let index = u32::from_le_bytes(bytes[ip + 1..ip + 5].try_into().unwrap());
                instructions.push(Instruction::Call { index });
                ip += 5;
            }
            OpCode::Return => {
                instructions.push(Instruction::Return);
                ip += 1;
            }
            OpCode::CreateList => todo!(),
        }
    }
    Ok((header, instructions))
}
