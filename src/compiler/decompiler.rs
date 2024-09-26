use super::{Header, Instruction, Value};
use crate::vm::OpCode;

pub fn decompile(bytes: &[u8]) -> Result<(Header, Vec<Instruction>), (String, Vec<Instruction>)> {
    let header = Header::from(&bytes[0..Header::SIZE as usize]);
    let mut instructions = Vec::new();
    let mut ip = Header::SIZE as usize;
    while ip < bytes.len() {
        let instruction = match get_instructions(&bytes, &mut ip) {
            Ok(instruction) => instruction,
            Err(e) => return Err((e, instructions)),
        };
        // eprintln!("{:02X} {:?}", instruction.size(), instruction);
        instructions.push(instruction);
    }
    debug_assert_eq!(ip, bytes.len());
    Ok((header, instructions))
}

pub fn decompile_chunk(bytes: &[u8]) -> Result<Vec<Instruction>, (String, Vec<Instruction>)> {
    let mut instructions = Vec::new();
    let mut ip = 0;
    while ip < bytes.len() {
        let instruction = match get_instructions(&bytes, &mut ip) {
            Ok(instruction) => instruction,
            Err(e) => return Err((e, instructions)),
        };
        instructions.push(instruction);
    }
    debug_assert_eq!(ip, bytes.len());
    Ok(instructions)
}

fn get_instructions(bytes: &[u8], ip: &mut usize) -> Result<Instruction, String> {
    let op = match OpCode::try_from(bytes[*ip]) {
        Ok(op) => op,
        Err(e) => return Err(e.to_string()),
    };

    Ok(match op {
        OpCode::Noop => {
            *ip += 1;
            Instruction::Noop
        }
        OpCode::Halt => {
            *ip += 1;
            Instruction::Halt
        }
        OpCode::AddF64 => {
            let count = u32::from_le_bytes(bytes[*ip + 1..*ip + 5].try_into().unwrap());
            *ip += 5;
            Instruction::AddF64 { count }
        }
        OpCode::Sub => todo!(),
        OpCode::Mul => todo!(),
        OpCode::Div => todo!(),
        OpCode::Mod => todo!(),
        OpCode::Eq => {
            let count = u32::from_le_bytes(bytes[*ip + 1..*ip + 5].try_into().unwrap());
            *ip += 5;
            Instruction::Eq { count }
        }
        OpCode::Gt => todo!(),
        OpCode::Lt => todo!(),
        OpCode::Gte => todo!(),
        OpCode::Lte => todo!(),
        OpCode::TypeOf => todo!(),
        OpCode::And => todo!(),
        OpCode::Or => todo!(),
        OpCode::Not => todo!(),
        OpCode::PushBool => {
            *ip += 1;
            let value = u8::from_le_bytes(bytes[*ip..*ip + 1].try_into().unwrap());
            *ip += 1;
            Instruction::PushBool { value: value != 0 }
        }
        OpCode::PushU8 => {
            *ip += 1;
            let value = u8::from_le_bytes(bytes[*ip..*ip + 1].try_into().unwrap());
            *ip += 1;
            Instruction::PushU8 { value }
        }
        OpCode::PushF64 => {
            let value = f64::from_le_bytes(bytes[*ip + 1..*ip + 9].try_into().unwrap());
            *ip += 9;
            Instruction::PushF64 { value }
        }
        OpCode::PushString => {
            *ip += 1;
            let length = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            let value = String::from_utf8(bytes[*ip..*ip + length as usize].to_vec()).unwrap();
            *ip += length as usize;
            Instruction::PushString { length, value }
        }
        OpCode::PopF64 => todo!(),
        OpCode::Swap => {
            *ip += 1;
            Instruction::Swap
        }
        OpCode::Dup => todo!("Dup is not implemented"),
        OpCode::Rot => {
            *ip += 1;
            Instruction::Rot
        }
        OpCode::LoadLocalVar => {
            let index = u32::from_le_bytes(bytes[*ip + 1..*ip + 5].try_into().unwrap());
            *ip += 5;
            Instruction::LoadLocalVar { index }
        }
        OpCode::GetLocalVar => {
            let index = u32::from_le_bytes(bytes[*ip + 1..*ip + 5].try_into().unwrap());
            *ip += 5;
            Instruction::GetLocalVar { index }
        }
        OpCode::LoadGlobalVar => {
            *ip += 1;
            Instruction::LoadGlobalVar
        }
        OpCode::GetGlobalVar => {
            let index = u32::from_le_bytes(bytes[*ip + 1..*ip + 5].try_into().unwrap());
            *ip += 5;
            Instruction::GetGlobalVar { index }
        }
        OpCode::Call => {
            *ip += 1;
            let index = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            let is_lambda = u8::from_le_bytes(bytes[*ip..*ip + 1].try_into().unwrap()) != 0;
            *ip += 1;
            Instruction::Call { index, is_lambda }
        }
        OpCode::Return => {
            *ip += 1;
            Instruction::Return
        }
        OpCode::BuiltIn => {
            *ip += 1;
            let count_of_args = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            let name_length = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            let name = String::from_utf8(bytes[*ip..*ip + name_length as usize].to_vec()).unwrap();
            *ip += name_length as usize;
            Instruction::BuiltIn {
                count_of_args,
                name_length,
                name,
            }
        }
        OpCode::LoadTest => {
            *ip += 1;
            let name_length = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            let name = String::from_utf8(bytes[*ip..*ip + name_length as usize].to_vec()).unwrap();
            *ip += name_length as usize;
            let index = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Instruction::LoadTest {
                name_length,
                name,
                index,
            }
        }
        OpCode::JumpIfFalse => {
            *ip += 1;
            let offset = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Instruction::JumpIfFalse { offset }
        }
        OpCode::JumpForward => {
            *ip += 1;
            let offset = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Instruction::JumpForward { offset }
        }
        OpCode::LoadLambda => {
            *ip += 1;
            let byte_count = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Instruction::LoadLambda { byte_count }
        }
    })
}

pub fn display_chunk(bytes: &[u8]) {
    let instructions = match decompile_chunk(&bytes) {
        Ok(result) => result,
        Err((e, result)) => {
            println!("{:?}", e);
            result
        }
    };

    for i in instructions {
        eprintln!("{}", i);
        // eprintln!("{:?}", i.to_bytecode());
    }
}
