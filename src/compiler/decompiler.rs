use super::{Header, Value};
use crate::vm::OpCode;

#[derive(Debug)]
pub enum Instruction {
    Noop,
    Halt,
    AddF64 {
        count: u32,
    },
    Eq {
        count: u32,
    },
    PushBool {
        value: bool,
    },
    PushU8 {
        value: u8,
    },
    PushF64 {
        value: f64,
    },
    PushString {
        length: u32,
        value: String,
    },
    Swap,
    Dup,
    Rot,
    LoadLocalVar {
        index: u32,
    },
    GetLocalVar {
        index: u32,
    },
    LoadGlobalVar,
    GetGlobalVar {
        index: u32,
    },
    Call {
        index: u32,
    },
    Return,
    BuiltIn {
        count_of_args: u32,
        name_length: u32,
        name: String,
    },
    LoadTest {
        name_length: u32,
        name: String,
        index: u32,
    },
    JumpIfFalse {
        offset: u32,
    },
    JumpForward {
        offset: u32,
    },
    LoadLambda {
        byte_count: u32,
    },
    CallLambda {
        arg_count: u32,
    },
}

impl Instruction {
    pub fn size(&self) -> u32 {
        match self {
            Self::Noop => 1,
            Self::Halt => 1,
            Self::AddF64 { .. } => 1 + 4,
            Self::Eq { .. } => 1 + 4,
            Self::PushBool { .. } => 1 + 1,
            Self::PushU8 { .. } => 1 + 1,
            Self::PushF64 { .. } => 1 + 8,
            Self::PushString { length, .. } => 1 + 4 + length,
            Self::Swap => 1,
            Self::Dup => 1,
            Self::Rot => 1,
            Self::LoadLocalVar { .. } => 1 + 4,
            Self::GetLocalVar { .. } => 1 + 4,
            Self::LoadGlobalVar => 1,
            Self::GetGlobalVar { .. } => 1 + 4,
            Self::Call { .. } => 1 + 4,
            Self::Return => 1,
            Self::BuiltIn { name_length, .. } => 1 + 4 + 4 + name_length,
            Self::LoadTest { name_length, .. } => 1 + 4 + name_length + 4,
            Self::JumpIfFalse { .. } => 1 + 4,
            Self::JumpForward { .. } => 1 + 4,
            Self::LoadLambda { .. } => 1 + 4,
            Self::CallLambda { .. } => 1 + 4,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Instruction::Noop => vec![OpCode::Noop as u8],
            Instruction::Halt => vec![OpCode::Halt as u8],
            Instruction::AddF64 { count } => {
                let mut bytes = vec![OpCode::AddF64 as u8];
                bytes.extend(count.to_le_bytes());
                bytes
            }
            Instruction::Eq { count } => {
                let mut bytes = vec![OpCode::Eq as u8];
                bytes.extend(count.to_le_bytes());
                bytes
            }
            Instruction::PushBool { value } => {
                let mut bytes = vec![OpCode::PushBool as u8];
                bytes.extend((*value as u8).to_le_bytes());
                bytes
            }
            Instruction::PushU8 { value } => {
                let mut bytes = vec![OpCode::PushU8 as u8];
                bytes.extend(value.to_le_bytes());
                bytes
            }
            Instruction::PushF64 { value } => {
                let mut bytes = vec![OpCode::PushF64 as u8];
                bytes.extend(value.to_le_bytes());
                bytes
            }
            Instruction::PushString { length, value } => {
                let mut bytes = vec![OpCode::PushString as u8];
                bytes.extend(length.to_le_bytes());
                bytes.extend(value.as_bytes());
                bytes
            }
            Instruction::Swap => vec![OpCode::Swap as u8],
            Instruction::Dup => vec![OpCode::Dup as u8],
            Instruction::Rot => vec![OpCode::Rot as u8],
            Instruction::LoadLocalVar { index } => {
                let mut bytes = vec![OpCode::LoadLocalVar as u8];
                bytes.extend(index.to_le_bytes());
                bytes
            }
            Instruction::GetLocalVar { index } => {
                let mut bytes = vec![OpCode::GetLocalVar as u8];
                bytes.extend(index.to_le_bytes());
                bytes
            }
            Instruction::LoadGlobalVar => vec![OpCode::LoadGlobalVar as u8],
            Instruction::GetGlobalVar { index } => {
                let mut bytes = vec![OpCode::GetGlobalVar as u8];
                bytes.extend(index.to_le_bytes());
                bytes
            }
            Instruction::Call { index } => {
                let mut bytes = vec![OpCode::Call as u8];
                bytes.extend(index.to_le_bytes());
                bytes
            }
            Instruction::Return => vec![OpCode::Return as u8],
            Instruction::BuiltIn {
                count_of_args,
                name_length,
                name,
            } => {
                let mut bytes = vec![OpCode::BuiltIn as u8];
                bytes.extend(count_of_args.to_le_bytes());
                bytes.extend(name_length.to_le_bytes());
                bytes.extend(name.as_bytes());
                bytes
            }
            Instruction::LoadTest {
                name_length,
                name,
                index,
            } => {
                let mut bytes = vec![OpCode::LoadTest as u8];
                bytes.extend(name_length.to_le_bytes());
                bytes.extend(name.as_bytes());
                bytes.extend(index.to_le_bytes());
                bytes
            }
            Instruction::JumpIfFalse { offset } => {
                let mut bytes = vec![OpCode::JumpIfFalse as u8];
                bytes.extend(offset.to_le_bytes());
                bytes
            }
            Instruction::JumpForward { offset } => {
                let mut bytes = vec![OpCode::JumpForward as u8];
                bytes.extend(offset.to_le_bytes());
                bytes
            }
            Instruction::LoadLambda { byte_count } => {
                let mut bytes = vec![OpCode::LoadLambda as u8];
                bytes.extend(byte_count.to_le_bytes());
                bytes
            }
            Instruction::CallLambda { arg_count } => {
                let mut bytes = vec![OpCode::CallLambda as u8];
                bytes.extend(arg_count.to_le_bytes());
                bytes
            }
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Noop => write!(
                f,
                "{:02X}: Noop, size: {:02X}",
                OpCode::Noop as u8,
                self.size()
            ),
            Instruction::Halt => write!(
                f,
                "{:02X}: Halt, size: {:02X}",
                OpCode::Halt as u8,
                self.size()
            ),
            Instruction::AddF64 { count } => {
                write!(
                    f,
                    "{:02X}: AddF64 {:02X}, size: {:02X}",
                    OpCode::AddF64 as u8,
                    count,
                    self.size()
                )
            }
            Instruction::Eq { count } => {
                write!(
                    f,
                    "{:02X}: Eq {:02X}, size: {:02X}",
                    OpCode::Eq as u8,
                    count,
                    self.size()
                )
            }
            Instruction::PushBool { value } => {
                write!(
                    f,
                    "{:02X}: PushBool {}, size: {:02X}",
                    OpCode::PushBool as u8,
                    value,
                    self.size()
                )
            }
            Instruction::PushU8 { value } => {
                write!(
                    f,
                    "{:02X}: PushU8 {:02X}, size: {:02X}",
                    OpCode::PushU8 as u8,
                    value,
                    self.size()
                )
            }
            Instruction::PushF64 { value } => {
                write!(
                    f,
                    "{:02X}: PushF64 {:.2}, size: {:02X}",
                    OpCode::PushF64 as u8,
                    value,
                    self.size()
                )
            }
            Instruction::PushString { length, value } => {
                write!(
                    f,
                    "{:02X}: PushString {:02X} {:?}, size: {:02X}",
                    OpCode::PushString as u8,
                    length,
                    value,
                    self.size(),
                )
            }
            Instruction::Swap => write!(
                f,
                "{:02X}: Swap, size: {:02X}",
                OpCode::Swap as u8,
                self.size()
            ),
            Instruction::Dup => write!(
                f,
                "{:02X}: Dup, size: {:02X}",
                OpCode::Dup as u8,
                self.size()
            ),
            Instruction::Rot => write!(
                f,
                "{:02X}: Rot, size: {:02X}",
                OpCode::Rot as u8,
                self.size()
            ),
            Instruction::LoadLocalVar { index } => {
                write!(
                    f,
                    "{:02X}: LoadLocalVar {:02X}, size: {:02X}",
                    OpCode::LoadLocalVar as u8,
                    index,
                    self.size(),
                )
            }
            Instruction::GetLocalVar { index } => {
                write!(
                    f,
                    "{:02X}: GetLocalVar {:02X}, size: {:02X}",
                    OpCode::GetLocalVar as u8,
                    index,
                    self.size(),
                )
            }
            Instruction::LoadGlobalVar => {
                write!(
                    f,
                    "{:02X}: LoadGlobalVar, size: {:02X}",
                    OpCode::LoadGlobalVar as u8,
                    self.size()
                )
            }
            Instruction::GetGlobalVar { index } => {
                write!(
                    f,
                    "{:02X}: GetGlobalVar {:02X}, size: {:02X}",
                    OpCode::GetGlobalVar as u8,
                    index,
                    self.size(),
                )
            }
            Instruction::Call { index } => {
                write!(
                    f,
                    "{:02X}: Call {:02X}, size: {:02X}",
                    OpCode::Call as u8,
                    index,
                    self.size()
                )
            }
            Instruction::Return => write!(
                f,
                "{:02X}: Return, size: {:02X}",
                OpCode::Return as u8,
                self.size()
            ),
            Instruction::BuiltIn {
                count_of_args,
                name_length,
                name,
            } => write!(
                f,
                "{:02X}: BuiltIn {:02X} {:02X} {:?}, size: {:02X}",
                OpCode::BuiltIn as u8,
                count_of_args,
                name_length,
                name,
                self.size(),
            ),
            Instruction::LoadTest {
                name_length,
                name,
                index,
            } => {
                write!(
                    f,
                    "{:02X}: LoadTest {:02X} {:?} {:02X}, size: {:02X}",
                    OpCode::LoadTest as u8,
                    name_length,
                    name,
                    index,
                    self.size(),
                )
            }
            Instruction::JumpIfFalse { offset } => {
                write!(
                    f,
                    "{:02X}: JumpIfFalse {:02X}, size: {:02X}",
                    OpCode::JumpIfFalse as u8,
                    offset,
                    self.size(),
                )
            }
            Instruction::JumpForward { offset } => {
                write!(
                    f,
                    "{:02X}: JumpForward {:02X}, size: {:02X}",
                    OpCode::JumpForward as u8,
                    offset,
                    self.size(),
                )
            }
            Instruction::LoadLambda { byte_count } => {
                write!(
                    f,
                    "{:02X}: LoadLambda {:02X}, size: {:02X}",
                    OpCode::LoadLambda as u8,
                    byte_count,
                    self.size(),
                )
            }
            Instruction::CallLambda { arg_count } => {
                write!(
                    f,
                    "{:02X}: CallLambda {:02X}, size: {:02X}",
                    OpCode::CallLambda as u8,
                    arg_count,
                    self.size(),
                )
            }
        }
    }
}

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
            let index = u32::from_le_bytes(bytes[*ip + 1..*ip + 5].try_into().unwrap());
            *ip += 5;
            Instruction::Call { index }
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
        OpCode::CallLambda => {
            *ip += 1;
            let arg_count = u32::from_le_bytes(bytes[*ip..*ip + 4].try_into().unwrap());
            *ip += 4;
            Instruction::CallLambda { arg_count }
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
