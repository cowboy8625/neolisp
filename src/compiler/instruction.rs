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
        is_lambda: bool,
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
            Self::Call { .. } => 1 + 4 + 1,
            Self::Return => 1,
            Self::BuiltIn { name_length, .. } => 1 + 4 + 4 + name_length,
            Self::LoadTest { name_length, .. } => 1 + 4 + name_length + 4,
            Self::JumpIfFalse { .. } => 1 + 4,
            Self::JumpForward { .. } => 1 + 4,
            Self::LoadLambda { .. } => 1 + 4,
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
            Instruction::Call { index, is_lambda } => {
                let mut bytes = vec![OpCode::Call as u8];
                bytes.extend(index.to_le_bytes());
                bytes.push(*is_lambda as u8);
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
            Instruction::Call { index, is_lambda } => {
                write!(
                    f,
                    "{:02X}: Call {:02X} {:?}, size: {:02X}",
                    OpCode::Call as u8,
                    index,
                    is_lambda,
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
        }
    }
}
