use super::BUILTINS;
use num_derive::{FromPrimitive, ToPrimitive};
#[derive(Debug, Clone, PartialEq, FromPrimitive, ToPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Halt,
    Return,
    ReturnFromTest,
    Push,
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    And,
    Or,
    Not,
    Mod,
    Rot,
    Call,
    CallTest,
    TailCall,
    SetLocal,
    SetGlobal,
    SetFree,
    GetLocal,
    GetGlobal,
    GetFree,
    JumpIf,
    JumpForward,
    JumpBackward,
    Jump,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Halt,
    Return,
    ReturnFromTest,
    Push(Box<Value>),
    Add(usize),
    Sub(usize),
    Mul(usize),
    Div(usize),
    Eq(usize),
    GreaterThan(usize),
    LessThan(usize),
    GreaterThanOrEqual(usize),
    LessThanOrEqual(usize),
    And(usize),
    Or(usize),
    Not,
    Mod,
    Rot,
    Call(usize),
    CallTest,
    TailCall(usize),
    SetLocal(RuntimeMetadata),
    SetGlobal(RuntimeMetadata),
    SetFree(RuntimeMetadata),
    GetLocal(RuntimeMetadata),
    GetGlobal(RuntimeMetadata),
    GetFree(RuntimeMetadata),
    JumpIf(usize),
    JumpForward(usize),
    JumpBackward(usize),
    Jump(usize),
}

impl Instruction {
    pub fn size(&self) -> usize {
        match self {
            Self::Halt => 1,
            Self::Return => 1,
            Self::ReturnFromTest => 1,
            Self::Push(value) => 1 + value.size(),
            Self::Add(_) => 2,
            Self::Sub(_) => 2,
            Self::Mul(_) => 2,
            Self::Div(_) => 2,
            Self::Eq(_) => 2,
            Self::GreaterThan(_) => 2,
            Self::LessThan(_) => 2,
            Self::GreaterThanOrEqual(_) => 2,
            Self::LessThanOrEqual(_) => 2,
            Self::And(_) => 2,
            Self::Or(_) => 2,
            Self::Not => 1,
            Self::Mod => 1,
            Self::Rot => 1,
            Self::Call(_) => 2,
            Self::CallTest => 1,
            Self::TailCall(_) => 2,
            Self::SetLocal(md) => 1 + md.size(),
            Self::SetGlobal(md) => 1 + md.size(),
            Self::SetFree(md) => 1 + md.size(),
            Self::GetLocal(md) => 1 + md.size(),
            Self::GetGlobal(md) => 1 + md.size(),
            Self::GetFree(md) => 1 + md.size(),
            Self::JumpIf(_) => 5,
            Self::JumpForward(_) => 5,
            Self::JumpBackward(_) => 5,
            Self::Jump(_) => 5,
        }
    }

    pub fn opcode(&self) -> OpCode {
        match self {
            Self::Halt => OpCode::Halt,
            Self::Return => OpCode::Return,
            Self::ReturnFromTest => OpCode::ReturnFromTest,
            Self::Push(_) => OpCode::Push,
            Self::Add(_) => OpCode::Add,
            Self::Sub(_) => OpCode::Sub,
            Self::Mul(_) => OpCode::Mul,
            Self::Div(_) => OpCode::Div,
            Self::Eq(_) => OpCode::Eq,
            Self::GreaterThan(_) => OpCode::GreaterThan,
            Self::LessThan(_) => OpCode::LessThan,
            Self::GreaterThanOrEqual(_) => OpCode::GreaterThanOrEqual,
            Self::LessThanOrEqual(_) => OpCode::LessThanOrEqual,
            Self::And(_) => OpCode::And,
            Self::Or(_) => OpCode::Or,
            Self::Not => OpCode::Not,
            Self::Mod => OpCode::Mod,
            Self::Rot => OpCode::Rot,
            Self::Call(..) => OpCode::Call,
            Self::CallTest => OpCode::CallTest,
            Self::TailCall(..) => OpCode::TailCall,
            Self::SetLocal(..) => OpCode::SetLocal,
            Self::SetGlobal(..) => OpCode::SetGlobal,
            Self::SetFree(..) => OpCode::SetFree,
            Self::GetLocal(_) => OpCode::GetLocal,
            Self::GetGlobal(_) => OpCode::GetGlobal,
            Self::GetFree(_) => OpCode::GetFree,
            Self::JumpIf(_) => OpCode::JumpIf,
            Self::JumpForward(_) => OpCode::JumpForward,
            Self::JumpBackward(_) => OpCode::JumpBackward,
            Self::Jump(_) => OpCode::Jump,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        match self {
            Self::Halt => bytes.push(OpCode::Halt as u8),
            Self::Return => bytes.push(OpCode::Return as u8),
            Self::ReturnFromTest => bytes.push(OpCode::ReturnFromTest as u8),
            Self::Push(value) => {
                bytes.push(OpCode::Push as u8);
                bytes.extend(&value.to_bytecode());
            }
            Self::Add(count)
            | Self::Sub(count)
            | Self::Mul(count)
            | Self::Div(count)
            | Self::Eq(count)
            | Self::GreaterThan(count)
            | Self::LessThan(count)
            | Self::GreaterThanOrEqual(count)
            | Self::LessThanOrEqual(count)
            | Self::And(count)
            | Self::Or(count) => {
                bytes.push(self.opcode() as u8);
                bytes.push(*count as u8);
            }
            Self::Not | Self::Mod | Self::Rot | Self::CallTest => bytes.push(self.opcode() as u8),
            Self::Call(count) | Self::TailCall(count) => {
                bytes.push(self.opcode() as u8);
                bytes.push(*count as u8);
            }
            Self::GetLocal(metadata)
            | Self::GetGlobal(metadata)
            | Self::GetFree(metadata)
            | Self::SetLocal(metadata)
            | Self::SetGlobal(metadata)
            | Self::SetFree(metadata) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&metadata.to_bytecode());
            }
            Self::JumpIf(address)
            | Self::JumpForward(address)
            | Self::JumpBackward(address)
            | Self::Jump(address) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&(*address as u32).to_le_bytes());
            }
        }

        bytes
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Halt => write!(f, "halt"),
            Instruction::Return => write!(f, "ret"),
            Instruction::ReturnFromTest => write!(f, "ret-test"),
            Instruction::Push(value) => write!(f, "{:<10} {value}", "push"),
            Instruction::Add(count) => write!(f, "{:<10} {count}", "add"),
            Instruction::Sub(count) => write!(f, "{:<10} {count}", "sub"),
            Instruction::Mul(count) => write!(f, "{:<10} {count}", "mul"),
            Instruction::Div(count) => write!(f, "{:<10} {count}", "div"),
            Instruction::Eq(count) => write!(f, "eq {count}"),
            Instruction::GreaterThan(count) => write!(f, "{:<10} {count}", "gt"),
            Instruction::LessThan(count) => write!(f, "{:<10} {count}", "lt"),
            Instruction::GreaterThanOrEqual(count) => write!(f, "{:<10} {count}", "gte"),
            Instruction::LessThanOrEqual(count) => write!(f, "{:<10} {count}", "lte"),
            Instruction::And(count) => write!(f, "{:<10} {count}", "and"),
            Instruction::Or(count) => write!(f, "{:<10} {count}", "or"),
            Instruction::Not => write!(f, "not"),
            Instruction::Mod => write!(f, "mod"),
            Instruction::Rot => write!(f, "rot"),
            Instruction::Call(count) => write!(f, "{:<10} {count}", "call"),
            Instruction::CallTest => write!(f, "call-test"),
            Instruction::TailCall(count) => write!(f, "{:<10} {}", "tail-call", count),
            Instruction::SetLocal(metadata) => write!(f, "{:<10} {metadata}", "set-local"),
            Instruction::SetGlobal(metadata) => write!(f, "{:<10} {metadata}", "set-global"),
            Instruction::SetFree(metadata) => write!(f, "{:<10} {metadata}", "set-free"),
            Instruction::GetLocal(metadata) => write!(f, "{:<10} {metadata}", "get-local"),
            Instruction::GetGlobal(metadata) => write!(f, "{:<10} {metadata}", "get-global"),
            Instruction::GetFree(metadata) => write!(f, "{:<10} {metadata}", "get-free"),
            Instruction::JumpIf(address) => write!(f, "{:<10} {address}", "jump-if"),
            Instruction::JumpForward(address) => write!(f, "{:<10} {address}", "jump-forward"),
            Instruction::JumpBackward(address) => write!(f, "{:<10} {address}", "jump-backward"),
            Instruction::Jump(address) => write!(f, "{:<10} {address}", "jump"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeMetadata {
    pub data: usize,
    pub name: Box<String>,
}

impl RuntimeMetadata {
    pub fn new(data: usize, name: impl Into<String>) -> Self {
        Self {
            data,
            name: Box::new(name.into()),
        }
    }

    pub fn size(&self) -> usize {
        // 4 bytes for address
        // 1 bytes for name length
        // name
        4 + 1 + self.name.len()
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(&(self.data as u32).to_le_bytes());
        if self.name.len() > 255 {
            panic!("Name too long");
        }
        bytes.extend(&(self.name.len() as u8).to_le_bytes());
        bytes.extend(self.name.as_bytes());
        bytes
    }
}

impl std::fmt::Display for RuntimeMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?}", self.data, self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Callable {
    pub address: usize,
    pub name: String,
}

impl Callable {
    pub fn new(address: usize, name: impl Into<String>) -> Self {
        Self {
            address,
            name: name.into(),
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(&(self.address as u32).to_le_bytes());
        bytes.extend(&(self.name.len() as u32).to_le_bytes());
        bytes.extend(self.name.as_bytes());
        bytes
    }

    pub fn size(&self) -> usize {
        4 + 4 + self.name.len()
    }
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.address, self.name)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U8(u8),
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
    String(Box<String>),
    Bool(bool),
    List(Box<Vec<Value>>),
    Callable(Box<Callable>),
    Builtin(usize),
    Symbol(Box<String>),
    Keyword(Box<String>),
}

impl Value {
    pub const CODE_U8: u8 = 0x00;
    pub const CODE_I32: u8 = 0x01;
    pub const CODE_U32: u8 = 0x02;
    pub const CODE_F32: u8 = 0x03;
    pub const CODE_F64: u8 = 0x04;
    pub const CODE_STRING: u8 = 0x05;
    pub const CODE_BOOL: u8 = 0x06;
    pub const CODE_LIST: u8 = 0x07;
    pub const CODE_CALLABLE: u8 = 0x08;
    pub const CODE_BUILTIN: u8 = 0x09;
    pub const CODE_SYMBOL: u8 = 0x0A;
    pub const CODE_KEYWORD: u8 = 0x0B;

    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Value::U8(v) => vec![Self::CODE_U8, *v],
            Value::I32(v) => {
                let mut bytes = vec![Self::CODE_I32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::U32(v) => {
                let mut bytes = vec![Self::CODE_U32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F32(v) => {
                let mut bytes = vec![Self::CODE_F32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F64(v) => {
                let mut bytes = vec![Self::CODE_F64];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::String(v) => {
                let mut bytes = vec![Self::CODE_STRING];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(v.as_bytes());
                bytes
            }
            Value::Bool(v) => vec![Self::CODE_BOOL, if *v { 1 } else { 0 }],
            Value::List(vec) => {
                let mut bytes = vec![Self::CODE_LIST];
                bytes.extend_from_slice(&(vec.len() as u32).to_le_bytes());
                for v in vec.iter() {
                    bytes.extend(v.to_bytecode());
                }
                bytes
            }
            Value::Callable(callable) => {
                let mut bytes = vec![Self::CODE_CALLABLE];
                let callable_bytes = callable.to_bytecode();
                bytes.extend(callable_bytes);
                bytes
            }
            Value::Builtin(index) => {
                let mut bytes = vec![Self::CODE_BUILTIN];
                bytes.extend_from_slice(&(*index as u32).to_le_bytes());
                bytes
            }
            Value::Symbol(v) => {
                let mut bytes = vec![Self::CODE_SYMBOL];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(v.as_bytes());
                bytes
            }
            Value::Keyword(v) => {
                let mut bytes = vec![Self::CODE_KEYWORD];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(v.as_bytes());
                bytes
            }
        }
    }

    pub fn size(&self) -> usize {
        let conent_size = match self {
            Value::U8(_) => 1,
            Value::I32(_) => 4,
            Value::U32(_) => 4,
            Value::F32(_) => 4,
            Value::F64(_) => 8,
            Value::String(v) => 4 + v.len(),
            Value::Bool(_) => 1,
            Value::List(vec) => 4 + vec.iter().map(|v| v.size()).sum::<usize>(),
            // 4 bytes    4 bytes
            // start......end
            Value::Callable(callable) => callable.size(),
            Value::Builtin(_) => 4,
            Value::Symbol(v) => 4 + v.len(),
            Value::Keyword(v) => 4 + v.len(),
        };
        // opcode + content
        1 + conent_size
    }

    pub fn type_of(&self) -> String {
        match self {
            Self::U8(_) => "u8".to_string(),
            Self::I32(_) => "i32".to_string(),
            Self::U32(_) => "u32".to_string(),
            Self::F32(_) => "f32".to_string(),
            Self::F64(_) => "f64".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Bool(_) => "Bool".to_string(),
            Self::List(_) => "List".to_string(),
            Self::Callable(_) => "Function".to_string(),
            Self::Builtin(_) => "Builtin".to_string(),
            Self::Symbol(_) => "Symbol".to_string(),
            Self::Keyword(_) => "Keyword".to_string(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(value) => write!(f, "{value}"),
            Self::I32(value) => write!(f, "{value}"),
            Self::U32(value) => write!(f, "{value}"),
            Self::F32(value) => write!(f, "{value}"),
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
            Self::Callable(data) => write!(f, "<function {data}>"),
            Self::Builtin(index) => write!(f, "<function {:?}>", BUILTINS[*index]),
            Self::Symbol(value) => write!(f, "{value}"),
            Self::Keyword(value) => write!(f, "{value}"),
        }
    }
}
