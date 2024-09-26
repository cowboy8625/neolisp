use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    StartAt(usize),
    Noop,
    Halt,
    Return,
    Push(Value),
    Add,
    Sub,
    Eq,
    Or,
    Rot,
    Call(Callee, u8),
    LoadLocal,
    GetLocal(usize),
    LoadGlobal,
    GetGlobal(usize),
    LoadFree,
    GetFree(usize),
    JumpIf(usize),
    Jump(usize),
}

impl Instruction {
    pub const START_AT: u8 = 0x00;
    pub const NOOP: u8 = 0x01;
    pub const HALT: u8 = 0x02;
    pub const RETURN: u8 = 0x03;
    pub const PUSH: u8 = 0x04;
    pub const ADD: u8 = 0x05;
    pub const SUB: u8 = 0x06;
    pub const EQ: u8 = 0x07;
    pub const OR: u8 = 0x08;
    pub const ROT: u8 = 0x09;
    pub const CALL: u8 = 0x0A;
    pub const LOAD_LOCAL: u8 = 0x0B;
    pub const GET_LOCAL: u8 = 0x0C;
    pub const LOAD_GLOBAL: u8 = 0x0D;
    pub const GET_GLOBAL: u8 = 0x0E;
    pub const LOAD_FREE: u8 = 0x0F;
    pub const GET_FREE: u8 = 0x10;
    pub const JUMP_IF: u8 = 0x11;
    pub const JUMP: u8 = 0x12;

    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Instruction::StartAt(v) => {
                let mut bytes = vec![Self::START_AT];
                bytes.extend_from_slice(&(*v as u32).to_le_bytes());
                bytes
            }
            Instruction::Noop => vec![Self::NOOP],
            Instruction::Halt => vec![Self::HALT],
            Instruction::Return => vec![Self::RETURN],
            Instruction::Push(value) => {
                let mut bytes = vec![Self::PUSH];
                bytes.extend_from_slice(&value.to_bytecode());
                bytes
            }
            Instruction::Add => vec![Self::ADD],
            Instruction::Sub => vec![Self::SUB],
            Instruction::Eq => vec![Self::EQ],
            Instruction::Or => vec![Self::OR],
            Instruction::Rot => vec![Self::ROT],
            Instruction::Call(callee, count) => {
                let mut bytes = vec![Self::CALL];
                bytes.extend_from_slice(&callee.to_bytecode());
                bytes.extend_from_slice(&count.to_le_bytes());
                bytes
            }
            Instruction::LoadLocal => vec![Self::LOAD_LOCAL],
            Instruction::GetLocal(index) => {
                let mut bytes = vec![Self::GET_LOCAL];
                bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::LoadGlobal => vec![Self::LOAD_GLOBAL],
            Instruction::GetGlobal(index) => {
                let mut bytes = vec![Self::GET_GLOBAL];
                bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::LoadFree => vec![Self::LOAD_FREE],
            Instruction::GetFree(index) => {
                let mut bytes = vec![Self::GET_FREE];
                bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::JumpIf(address) => {
                let mut bytes = vec![Self::JUMP_IF];
                bytes.extend_from_slice(&address.to_le_bytes());
                bytes
            }
            Instruction::Jump(address) => {
                let mut bytes = vec![Self::JUMP];
                bytes.extend_from_slice(&address.to_le_bytes());
                bytes
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Function,
    Builtin(String),
}

impl Callee {
    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Callee::Function => vec![0x00],
            Callee::Builtin(name) => {
                let mut bytes = vec![0x01];
                bytes.push(name.len() as u8);
                bytes.extend_from_slice(name.as_bytes());
                bytes
            }
        }
    }
}
