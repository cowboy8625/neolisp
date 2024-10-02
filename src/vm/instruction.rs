use super::value::Value;

use num_derive::{FromPrimitive, ToPrimitive};

#[repr(u8)]
#[derive(Debug, Clone, PartialEq, FromPrimitive, ToPrimitive)]
pub enum OpCode {
    StartAt,
    Noop,
    Halt,
    Return,
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
    LoadLocal,
    GetLocal,
    LoadGlobal,
    GetGlobal,
    LoadFree,
    GetFree,
    JumpIf,
    Jump,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    StartAt(usize),
    Noop,
    Halt,
    Return,
    Push(Value),
    Add(u8),
    Sub(u8),
    Mul(u8),
    Div(u8),
    Eq(u8),
    GreaterThan(u8),
    LessThan(u8),
    GreaterThanOrEqual(u8),
    LessThanOrEqual(u8),
    And(u8),
    Or,
    Not,
    Mod,
    Rot,
    Call(Callee, u8),
    LoadLocal,
    GetLocal(usize),
    LoadGlobal,
    GetGlobal(usize),
    LoadFree,
    GetFree(usize),
    JumpIf(usize),
    Jump(Direction),
}

impl Instruction {
    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Instruction::StartAt(v) => {
                let mut bytes = vec![OpCode::StartAt as u8];
                bytes.extend_from_slice(&(*v as u32).to_le_bytes());
                bytes
            }
            Instruction::Noop => vec![OpCode::Noop as u8],
            Instruction::Halt => vec![OpCode::Halt as u8],
            Instruction::Return => vec![OpCode::Return as u8],
            Instruction::Push(value) => {
                let mut bytes = vec![OpCode::Push as u8];
                bytes.extend_from_slice(&value.to_bytecode());
                bytes
            }
            Instruction::Add(count) => vec![OpCode::Add as u8, *count],
            Instruction::Sub(count) => vec![OpCode::Sub as u8, *count],
            Instruction::Mul(count) => vec![OpCode::Mul as u8, *count],
            Instruction::Div(count) => vec![OpCode::Div as u8, *count],
            Instruction::Eq(count) => vec![OpCode::Eq as u8, *count],
            Instruction::GreaterThan(count) => vec![OpCode::GreaterThan as u8, *count],
            Instruction::LessThan(count) => vec![OpCode::LessThan as u8, *count],
            Instruction::GreaterThanOrEqual(count) => {
                vec![OpCode::GreaterThanOrEqual as u8, *count]
            }
            Instruction::LessThanOrEqual(count) => vec![OpCode::LessThanOrEqual as u8, *count],
            Instruction::And(count) => vec![OpCode::And as u8, *count],
            Instruction::Or => vec![OpCode::Or as u8],
            Instruction::Not => vec![OpCode::Not as u8],
            Instruction::Mod => vec![OpCode::Mod as u8],
            Instruction::Rot => vec![OpCode::Rot as u8],
            Instruction::Call(callee, count) => {
                let mut bytes = vec![OpCode::Call as u8];
                bytes.extend_from_slice(&callee.to_bytecode());
                bytes.push(*count);
                bytes
            }
            Instruction::LoadLocal => vec![OpCode::LoadLocal as u8],
            Instruction::GetLocal(index) => {
                let mut bytes = vec![OpCode::GetLocal as u8];
                bytes.push(*index as u8);
                // bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::LoadGlobal => vec![OpCode::LoadGlobal as u8],
            Instruction::GetGlobal(index) => {
                let mut bytes = vec![OpCode::GetGlobal as u8];
                bytes.push(*index as u8);
                // bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::LoadFree => vec![OpCode::LoadFree as u8],
            Instruction::GetFree(index) => {
                let mut bytes = vec![OpCode::GetFree as u8];
                bytes.push(*index as u8);
                // bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::JumpIf(address) => {
                let mut bytes = vec![OpCode::JumpIf as u8];
                bytes.extend_from_slice(&(*address as u32).to_le_bytes());
                bytes
            }
            Instruction::Jump(direction) => {
                let mut bytes = vec![OpCode::Jump as u8];
                bytes.extend_from_slice(&direction.to_bytecode());
                bytes
            }
        }
    }

    pub fn size(&self) -> usize {
        // NOTE: if anything changes here update to Stage1Instruction.size as well
        match self {
            Self::StartAt(_) => 5,
            Self::Add(_)
            | Self::Sub(_)
            | Self::Mul(_)
            | Self::Div(_)
            | Self::Eq(_)
            | Self::GreaterThan(_)
            | Self::LessThan(_)
            | Self::GreaterThanOrEqual(_)
            | Self::LessThanOrEqual(_)
            | Self::And(_) => 2,
            Self::Or
            | Self::Not
            | Self::Mod
            | Self::Rot
            | Self::Noop
            | Self::Halt
            | Self::LoadGlobal
            | Self::LoadLocal
            | Self::LoadFree
            | Self::Return => 1,
            Self::Push(value) => 1 + value.size(),
            Self::Call(callee, _) => 1 + callee.size() + 1,
            Self::GetLocal(_) | Self::GetGlobal(_) | Self::GetFree(_) => 2,
            Self::JumpIf(_) => 5,
            Self::Jump(direction) => 1 + direction.size(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Direction {
    Forward(usize),
    Backward(usize),
}

impl Direction {
    pub const OPCODE_FORWARD: u8 = 0x00;
    pub const OPCODE_BACKWARD: u8 = 0x01;
    pub fn to_bytecode(self) -> Vec<u8> {
        match self {
            Direction::Forward(address) => {
                let mut bytes = vec![Self::OPCODE_FORWARD];
                bytes.extend_from_slice(&(address as u32).to_le_bytes());
                bytes
            }
            Direction::Backward(address) => {
                let mut bytes = vec![Self::OPCODE_BACKWARD];
                bytes.extend_from_slice(&(address as u32).to_le_bytes());
                bytes
            }
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Direction::Forward(_) => 5,
            Direction::Backward(_) => 5,
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

    pub fn size(&self) -> usize {
        match self {
            Callee::Function => 1,
            Callee::Builtin(name) => 1 + 1 + name.len(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn test_size_vs_bytes_len(i: Instruction) {
        assert_eq!(i.size(), i.to_bytecode().len(), "{i:?}");
    }

    #[test]
    fn test_junk() {
        let i = vec![
            Instruction::Push(Value::String("then\n".to_string())),
            Instruction::Call(Callee::Builtin("print".to_string()), 1),
            Instruction::Jump(Direction::Forward(19)),
        ];
        let size = i.iter().map(|i| i.size()).sum::<usize>();
        eprintln!("{}", size);
        let bytes_len = i.iter().map(|i| i.to_bytecode().len()).sum::<usize>();
        eprintln!("len: {}", bytes_len);

        assert_eq!(
            Instruction::Push(Value::String("then\n".to_string())).size(),
            Instruction::Push(Value::String("then\n".to_string()))
                .to_bytecode()
                .len(),
        );
        test_size_vs_bytes_len(Instruction::StartAt(5));
        test_size_vs_bytes_len(Instruction::Push(Value::Bool(false)));
        test_size_vs_bytes_len(Instruction::LoadGlobal);
        test_size_vs_bytes_len(Instruction::GetGlobal(0));
        test_size_vs_bytes_len(Instruction::JumpIf(24));
        test_size_vs_bytes_len(Instruction::Push(Value::String("then\n".to_string())));
        test_size_vs_bytes_len(Instruction::Call(Callee::Builtin("print".to_string()), 1));
        test_size_vs_bytes_len(Instruction::Jump(Direction::Forward(19)));
        test_size_vs_bytes_len(Instruction::Push(Value::String("else\n".to_string())));
        test_size_vs_bytes_len(Instruction::Call(Callee::Builtin("print".to_string()), 1));
        test_size_vs_bytes_len(Instruction::Halt);
    }
}
