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
                bytes.push(*count as u8);
                bytes
            }
            Instruction::LoadLocal => vec![Self::LOAD_LOCAL],
            Instruction::GetLocal(index) => {
                let mut bytes = vec![Self::GET_LOCAL];
                bytes.push(*index as u8);
                // bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::LoadGlobal => vec![Self::LOAD_GLOBAL],
            Instruction::GetGlobal(index) => {
                let mut bytes = vec![Self::GET_GLOBAL];
                bytes.push(*index as u8);
                // bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::LoadFree => vec![Self::LOAD_FREE],
            Instruction::GetFree(index) => {
                let mut bytes = vec![Self::GET_FREE];
                bytes.push(*index as u8);
                // bytes.extend_from_slice(&index.to_le_bytes());
                bytes
            }
            Instruction::JumpIf(address) => {
                let mut bytes = vec![Self::JUMP_IF];
                bytes.extend_from_slice(&(*address as u32).to_le_bytes());
                bytes
            }
            Instruction::Jump(address) => {
                let mut bytes = vec![Self::JUMP];
                bytes.extend_from_slice(&(*address as u32).to_le_bytes());
                bytes
            }
        }
    }

    pub fn size(&self) -> usize {
        // NOTE: if anything changes here update to Stage1Instruction.size as well
        match self {
            Self::StartAt(_) => 5,
            Self::Add
            | Self::Sub
            | Self::Eq
            | Self::Or
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
            Self::JumpIf(_) | Self::Jump(_) => 5,
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
            Instruction::Jump(19),
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
        test_size_vs_bytes_len(Instruction::Jump(19));
        test_size_vs_bytes_len(Instruction::Push(Value::String("else\n".to_string())));
        test_size_vs_bytes_len(Instruction::Call(Callee::Builtin("print".to_string()), 1));
        test_size_vs_bytes_len(Instruction::Halt);
    }
}
