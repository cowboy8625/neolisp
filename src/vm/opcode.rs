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
    /// Stack manipulation
    PushBool,
    PushU8,
    PushF64,
    PushString,
    PopF64,
    /// Swap the top two items
    Swap,
    /// Duplicate the top most item and push it
    Dup,
    /// Take the top most items on stack and rotate them
    Rot,

    /// Load a value on to the local variable stack
    LoadLocalVar,
    /// Get a value on to the local variable stack
    GetLocalVar,
    /// Load a value on to the global variable stack
    LoadGlobalVar,
    /// Get a value on to the global variable stack
    GetGlobalVar,

    Call,
    Return,

    /// count of args
    /// name length
    /// name
    BuiltIn,
    LoadTest,
    /// Followed by Index
    JumpIfFalse,
    JumpForward,
    LoadLambda,
    CallLambda,
}

impl TryFrom<u8> for OpCode {
    type Error = String;
    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Self::Noop),
            0x01 => Ok(Self::Halt),
            0x02 => Ok(Self::AddF64),
            0x03 => Ok(Self::Sub),
            0x04 => Ok(Self::Mul),
            0x05 => Ok(Self::Div),
            0x06 => Ok(Self::Mod),
            0x07 => Ok(Self::Eq),
            0x08 => Ok(Self::Gt),
            0x09 => Ok(Self::Lt),
            0x0A => Ok(Self::Gte),
            0x0B => Ok(Self::Lte),
            0x0C => Ok(Self::TypeOf),
            0x0D => Ok(Self::And),
            0x0E => Ok(Self::Or),
            0x0F => Ok(Self::Not),
            0x10 => Ok(Self::PushBool),
            0x11 => Ok(Self::PushU8),
            0x12 => Ok(Self::PushF64),
            0x13 => Ok(Self::PushString),
            0x14 => Ok(Self::PopF64),
            0x15 => Ok(Self::Swap),
            0x16 => Ok(Self::Dup),
            0x17 => Ok(Self::Rot),
            0x18 => Ok(Self::LoadLocalVar),
            0x19 => Ok(Self::GetLocalVar),
            0x1A => Ok(Self::LoadGlobalVar),
            0x1B => Ok(Self::GetGlobalVar),
            0x1C => Ok(Self::Call),
            0x1D => Ok(Self::Return),
            0x1E => Ok(Self::BuiltIn),
            0x1F => Ok(Self::LoadTest),
            0x20 => Ok(Self::JumpIfFalse),
            0x21 => Ok(Self::JumpForward),
            0x22 => Ok(Self::LoadLambda),
            0x23 => Ok(Self::CallLambda),
            _ => Err(format!("unknown opcode: {value}")),
        }
    }
}
