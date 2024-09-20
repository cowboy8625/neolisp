use super::value::Value;

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    StartAt(usize),
    Noop,
    Halt,
    Return,
    Push(Value),
    Add,
    Rot,
    Call(Callee, u8),
    PopIntoLocalStack,
    LoadLocal(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Function(usize),
    UnSetFunctionLocation(String),
    Builtin(String),
}
