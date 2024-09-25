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

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Function,
    Builtin(String),
}
