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
    LoadLocal,
    GetLocal(IState),
    LoadGlobal,
    GetGlobal(IState),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IState {
    Set(usize),
    Unset(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Callee {
    Function(IState),
    Lambda(IState),
    Builtin(String),
}
