#![allow(dead_code)]
#![allow(unused)]

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Operator {
    Add,
    Eq,
}

impl From<&str> for Operator {
    fn from(s: &str) -> Self {
        match s {
            "+" => Self::Add,
            "=" => Self::Eq,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompiledHir {
    Functions(Vec<Function>),
    Lambdas(Vec<Lambda>),
    Tests(Vec<Test>),
    GlobalVar(Vec<Var>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Hir {
    Return,
    Halt,
    Operator(Operator, Vec<Hir>),
    Value(Value),
    If(If),
    BuiltIn(String, Vec<Hir>),
    Call(Option<String>, Vec<Hir>),
    LoadGlobal(String, usize),
    LoadLocal(String, usize),
    GetLocalAndLoadFree(String),
    LoadTest(String, u32),
    LoadLambda(String),
    // LoadFunction(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Id(String),
    U8(u8),
    U32(u32),
    F64(f64),
    String(String),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Vec<Hir>,
    pub then_block: Vec<Hir>,
    pub else_block: Vec<Hir>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub body: Vec<Hir>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub name: String,
    pub params: Vec<String>,
    pub instruction: Vec<Hir>,
    pub captured: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Test {
    pub name: String,
    pub instruction: Vec<Hir>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    pub name: String,
    pub instruction: Vec<Hir>,
}