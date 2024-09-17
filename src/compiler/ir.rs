#![allow(dead_code)]
use crate::symbol_table::{Scope, SymbolKind, SymbolTable};
use crate::unwrap;
use crate::vm::OpCode;

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

impl Operator {
    pub fn size(&self) -> u32 {
        1
    }

    pub fn to_bytecode(&self) -> u8 {
        match self {
            Self::Add => OpCode::AddF64 as u8,
            Self::Eq => OpCode::Eq as u8,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ir {
    Value(Value),
    Operator(Operator, Vec<Ir>),
    If(If),
    Call(SymbolKind, String, Vec<Ir>),
    Return,
    Halt,
    LoadGlobalVar(String),
    BuiltIn(String, Vec<Ir>),
    LoadTest(String, u32),
    Lambda(Lambda),
    CallLambda(Vec<Ir>),
}

impl Ir {
    pub fn size(&self) -> u32 {
        match self {
            Self::Value(v) => v.size(),
            Self::Operator(op, args) => op.size() + 4 + args.iter().map(|a| a.size()).sum::<u32>(),
            Self::If(i) => i.size(),
            // NOTE: CallLambda is the same
            Self::Call(symbol_type, name, args) => {
                1 + 4
                    + args.iter().map(|a| a.size()).sum::<u32>()
                    + if matches!(symbol_type, SymbolKind::Lambda) {
                        5
                    } else {
                        0
                    }
            }
            Self::Return => 2,
            Self::Halt => 1,
            Self::LoadGlobalVar(_) => 1,
            Self::BuiltIn(name, args) => {
                let mut count = 1; // opcode
                count += 4; // args count
                count += 4; // name length
                count += name.len() as u32; // name
                count += args.iter().map(|a| a.size()).sum::<u32>(); // args size
                count
            }
            Self::LoadTest(name, _) => {
                let mut count = 1; // opcode
                count += 4; // name length
                count += name.len() as u32; // name
                count += 4; // index
                count
            }
            Self::Lambda(lambda) => lambda.size(),
            // NOTE: Call is the same
            Self::CallLambda(args) => 1 + 4 + args.iter().map(|a| a.size()).sum::<u32>(),
        }
    }
    pub fn to_bytecode(&self, symbol_table: &mut SymbolTable) -> Vec<u8> {
        match self {
            Self::Value(v) => v.to_bytecode(symbol_table),
            Self::Operator(op, args) => {
                let mut bytes = args
                    .iter()
                    .map(|a| a.to_bytecode(symbol_table))
                    .flatten()
                    .collect::<Vec<_>>();
                bytes.push(op.to_bytecode());
                bytes.extend((args.len() as u32).to_le_bytes());
                bytes
            }
            Self::If(i) => i.to_bytecode(symbol_table),
            Self::Call(_, name, args) => match name.as_str() {
                _ => {
                    let mut bytes = args
                        .iter()
                        .map(|a| a.to_bytecode(symbol_table))
                        .flatten()
                        .collect::<Vec<_>>();
                    let Some(symbol) = symbol_table.lookup(name) else {
                        panic!("function `{name}` not found")
                    };
                    match &symbol.kind {
                        SymbolKind::Function => {
                            let Some(id) = symbol.location else {
                                panic!("location of function call `{name}` has not been set")
                            };
                            bytes.push(OpCode::Call as u8);
                            bytes.extend(id.to_le_bytes());
                        }
                        SymbolKind::Lambda if symbol.scope == Scope::Global => {
                            bytes.push(OpCode::GetGlobalVar as u8);
                            bytes.extend(symbol.location.unwrap().to_le_bytes());
                            bytes.push(OpCode::CallLambda as u8);
                            bytes.extend((args.len() as u32).to_le_bytes());
                        }
                        SymbolKind::Lambda if symbol.scope == Scope::Function => {
                            bytes.push(OpCode::GetLocalVar as u8);
                            bytes.extend(symbol.location.unwrap().to_le_bytes());
                            bytes.push(OpCode::CallLambda as u8);
                            bytes.extend((args.len() as u32).to_le_bytes());
                        }
                        item => unreachable!("Calling {name} of kind {item:?}"),
                    };
                    bytes
                }
            },
            Self::Return => vec![OpCode::Rot as u8, OpCode::Return as u8],
            Self::Halt => vec![OpCode::Halt as u8],
            Self::LoadGlobalVar(_) => vec![OpCode::LoadGlobalVar as u8],
            Self::BuiltIn(name, args) => {
                let mut bytes = args
                    .iter()
                    .map(|a| a.to_bytecode(symbol_table))
                    .flatten()
                    .collect::<Vec<_>>();
                let length = name.len() as u32;
                bytes.push(OpCode::BuiltIn as u8);
                // Args count
                let count = args
                    .iter()
                    .filter(|a| !matches!(a, Self::Lambda(_)))
                    .count() as u32;
                bytes.extend(count.to_le_bytes());
                // name length
                bytes.extend(length.to_le_bytes());
                // name
                bytes.extend(name.as_bytes());
                bytes
            }
            Self::LoadTest(name, index) => {
                let mut bytes = vec![OpCode::LoadTest as u8];
                let length = name.len() as u32;
                bytes.extend(length.to_le_bytes());
                bytes.extend(name.as_bytes());
                bytes.extend(index.to_le_bytes());
                bytes
            }
            Self::Lambda(lambda) => lambda.to_bytecode(symbol_table),
            Self::CallLambda(args) => {
                unreachable!("Lambda Should have been created");
                // let mut bytes = args
                //     .iter()
                //     .map(|a| a.to_bytecode(lookup_table))
                //     .flatten()
                //     .collect::<Vec<_>>();
                // bytes.push(OpCode::CallLambda as u8);
                // bytes.extend((args.len() as u32).to_le_bytes());
                // bytes
            }
        }
    }
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

impl Value {
    /// Return the size of the value in bytes
    pub fn size(&self) -> u32 {
        match self {
            Self::Id(..) => 1 + 4, // 1 for opcode + 4 for index,
            Self::U8(_) => 1,
            Self::U32(_) => 4,
            Self::F64(_) => 1 + 8, // 1 for opcode + 8 for value
            Self::String(v) => 1 + 4 + v.len() as u32, // 4 bytes for length and then the string
            Self::Bool(_) => 1 + 1, // 1 for opcode + 1 for value
        }
    }
    pub fn to_bytecode(&self, symbol_table: &mut SymbolTable) -> Vec<u8> {
        match self {
            Self::Id(name) => {
                let Some(symbol) = symbol_table.lookup(name) else {
                    panic!("unknown symbol name: {name}");
                };
                let mut bytes = match symbol.scope {
                    Scope::Global => vec![OpCode::GetGlobalVar as u8],
                    Scope::Function => {
                        vec![OpCode::GetLocalVar as u8]
                    }
                    _ => unreachable!(),
                };
                let Some(id) = symbol.location else {
                    eprintln!("{:#?}", symbol_table);
                    panic!("location of {:?} `{name}` has not been set", symbol.kind);
                };
                bytes.extend(id.to_le_bytes());
                bytes
            }
            Self::U8(value) => todo!(),
            Self::U32(value) => todo!(),
            Self::F64(value) => {
                let mut bytes = vec![OpCode::PushF64 as u8];
                bytes.extend(value.to_le_bytes());
                bytes
            }
            Self::String(value) => {
                let mut bytes = vec![OpCode::PushString as u8];
                bytes.extend((value.len() as u32).to_le_bytes());
                bytes.extend(value.as_bytes());
                bytes
            }
            Self::Bool(value) => vec![OpCode::PushBool as u8, *value as u8],
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<String>,
    pub instruction: Vec<Ir>,
}

impl Function {
    pub fn size(&self) -> u32 {
        let mut size = 0;

        for param in self.params.iter() {
            size += 1; // for opcode Rot
            size += 1; // for opcode LoadLocalVar
            size += 4; // for id
        }
        for instruction in self.instruction.iter() {
            size += instruction.size();
        }
        size
    }

    pub fn to_bytecode(&self, symbol_table: &mut SymbolTable) -> Vec<u8> {
        let mut bytes = vec![];

        for (i, param) in self.params.iter().enumerate() {
            let id = i as u32;
            symbol_table.set_location(Some(&self.name), param, id);
            bytes.push(OpCode::Rot as u8);
            bytes.push(OpCode::LoadLocalVar as u8);
            bytes.extend(id.to_le_bytes());
        }

        symbol_table.enter_scope(&self.name);
        for instruction in self.instruction.iter() {
            bytes.extend(instruction.to_bytecode(symbol_table));
        }
        symbol_table.exit_scope();

        bytes
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Test {
    pub name: String,
    pub instruction: Vec<Ir>,
}

impl Test {
    pub fn call_size(&self) -> u32 {
        Ir::LoadTest(self.name.clone(), 0).size()
    }

    pub fn size(&self) -> u32 {
        let mut size = 0;
        for instruction in self.instruction.iter() {
            size += instruction.size();
        }
        size
    }

    pub fn to_bytecode(&self, lookup_table: &mut SymbolTable) -> Vec<u8> {
        let mut bytes = vec![];
        for instruction in self.instruction.iter() {
            bytes.extend(instruction.to_bytecode(lookup_table));
        }
        bytes
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub params: Vec<String>,
    pub body: Vec<Ir>,
    /// ID is to scope the variables in the lambda from the global scope
    pub id: u32,
}

impl Lambda {
    pub fn size(&self) -> u32 {
        let mut size = 1; // for opcode LoadLambda
        size += 4; // 4 bytes for the bytes count

        let params_length = self.params.len() as u32;

        let mut opcodes = 1; // OpCode::Rot
        opcodes += 1; // OpCode::LoadLocalVar
        opcodes += 4; // index for LoadLocalVar

        size += params_length * opcodes;

        let body_size = self.body.iter().fold(0, |acc, ir| acc + ir.size());

        size += body_size;

        size
    }

    pub fn to_bytecode(&self, symbol_table: &mut SymbolTable) -> Vec<u8> {
        let mut bytes = vec![];

        let name = format!("lambda_{}", self.id);

        bytes.push(OpCode::LoadLambda as u8);
        let size = (self.size() - 1 - 4).to_le_bytes();
        bytes.extend(size);

        for (i, param) in self.params.iter().enumerate() {
            let id = i as u32;

            symbol_table.set_location(Some(&name), param, id);
            bytes.push(OpCode::Rot as u8);
            bytes.push(OpCode::LoadLocalVar as u8);
            bytes.extend(id.to_le_bytes());
        }

        symbol_table.enter_scope(&name);

        for instruction in self.body.iter() {
            bytes.extend(instruction.to_bytecode(symbol_table));
        }

        symbol_table.exit_scope();

        bytes
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Vec<Ir>,
    pub then_block: Vec<Ir>,
    pub else_block: Vec<Ir>,
}

impl If {
    pub fn size_of_condition(&self) -> u32 {
        self.condition.iter().fold(0, |acc, ir| acc + ir.size())
    }

    pub fn size_of_then_block(&self) -> u32 {
        self.then_block.iter().fold(0, |acc, ir| acc + ir.size())
        // for OpCode::Jump
        + 1
        // for offset
        + 4
    }

    pub fn size_of_else_block(&self) -> u32 {
        self.else_block.iter().fold(0, |acc, ir| acc + ir.size())
    }

    pub fn size(&self) -> u32 {
        let mut size =
            self.size_of_condition() + self.size_of_then_block() + self.size_of_else_block();
        size += 1; // for OpCode::JumpIfFalse
        size += 4; // for offset
        size
    }

    pub fn to_bytecode(&self, symbol_table: &mut SymbolTable) -> Vec<u8> {
        let mut bytes = vec![];
        for instruction in self.condition.iter() {
            bytes.extend(instruction.to_bytecode(symbol_table));
        }

        bytes.push(OpCode::JumpIfFalse as u8);
        let offset = self.size_of_then_block();
        bytes.extend(offset.to_le_bytes());

        for instruction in self.then_block.iter() {
            bytes.extend(instruction.to_bytecode(symbol_table));
        }

        bytes.push(OpCode::JumpForward as u8);
        let offset = self.size_of_else_block();
        bytes.extend(offset.to_le_bytes());

        for instruction in self.else_block.iter() {
            bytes.extend(instruction.to_bytecode(symbol_table));
        }
        bytes
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn test_value_if() {
    //     let value = Ir::If(If {
    //         condition: vec![Ir::BuiltIn(
    //             "=".to_string(),
    //             vec![Ir::Value(Value::F64(123.0)), Ir::Value(Value::F64(321.0))],
    //         )],
    //         then_block: vec![Ir::BuiltIn(
    //             "print".to_string(),
    //             vec![Ir::Value(Value::String("true\n".to_string()))],
    //         )],
    //         else_block: vec![Ir::BuiltIn(
    //             "print".to_string(),
    //             vec![Ir::Value(Value::String("false\n".to_string()))],
    //         )],
    //     });
    //     assert_eq!(value.size(), 1);
    // }
    //
    // #[test]
    // fn test_value_size() {
    //     assert_eq!(Value::Id("a".to_string()).size(), 1 + 4);
    //     assert_eq!(Value::U8(1).size(), 1);
    //     assert_eq!(Value::U32(1).size(), 4);
    //     assert_eq!(Value::F64(1.0).size(), 9);
    //     assert_eq!(Value::String("a".to_string()).size(), 1 + 4 + 1);
    //     assert_eq!(Value::Bool(true).size(), 1 + 1);
    // }
    //
    // #[test]
    // fn test_ir_size() {
    //     // assert_eq!(Ir::Value(Value));
    //     // assert_eq!(Ir::If(If));
    //     assert_eq!(Ir::Call("add".to_string(), Vec::new()).size(), 1 + 4);
    //     assert_eq!(Ir::Return.size(), 2);
    //     assert_eq!(Ir::Halt.size(), 1);
    //     assert_eq!(Ir::LoadGlobalVar("X".to_string()).size(), 1);
    //
    //     let mut lookup_table = LookupTable::new();
    //     lookup_table.insert("x".to_string(), Scope::Local(0));
    //
    //     let name = "add".to_string();
    //     let ircode = Ir::BuiltIn(name.clone(), vec![Ir::Value(Value::Id("x".to_string()))]);
    //     let bytes = ircode.to_bytecode(&lookup_table);
    //
    //     assert_eq!(ircode.size(), bytes.len() as u32);
    // }
}
