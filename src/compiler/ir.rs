#![allow(dead_code)]
use crate::vm::OpCode;

#[derive(Debug, Clone, PartialEq)]
pub enum Ir {
    Value(Value),
    If(If),
    Call(String, Vec<Ir>),
    Return,
}

impl Ir {
    pub fn size(&self) -> u32 {
        match self {
            Self::Value(v) => v.size(),
            Self::If(i) => i.size(),
            Self::Call(name, args) => match name.as_str() {
                "+" | "print" => 1 + args.iter().map(|a| a.size()).sum::<u32>(),
                _ => 1 + 4 + args.iter().map(|a| a.size()).sum::<u32>(),
            },
            Self::Return => 1,
        }
    }
    pub fn to_bytecode(&self, lookup_table: &std::collections::HashMap<String, u32>) -> Vec<u8> {
        match self {
            Self::Value(v) => v.to_bytecode(lookup_table),
            Self::If(i) => i.to_bytecode(lookup_table),
            Self::Call(name, args) => match name.as_str() {
                "print" => {
                    let mut bytes = args
                        .iter()
                        .map(|a| a.to_bytecode(lookup_table))
                        .flatten()
                        .collect::<Vec<_>>();
                    bytes.push(OpCode::Print as u8);
                    bytes
                }
                "+" => {
                    let mut bytes = args
                        .iter()
                        .map(|a| a.to_bytecode(lookup_table))
                        .flatten()
                        .collect::<Vec<_>>();
                    bytes.push(OpCode::Print as u8);
                    bytes
                }
                _ => {
                    let mut bytes = args
                        .iter()
                        .map(|a| a.to_bytecode(lookup_table))
                        .flatten()
                        .collect::<Vec<_>>();
                    let id = lookup_table.get(name).unwrap();
                    bytes.push(OpCode::Call as u8);
                    bytes.extend(id.to_le_bytes());
                    bytes
                }
            },
            Self::Return => vec![OpCode::Return as u8],
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
            Self::Id(_) => 1 + 4, // 1 for opcode + 4 for index,
            Self::U8(_) => 1,
            Self::U32(_) => 4,
            Self::F64(_) => 8,
            Self::String(v) => 4 + v.len() as u32, // 4 bytes for length and then the string
            Self::Bool(_) => 1 + 1,                // 1 for opcode + 1 for value
        }
    }
    pub fn to_bytecode(&self, lookup_table: &std::collections::HashMap<String, u32>) -> Vec<u8> {
        match self {
            Self::Id(id) => {
                let Some(id) = lookup_table.get(id) else {
                    panic!("unknown id: {id}");
                };
                let mut bytes = vec![OpCode::LocalVar as u8];
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
            Self::Bool(value) => vec![OpCode::PushU8 as u8, *value as u8],
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
            size += 1; // for opcode
            size += 4; // for id
        }
        for instruction in self.instruction.iter() {
            size += instruction.size();
        }
        size
    }

    pub fn to_bytecode(
        &self,
        _global_lookup_table: &std::collections::HashMap<String, u32>,
    ) -> Vec<u8> {
        let mut bytes = vec![];
        let mut lookup_table = std::collections::HashMap::new();

        for (i, param) in self.params.iter().enumerate() {
            let id = i as u32;
            lookup_table.insert(param.clone(), id);
            bytes.push(OpCode::LocalVar as u8);
            bytes.extend(id.to_le_bytes());
        }

        let mut table = _global_lookup_table.clone();
        for (key, value) in lookup_table.iter() {
            table.insert(key.clone(), *value);
        }
        for instruction in self.instruction.iter() {
            bytes.extend(instruction.to_bytecode(&table));
        }

        bytes
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Value,
    pub if_block: Vec<Ir>,
    pub else_block: Vec<Ir>,
}

impl If {
    pub fn size(&self) -> u32 {
        todo!()
    }

    pub fn to_bytecode(&self, lookup_table: &std::collections::HashMap<String, u32>) -> Vec<u8> {
        todo!()
    }
}
