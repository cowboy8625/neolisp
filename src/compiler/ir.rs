#![allow(dead_code)]
use crate::vm::OpCode;

pub type LookupTable = std::collections::HashMap<String, Scope>;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Scope {
    Global(u32),
    Local(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ir {
    Value(Value),
    If(If),
    Call(String, Vec<Ir>),
    Return,
    Halt,
    LoadGlobalVar(String),
    BuiltIn(String, Vec<Ir>),
    LoadTest(String, u32),
}

impl Ir {
    pub fn size(&self) -> u32 {
        match self {
            Self::Value(v) => v.size(),
            Self::If(i) => i.size(),
            Self::Call(name, args) => 1 + 4 + args.iter().map(|a| a.size()).sum::<u32>(),
            Self::Return => 2,
            Self::Halt => 1,
            Self::LoadGlobalVar(_) => 1,
            Self::BuiltIn(name, args) => match name.as_str() {
                "=" | "+" | "print" => {
                    let mut count = args.iter().map(|a| a.size()).sum::<u32>(); // args size
                    count += 1; // opcode
                    count += 4; // args count
                    count
                }
                _ => {
                    let mut count = 1; // opcode
                    count += 4; // args count
                    count += 4; // name length
                    count += name.len() as u32; // name
                    count += args.iter().map(|a| a.size()).sum::<u32>(); // args size
                    count
                }
            },
            Self::LoadTest(name, _) => {
                let mut count = 1; // opcode
                count += 4; // name length
                count += name.len() as u32; // name
                count += 4; // index
                count
            }
        }
    }
    pub fn to_bytecode(&self, lookup_table: &LookupTable) -> Vec<u8> {
        match self {
            Self::Value(v) => v.to_bytecode(lookup_table),
            Self::If(i) => i.to_bytecode(lookup_table),
            Self::Call(name, args) => match name.as_str() {
                _ => {
                    let mut bytes = args
                        .iter()
                        .map(|a| a.to_bytecode(lookup_table))
                        .flatten()
                        .collect::<Vec<_>>();
                    let id = match lookup_table.get(name) {
                        Some(Scope::Global(id)) => *id,
                        Some(Scope::Local(_)) => panic!("cannot call function from local scope"),
                        None => panic!("function `{name}` not found"),
                    };
                    bytes.push(OpCode::Call as u8);
                    bytes.extend(id.to_le_bytes());
                    bytes
                }
            },
            Self::Return => vec![OpCode::Rot as u8, OpCode::Return as u8],
            Self::Halt => vec![OpCode::Halt as u8],
            Self::LoadGlobalVar(_) => vec![OpCode::LoadGlobalVar as u8],
            Self::BuiltIn(name, args) => match name.as_str() {
                "=" | "+" | "print" | "list" => {
                    let mut bytes = args
                        .iter()
                        .map(|a| a.to_bytecode(lookup_table))
                        .flatten()
                        .collect::<Vec<_>>();
                    let op = match name.as_str() {
                        "=" => OpCode::Eq,
                        "+" => OpCode::AddF64,
                        "print" => OpCode::Print,
                        "list" => OpCode::CreateList,
                        _ => unreachable!(),
                    };
                    bytes.push(op as u8);
                    bytes.extend((args.len() as u32).to_le_bytes());
                    bytes
                }
                _ => {
                    let mut bytes = args
                        .iter()
                        .map(|a| a.to_bytecode(lookup_table))
                        .flatten()
                        .collect::<Vec<_>>();
                    let length = name.len() as u32;
                    bytes.push(OpCode::BuiltIn as u8);
                    // Args count
                    bytes.extend((args.len() as u32).to_le_bytes());
                    // name length
                    bytes.extend(length.to_le_bytes());
                    // name
                    bytes.extend(name.as_bytes());
                    bytes
                }
            },
            Self::LoadTest(name, index) => {
                let mut bytes = vec![OpCode::LoadTest as u8];
                let length = name.len() as u32;
                bytes.extend(length.to_le_bytes());
                bytes.extend(name.as_bytes());
                bytes.extend(index.to_le_bytes());
                bytes
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
    pub fn to_bytecode(&self, lookup_table: &LookupTable) -> Vec<u8> {
        match self {
            Self::Id(name) => {
                let Some(scope) = lookup_table.get(name) else {
                    panic!("unknown id: {name}");
                };
                let mut bytes = match scope {
                    Scope::Global(_) => vec![OpCode::GetGlobalVar as u8],
                    Scope::Local(_) => vec![OpCode::GetLocalVar as u8],
                };
                let id = match scope {
                    Scope::Global(id) => *id,
                    Scope::Local(id) => *id,
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
            size += 1; // for opcode Rot
            size += 1; // for opcode LoadLocalVar
            size += 4; // for id
        }
        for instruction in self.instruction.iter() {
            size += instruction.size();
        }
        size
    }

    pub fn to_bytecode(&self, global_lookup_table: &LookupTable) -> Vec<u8> {
        let mut bytes = vec![];
        let mut lookup_table = LookupTable::new();

        for (i, param) in self.params.iter().enumerate() {
            let id = i as u32;
            lookup_table.insert(param.clone(), Scope::Local(id));
            bytes.push(OpCode::Rot as u8);
            bytes.push(OpCode::LoadLocalVar as u8);
            bytes.extend(id.to_le_bytes());
        }

        let mut table = global_lookup_table.clone();
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

    pub fn to_bytecode(&self, lookup_table: &LookupTable) -> Vec<u8> {
        let mut bytes = vec![];
        for instruction in self.instruction.iter() {
            bytes.extend(instruction.to_bytecode(lookup_table));
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

    pub fn to_bytecode(&self, lookup_table: &LookupTable) -> Vec<u8> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_size() {
        assert_eq!(Value::Id("a".to_string()).size(), 1 + 4);
        assert_eq!(Value::U8(1).size(), 1);
        assert_eq!(Value::U32(1).size(), 4);
        assert_eq!(Value::F64(1.0).size(), 8);
        assert_eq!(Value::String("a".to_string()).size(), 4 + 1);
        assert_eq!(Value::Bool(true).size(), 1 + 1);
    }

    #[test]
    fn test_ir_size() {
        // assert_eq!(Ir::Value(Value));
        // assert_eq!(Ir::If(If));
        assert_eq!(Ir::Call("add".to_string(), Vec::new()).size(), 1 + 4);
        assert_eq!(Ir::Return.size(), 2);
        assert_eq!(Ir::Halt.size(), 1);
        assert_eq!(Ir::LoadGlobalVar("X".to_string()).size(), 1);

        let mut lookup_table = LookupTable::new();
        lookup_table.insert("x".to_string(), Scope::Local(0));

        let name = "add".to_string();
        let ircode = Ir::BuiltIn(name.clone(), vec![Ir::Value(Value::Id("x".to_string()))]);
        let bytes = ircode.to_bytecode(&lookup_table);

        assert_eq!(ircode.size(), bytes.len() as u32);
    }
}
