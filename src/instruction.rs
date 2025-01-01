use super::ast::Span;
use crate::symbol_table::Type;
use num_derive::{FromPrimitive, ToPrimitive};

#[derive(Debug, Clone, PartialEq, FromPrimitive, ToPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Halt,
    Return,
    ReturnFromTest,
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
    CallTest,
    TailCall,
    SetLocal,
    SetGlobal,
    SetFree,
    GetLocal,
    GetGlobal,
    GetFree,
    JumpIf,
    JumpForward,
    JumpBackward,
    Jump,
    LoadLibrary,
    CallFfi,
    StructInit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Halt,
    Return,
    ReturnFromTest,
    Push(Box<Value>),
    Add(usize),
    Sub(usize),
    Mul(usize),
    Div(usize),
    Eq(usize),
    GreaterThan(usize),
    LessThan(usize),
    GreaterThanOrEqual(usize),
    LessThanOrEqual(usize),
    And(usize),
    Or(usize),
    Not,
    Mod,
    Rot,
    Call(usize),
    CallTest,
    TailCall(usize),
    SetLocal(RuntimeMetadata),
    SetGlobal(RuntimeMetadata),
    SetFree(RuntimeMetadata),
    GetLocal(RuntimeMetadata),
    GetGlobal(RuntimeMetadata),
    GetFree(RuntimeMetadata),
    JumpIf(usize),
    JumpForward(usize),
    JumpBackward(usize),
    Jump(usize),
    LoadLibrary(LoadLibrary),
    CallFfi(CallFfi),
    StructInit(RuntimeMetadata),
}

impl Instruction {
    pub fn size(&self) -> usize {
        match self {
            Self::Halt => 1,
            Self::Return => 1,
            Self::ReturnFromTest => 1,
            Self::Push(value) => 1 + value.size(),
            Self::Add(_) => 2,
            Self::Sub(_) => 2,
            Self::Mul(_) => 2,
            Self::Div(_) => 2,
            Self::Eq(_) => 2,
            Self::GreaterThan(_) => 2,
            Self::LessThan(_) => 2,
            Self::GreaterThanOrEqual(_) => 2,
            Self::LessThanOrEqual(_) => 2,
            Self::And(_) => 2,
            Self::Or(_) => 2,
            Self::Not => 1,
            Self::Mod => 1,
            Self::Rot => 1,
            Self::Call(_) => 2,
            Self::CallTest => 1,
            Self::TailCall(_) => 2,
            Self::SetLocal(md) => 1 + md.size(),
            Self::SetGlobal(md) => 1 + md.size(),
            Self::SetFree(md) => 1 + md.size(),
            Self::GetLocal(md) => 1 + md.size(),
            Self::GetGlobal(md) => 1 + md.size(),
            Self::GetFree(md) => 1 + md.size(),
            Self::JumpIf(_) => 5,
            Self::JumpForward(_) => 5,
            Self::JumpBackward(_) => 5,
            Self::Jump(_) => 5,
            Self::LoadLibrary(lib) => 1 + lib.size(),
            Self::CallFfi(call) => 1 + call.size(),
            Self::StructInit(md) => 1 + md.size(),
        }
    }

    pub fn opcode(&self) -> OpCode {
        match self {
            Self::Halt => OpCode::Halt,
            Self::Return => OpCode::Return,
            Self::ReturnFromTest => OpCode::ReturnFromTest,
            Self::Push(..) => OpCode::Push,
            Self::Add(..) => OpCode::Add,
            Self::Sub(..) => OpCode::Sub,
            Self::Mul(..) => OpCode::Mul,
            Self::Div(..) => OpCode::Div,
            Self::Eq(..) => OpCode::Eq,
            Self::GreaterThan(..) => OpCode::GreaterThan,
            Self::LessThan(..) => OpCode::LessThan,
            Self::GreaterThanOrEqual(_) => OpCode::GreaterThanOrEqual,
            Self::LessThanOrEqual(_) => OpCode::LessThanOrEqual,
            Self::And(..) => OpCode::And,
            Self::Or(..) => OpCode::Or,
            Self::Not => OpCode::Not,
            Self::Mod => OpCode::Mod,
            Self::Rot => OpCode::Rot,
            Self::Call(..) => OpCode::Call,
            Self::CallTest => OpCode::CallTest,
            Self::TailCall(..) => OpCode::TailCall,
            Self::SetLocal(..) => OpCode::SetLocal,
            Self::SetGlobal(..) => OpCode::SetGlobal,
            Self::SetFree(..) => OpCode::SetFree,
            Self::GetLocal(..) => OpCode::GetLocal,
            Self::GetGlobal(..) => OpCode::GetGlobal,
            Self::GetFree(..) => OpCode::GetFree,
            Self::JumpIf(..) => OpCode::JumpIf,
            Self::JumpForward(..) => OpCode::JumpForward,
            Self::JumpBackward(..) => OpCode::JumpBackward,
            Self::Jump(..) => OpCode::Jump,
            Self::LoadLibrary(..) => OpCode::LoadLibrary,
            Self::CallFfi(..) => OpCode::CallFfi,
            Self::StructInit(..) => OpCode::StructInit,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        match self {
            Self::Halt => bytes.push(OpCode::Halt as u8),
            Self::Return => bytes.push(OpCode::Return as u8),
            Self::ReturnFromTest => bytes.push(OpCode::ReturnFromTest as u8),
            Self::Push(value) => {
                bytes.push(OpCode::Push as u8);
                bytes.extend(&value.to_bytecode());
            }
            Self::Add(count)
            | Self::Sub(count)
            | Self::Mul(count)
            | Self::Div(count)
            | Self::Eq(count)
            | Self::GreaterThan(count)
            | Self::LessThan(count)
            | Self::GreaterThanOrEqual(count)
            | Self::LessThanOrEqual(count)
            | Self::And(count)
            | Self::Or(count) => {
                bytes.push(self.opcode() as u8);
                bytes.push(*count as u8);
            }
            Self::Not | Self::Mod | Self::Rot | Self::CallTest => bytes.push(self.opcode() as u8),
            Self::Call(count) | Self::TailCall(count) => {
                bytes.push(self.opcode() as u8);
                bytes.push(*count as u8);
            }
            Self::GetLocal(metadata)
            | Self::GetGlobal(metadata)
            | Self::GetFree(metadata)
            | Self::SetLocal(metadata)
            | Self::SetGlobal(metadata)
            | Self::SetFree(metadata) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&metadata.to_bytecode());
            }
            Self::JumpIf(address)
            | Self::JumpForward(address)
            | Self::JumpBackward(address)
            | Self::Jump(address) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&(*address as u32).to_le_bytes());
            }
            Self::LoadLibrary(lib) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&lib.to_bytecode());
            }
            Self::CallFfi(call) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&call.to_bytecode());
            }
            Self::StructInit(md) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&md.to_bytecode());
            }
        }

        bytes
    }

    pub fn debugger_display(&self) -> String {
        if let Self::Push(value) = self {
            return format!("{:<10} {}", "push", value.debugger_display());
        }
        format!("{self}")
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Halt => write!(f, "halt"),
            Self::Return => write!(f, "ret"),
            Self::ReturnFromTest => write!(f, "ret-test"),
            Self::Push(value) => write!(f, "{:<10} {value}", "push"),
            Self::Add(count) => write!(f, "{:<10} {count}", "add"),
            Self::Sub(count) => write!(f, "{:<10} {count}", "sub"),
            Self::Mul(count) => write!(f, "{:<10} {count}", "mul"),
            Self::Div(count) => write!(f, "{:<10} {count}", "div"),
            Self::Eq(count) => write!(f, "eq {count}"),
            Self::GreaterThan(count) => write!(f, "{:<10} {count}", "gt"),
            Self::LessThan(count) => write!(f, "{:<10} {count}", "lt"),
            Self::GreaterThanOrEqual(count) => write!(f, "{:<10} {count}", "gte"),
            Self::LessThanOrEqual(count) => write!(f, "{:<10} {count}", "lte"),
            Self::And(count) => write!(f, "{:<10} {count}", "and"),
            Self::Or(count) => write!(f, "{:<10} {count}", "or"),
            Self::Not => write!(f, "not"),
            Self::Mod => write!(f, "mod"),
            Self::Rot => write!(f, "rot"),
            Self::Call(count) => write!(f, "{:<10} {count}", "call"),
            Self::CallTest => write!(f, "call-test"),
            Self::TailCall(count) => write!(f, "{:<10} {}", "tail-call", count),
            Self::SetLocal(metadata) => write!(f, "{:<10} {metadata}", "set-local"),
            Self::SetGlobal(metadata) => write!(f, "{:<10} {metadata}", "set-global"),
            Self::SetFree(metadata) => write!(f, "{:<10} {metadata}", "set-free"),
            Self::GetLocal(metadata) => write!(f, "{:<10} {metadata}", "get-local"),
            Self::GetGlobal(metadata) => write!(f, "{:<10} {metadata}", "get-global"),
            Self::GetFree(metadata) => write!(f, "{:<10} {metadata}", "get-free"),
            Self::JumpIf(address) => write!(f, "{:<10} {address}", "jump-if"),
            Self::JumpForward(address) => write!(f, "{:<10} {address}", "jump-forward"),
            Self::JumpBackward(address) => write!(f, "{:<10} {address}", "jump-backward"),
            Self::Jump(address) => write!(f, "{:<10} {address}", "jump"),
            Self::LoadLibrary(lib) => write!(f, "{:<10} {lib}", "load-library"),
            Self::CallFfi(call) => write!(f, "{:<10} {call}", "call-ffi"),
            Self::StructInit(md) => write!(f, "{:<10} {md}", "struct-init"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RuntimeMetadata {
    pub data: usize,
    pub name: Box<String>,
    pub span: Span,
}

impl From<&crate::symbol_table::Struct> for RuntimeMetadata {
    fn from(value: &crate::symbol_table::Struct) -> Self {
        Self {
            data: value.id,
            name: Box::new(value.name.to_string()),
            span: value.span.clone(),
        }
    }
}

impl RuntimeMetadata {
    pub fn new(data: usize, name: impl Into<String>, span: Span) -> Self {
        Self {
            data,
            name: Box::new(name.into()),
            span,
        }
    }

    pub fn size(&self) -> usize {
        // 4 bytes for address
        4 +
        // 1 bytes for name length
        1 +
        // name
        self.name.len() +
        // 4 start span
        4 +
        // 4 end span
        4
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(&(self.data as u32).to_le_bytes());
        if self.name.len() > 255 {
            panic!("Name too long");
        }
        bytes.extend(&(self.name.len() as u8).to_le_bytes());
        bytes.extend(self.name.as_bytes());
        let start = self.span.start as u32;
        bytes.extend(&start.to_le_bytes());
        let end = self.span.end as u32;
        bytes.extend(&end.to_le_bytes());
        bytes
    }
}

impl std::fmt::Display for RuntimeMetadata {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?} {:?}", self.data, self.name, self.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Callable {
    pub address: usize,
    pub name: String,
    pub span: Span,
}

impl Callable {
    pub fn new(address: usize, name: impl Into<String>, span: Span) -> Self {
        Self {
            address,
            name: name.into(),
            span,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(&(self.address as u32).to_le_bytes());
        bytes.extend(&(self.name.len() as u32).to_le_bytes());
        bytes.extend(self.name.as_bytes());
        let start = self.span.start as u32;
        bytes.extend(&start.to_le_bytes());
        let end = self.span.end as u32;
        bytes.extend(&end.to_le_bytes());
        debug_assert_eq!(bytes.len(), self.size());
        bytes
    }

    pub fn size(&self) -> usize {
        // 4 address
        4 +
        // 4 name length
        4 +
        // name length
        self.name.len() +
        // 4 start span
        4 +
        // 4 end span
        4
    }
}

impl std::fmt::Display for Callable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{:?}", self.address, self.name, self.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoadLibrary {
    pub name: String,
    pub span: Span,
}

impl LoadLibrary {
    pub fn new(name: impl Into<String>, span: Span) -> Self {
        Self {
            name: name.into(),
            span,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(&(self.name.len() as u32).to_le_bytes());
        bytes.extend(self.name.as_bytes());
        let start = self.span.start as u32;
        bytes.extend(&start.to_le_bytes());
        let end = self.span.end as u32;
        bytes.extend(&end.to_le_bytes());
        debug_assert_eq!(bytes.len(), self.size());
        bytes
    }

    pub fn size(&self) -> usize {
        // 4 name length
        4 +
        // name length
        self.name.len() +
        // 4 start span
        4 +
        // 4 end span
        4
    }
}

impl std::fmt::Display for LoadLibrary {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{:?}", self.name, self.span)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallFfi {
    pub lib: String,
    pub name: String,
    pub args: Vec<Type>,
    pub ret: Type,
    pub span: Span,
}

impl CallFfi {
    pub fn new(
        lib: impl Into<String>,
        name: impl Into<String>,
        args: Vec<Type>,
        ret: Type,
        span: Span,
    ) -> Self {
        Self {
            lib: lib.into(),
            name: name.into(),
            args,
            ret,
            span,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        // 4 lib length
        bytes.extend(&(self.lib.len() as u32).to_le_bytes());
        // lib
        bytes.extend(self.lib.as_bytes());
        // 4 name length
        bytes.extend(&(self.name.len() as u32).to_le_bytes());
        // name
        bytes.extend(self.name.as_bytes());
        // 1 args len
        let args_len = self.args.len();
        if args_len > 255 {
            panic!("Args too long");
        }
        bytes.push(args_len as u8);
        // args
        for arg in &self.args {
            bytes.push(*arg as u8);
        }
        // ret
        bytes.push(self.ret as u8);
        // 4 start span
        let start = self.span.start as u32;
        bytes.extend(&start.to_le_bytes());
        // 4 end span
        let end = self.span.end as u32;
        bytes.extend(&end.to_le_bytes());
        debug_assert_eq!(bytes.len(), self.size());
        bytes
    }

    pub fn size(&self) -> usize {
        // 4 lib length
        4 +
        // lib length
        self.lib.len() +
        // 4 name length
        4 +
        // name length
        self.name.len() +
        // 1 length of args u8
        1 +
        // 1 arg type
        self.args.len() +
        // 1 ret
        1 +
        // 4 start span
        4 +
        // 4 end span
        4
    }

    pub(crate) fn get_arg_types(&self) -> Vec<libffi::middle::Type> {
        self.args
            .iter()
            .map(|t| match t {
                Type::Nil => libffi::middle::Type::void(),
                Type::Bool => libffi::middle::Type::i32(),
                Type::Int => libffi::middle::Type::i32(),
                Type::String => libffi::middle::Type::pointer(),
            })
            .collect()
    }

    pub(crate) fn get_ret_type(&self) -> libffi::middle::Type {
        match self.ret {
            Type::Nil => libffi::middle::Type::void(),
            Type::Bool => libffi::middle::Type::i32(),
            Type::Int => libffi::middle::Type::i32(),
            Type::String => libffi::middle::Type::pointer(),
        }
    }
}

impl std::fmt::Display for CallFfi {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}(", self.lib, self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i == self.args.len() - 1 {
                write!(f, "{}", arg)?;
                continue;
            }
            write!(f, "{}, ", arg)?;
        }
        write!(f, ") -> {}:{:?}", self.ret, self.span)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, FromPrimitive, ToPrimitive)]
#[repr(u8)]
pub enum ValueKind {
    Nil,
    U8,
    I32,
    U32,
    F32,
    F64,
    String,
    Bool,
    List,
    Callable,
    Builtin,
    Symbol,
    Keyword,
    Struct,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Nil,
    U8(u8),
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
    String(Box<String>),
    Bool(bool),
    List(Box<Vec<Value>>),
    Callable(Box<Callable>),
    Builtin(Box<Callable>),
    Symbol(Box<String>),
    Keyword(Box<String>),
    Struct(Box<Struct>),
}

impl Value {
    pub fn kind(&self) -> ValueKind {
        match self {
            Self::Nil => ValueKind::Nil,
            Self::U8(_) => ValueKind::U8,
            Self::I32(_) => ValueKind::I32,
            Self::U32(_) => ValueKind::U32,
            Self::F32(_) => ValueKind::F32,
            Self::F64(_) => ValueKind::F64,
            Self::String(_) => ValueKind::String,
            Self::Bool(_) => ValueKind::Bool,
            Self::List(_) => ValueKind::List,
            Self::Callable(_) => ValueKind::Callable,
            Self::Builtin(_) => ValueKind::Builtin,
            Self::Symbol(_) => ValueKind::Symbol,
            Self::Keyword(_) => ValueKind::Keyword,
            Self::Struct(_) => ValueKind::Struct,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Value::Nil => vec![ValueKind::Nil as u8],
            Value::U8(v) => vec![ValueKind::U8 as u8, *v],
            Value::I32(v) => {
                let mut bytes = vec![ValueKind::I32 as u8];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::U32(v) => {
                let mut bytes = vec![ValueKind::U32 as u8];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F32(v) => {
                let mut bytes = vec![ValueKind::F32 as u8];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F64(v) => {
                let mut bytes = vec![ValueKind::F64 as u8];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::String(v) => {
                let mut bytes = vec![ValueKind::String as u8];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(v.as_bytes());
                bytes
            }
            Value::Bool(v) => vec![ValueKind::Bool as u8, if *v { 1 } else { 0 }],
            Value::List(vec) => {
                let mut bytes = vec![ValueKind::List as u8];
                bytes.extend_from_slice(&(vec.len() as u32).to_le_bytes());
                for v in vec.iter() {
                    bytes.extend(v.to_bytecode());
                }
                bytes
            }
            Value::Callable(callable) => {
                let mut bytes = vec![ValueKind::Callable as u8];
                let callable_bytes = callable.to_bytecode();
                bytes.extend(callable_bytes);
                bytes
            }
            Value::Builtin(callable) => {
                let mut bytes = vec![ValueKind::Builtin as u8];
                let callable_bytes = callable.to_bytecode();
                bytes.extend(callable_bytes);
                bytes
            }
            Value::Symbol(v) => {
                let mut bytes = vec![ValueKind::Symbol as u8];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(v.as_bytes());
                bytes
            }
            Value::Keyword(v) => {
                let mut bytes = vec![ValueKind::Keyword as u8];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(v.as_bytes());
                bytes
            }
            Value::Struct(v) => {
                let mut bytes = vec![ValueKind::Struct as u8];
                let struct_bytes = v.to_bytecode();
                bytes.extend(struct_bytes);
                bytes
            }
        }
    }

    pub fn size(&self) -> usize {
        let conent_size = match self {
            Value::Nil => 0,
            Value::U8(_) => 1,
            Value::I32(_) => 4,
            Value::U32(_) => 4,
            Value::F32(_) => 4,
            Value::F64(_) => 8,
            Value::String(v) => 4 + v.len(),
            Value::Bool(_) => 1,
            Value::List(vec) => 4 + vec.iter().map(|v| v.size()).sum::<usize>(),
            // 4 bytes    4 bytes
            // start......end
            Value::Callable(callable) => callable.size(),
            Value::Builtin(callable) => callable.size(),
            Value::Symbol(v) => 4 + v.len(),
            Value::Keyword(v) => 4 + v.len(),
            Value::Struct(v) => v.size(),
        };
        // opcode + content
        1 + conent_size
    }

    pub fn type_of(&self) -> String {
        match self {
            Self::Nil => "nil".to_string(),
            Self::U8(_) => "u8".to_string(),
            Self::I32(_) => "i32".to_string(),
            Self::U32(_) => "u32".to_string(),
            Self::F32(_) => "f32".to_string(),
            Self::F64(_) => "f64".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Bool(_) => "Bool".to_string(),
            Self::List(_) => "List".to_string(),
            Self::Callable(_) => "Function".to_string(),
            Self::Builtin(_) => "Builtin".to_string(),
            Self::Symbol(_) => "Symbol".to_string(),
            Self::Keyword(_) => "Keyword".to_string(),
            Self::Struct(data) => data.name.clone(),
        }
    }

    pub fn debugger_display(&self) -> String {
        if let Self::String(value) = self {
            return format!("{:?}", value);
        }
        format!("{}", self)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => write!(f, "nil"),
            Self::U8(value) => write!(f, "{value}"),
            Self::I32(value) => write!(f, "{value}"),
            Self::U32(value) => write!(f, "{value}"),
            Self::F32(value) => write!(f, "{value}"),
            Self::F64(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::List(value) => {
                write!(
                    f,
                    "({})",
                    value
                        .iter()
                        .map(|v| format!("{}", v))
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Self::Callable(data) => write!(f, "<function {data}>"),
            Self::Builtin(data) => write!(f, "<builtin {data}>"),
            Self::Symbol(value) => write!(f, "{value}"),
            Self::Keyword(value) => write!(f, "{value}"),
            Self::Struct(data) => {
                write!(f, "({} ", data.name)?;
                for (name, value) in data.field_names.iter().zip(data.field_values.iter()) {
                    write!(f, "{} {},", name, value)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: String,
    pub span: Span,
    pub field_names: Vec<String>,
    pub field_values: Vec<Value>,
}

impl Struct {
    pub fn size(&self) -> usize {
        // 4 bytes for name length
        4 +
        // name length
        self.name.len() +
        // 4 start span
        4 +
        // 4 end span
        4 +
        // 4 field len
        4 +
        // field names
        self.field_names.iter().map(|name| 4 + name.len()).sum::<usize>() +
        // field values
        self.field_values.iter().map(|value| value.size()).sum::<usize>()
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend(&(self.name.len() as u32).to_le_bytes());
        bytes.extend(self.name.as_bytes());
        let start = self.span.start as u32;
        bytes.extend(&start.to_le_bytes());
        let end = self.span.end as u32;
        bytes.extend(&end.to_le_bytes());
        bytes.extend(&(self.field_names.len() as u32).to_le_bytes());
        for name in self.field_names.iter() {
            bytes.extend(&(name.len() as u32).to_le_bytes());
            bytes.extend(name.as_bytes());
        }
        bytes.extend(&(self.field_values.len() as u32).to_le_bytes());
        for value in self.field_values.iter() {
            bytes.extend(value.to_bytecode());
        }
        debug_assert_eq!(bytes.len(), self.size());
        bytes
    }
}
