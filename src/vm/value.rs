#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U8(u8),
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
    String(String),
    Bool(bool),
    List(Vec<Value>),
    Callable(usize),
}

impl Value {
    pub const CODE_U8: u8 = 0x00;
    pub const CODE_I32: u8 = 0x01;
    pub const CODE_U32: u8 = 0x02;
    pub const CODE_F32: u8 = 0x03;
    pub const CODE_F64: u8 = 0x04;
    pub const CODE_STRING: u8 = 0x05;
    pub const CODE_BOOL: u8 = 0x06;
    pub const CODE_LIST: u8 = 0x07;
    pub const CODE_CALLABLE: u8 = 0x08;

    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Value::U8(v) => vec![Self::CODE_U8, *v],
            Value::I32(v) => {
                let mut bytes = vec![Self::CODE_I32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::U32(v) => {
                let mut bytes = vec![Self::CODE_U32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F32(v) => {
                let mut bytes = vec![Self::CODE_F32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F64(v) => {
                let mut bytes = vec![Self::CODE_F64];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::String(v) => {
                let mut bytes = vec![Self::CODE_STRING];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(&v.as_bytes());
                bytes
            }
            Value::Bool(v) => vec![Self::CODE_BOOL, if *v { 1 } else { 0 }],
            Value::List(vec) => {
                let mut bytes = vec![Self::CODE_LIST];
                bytes.extend_from_slice(&(vec.len() as u32).to_le_bytes());
                for v in vec {
                    bytes.extend(v.to_bytecode());
                }
                bytes
            }
            Value::Callable(v) => {
                let mut bytes = vec![Self::CODE_CALLABLE];
                bytes.extend_from_slice(&(*v as u32).to_le_bytes());
                bytes
            }
        }
    }

    pub fn size(&self) -> usize {
        let conent_size = match self {
            Value::U8(_) => 1,
            Value::I32(_) => 4,
            Value::U32(_) => 4,
            Value::F32(_) => 4,
            Value::F64(_) => 8,
            Value::String(v) => 4 + v.len(),
            Value::Bool(_) => 1,
            Value::List(vec) => 4 + vec.iter().map(|v| v.size()).sum::<usize>(),
            Value::Callable(_) => 4,
        };
        // opcode + content
        1 + conent_size
    }

    pub fn type_of(&self) -> String {
        match self {
            Self::U8(_) => "u8".to_string(),
            Self::I32(_) => "i32".to_string(),
            Self::U32(_) => "u32".to_string(),
            Self::F32(_) => "f32".to_string(),
            Self::F64(_) => "f64".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Bool(_) => "Bool".to_string(),
            Self::List(_) => "List".to_string(),
            Self::Callable(_) => "Function".to_string(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
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
            Self::Callable(index) => write!(f, "<function {index:02X}>"),
        }
    }
}
