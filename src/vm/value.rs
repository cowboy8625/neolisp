use super::instruction::IState;

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
    Callable(IState),
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
            Self::Callable(IState::Set(index)) => write!(f, "<function {index:02X}>"),
            Self::Callable(IState::Unset(name)) => write!(f, "<function unset {name}>"),
        }
    }
}
