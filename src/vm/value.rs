#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U8(u8),
    U32(u32),
    F64(f64),
    String(String),
    Bool(bool),
    List(Vec<Value>),
    Lambda(u32),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(value) => write!(f, "{value}"),
            Self::U32(value) => write!(f, "{value}"),
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
            Self::Lambda(_) => write!(f, "<lambda>"),
        }
    }
}
