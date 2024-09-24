mod builtin;
mod instruction;
mod machine;
mod value;
pub use instruction::{Callee, IState, Instruction};
pub use machine::Machine;
pub use value::Value;