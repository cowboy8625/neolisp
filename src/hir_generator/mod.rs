mod compiler;
mod hir;
#[cfg(test)]
mod test;

use super::{BUILTINS, OPERATORS};
pub use compiler::Compiler as HirCompiler;
pub use hir::{CompiledHir, Function, Hir, If, Lambda, Operator, Test, Value, Var};
