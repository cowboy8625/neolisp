#![allow(dead_code)]
#![allow(unused)]

mod compiler;
mod lir;
#[cfg(test)]
mod test;

use super::{BUILTINS, OPERATORS};
