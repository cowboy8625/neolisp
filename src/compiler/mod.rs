#![allow(dead_code)]
#![allow(unused)]
mod header;
mod ir;
use crate::ast::{Expr, Spanned};
use crate::parser::parser;
use chumsky::prelude::Parser;

pub use header::Header;
use ir::{Function, Ir, LookupTable, Scope, Value};

pub fn compile(src: &str) -> Result<Vec<u8>, Vec<String>> {
    let ast = get_ast(src)?;
    let mut compiler = Compiler::default();
    let bytes = compiler.compile(&ast)?;

    Ok(bytes)
}

fn get_ast(src: &str) -> Result<Vec<Spanned<Expr>>, Vec<String>> {
    match parser().parse(src) {
        Ok(ast) => Ok(ast),
        Err(errors) => Err(errors
            .into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()),
    }
}

#[derive(Debug)]
struct Compiler {
    header: Header,
    functions: Vec<Function>,
    lookup_table: LookupTable,
    offset: u32,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            header: Header::default(),
            functions: Vec::new(),
            lookup_table: LookupTable::new(),
            offset: Header::SIZE,
        }
    }
}

impl Compiler {
    fn create_lookup_table(&mut self) {
        for function in self.functions.iter() {
            self.lookup_table
                .insert(function.name.clone(), Scope::Global(self.offset));
            let function_size = function.size();
            self.offset += function_size;
        }
    }

    fn generate_ir_code(&mut self, ast: &[Spanned<Expr>]) {
        let mut global_ir_code = Vec::new();
        for spanned in ast.iter() {
            self.compile_expr(&mut global_ir_code, spanned);
        }
    }

    fn compile(&mut self, ast: &[Spanned<Expr>]) -> Result<Vec<u8>, Vec<String>> {
        self.generate_ir_code(ast);
        let mut bytes = Vec::new();
        bytes.extend(self.header.to_bytecode());
        for function in self.functions.iter() {
            bytes.extend(function.to_bytecode(&self.lookup_table));
        }
        Ok(bytes)
    }

    fn compile_expr(&mut self, ir_code: &mut Vec<Ir>, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(v) => ir_code.push(Ir::Value(Value::Bool(*v))),
            Expr::String(v) => ir_code.push(Ir::Value(Value::String(v.clone()))),
            Expr::Symbol(v) => match v.as_str() {
                _ => ir_code.push(Ir::Value(Value::Id(v.clone()))),
            },
            Expr::Number(v) => ir_code.push(Ir::Value(Value::F64(*v))),
            Expr::List(v) => self.compile_s_expr(ir_code, v),
            Expr::Builtin(_, _) => todo!(),
            Expr::Func(_) => todo!(),
            Expr::Lambda(_) => todo!(),
        }
    }

    fn compile_s_expr(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
        match s_expr.first() {
            Some(spanned) => match &spanned.expr {
                Expr::Symbol(v) if v.as_str() == "fn" => self.compile_function(s_expr),
                Expr::Symbol(v) if v.as_str() == "var" => self.compile_var(ir_code, s_expr),
                Expr::Symbol(_) => self.compile_call(ir_code, s_expr),
                v => unimplemented!("{:#?}", v),
            },
            None => {}
        }
    }

    fn compile_function(&mut self, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(1)
        else {
            panic!("expected function name");
        };

        let Some(Spanned {
            expr: Expr::List(params),
            ..
        }) = s_expr.get(2)
        else {
            panic!("expected function params");
        };

        let params = params
            .iter()
            .map(|param| match &param.expr {
                Expr::Symbol(v) => v.to_string(),
                v => panic!("expected function param to be a symbols found {:?}", v),
            })
            .collect();

        let Some(Spanned {
            expr: Expr::List(body),
            ..
        }) = s_expr.get(3)
        else {
            panic!("expected function body");
        };

        let mut instruction = Vec::new();
        self.compile_s_expr(&mut instruction, body);
        if name == "main" {
            instruction.push(Ir::Halt);
        } else {
            instruction.push(Ir::Return);
        }

        let function = Function {
            name: name.to_string(),
            params,
            instruction,
        };

        self.functions.push(function);
    }

    fn compile_var(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(1)
        else {
            panic!("expected variable name");
        };
        for spanned in s_expr.iter().skip(2) {
            self.compile_expr(ir_code, spanned);
        }
        ir_code.push(Ir::LoadGlobalVar(name.to_string()));
    }

    fn compile_call(&mut self, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
        let Some(Spanned {
            expr: Expr::Symbol(name),
            ..
        }) = s_expr.get(0)
        else {
            panic!("expected function name");
        };

        let args = s_expr.iter().skip(1).fold(Vec::new(), |mut acc, s| {
            self.compile_expr(&mut acc, s);
            acc
        });
        ir_code.push(Ir::Call(name.clone(), args));
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;
//     use pretty_assertions::assert_eq;
//     #[test]
//     fn test_compiler_intermediate_code() {
//         let src = include_str!("../../samples/simple.nl");
//         let program = compile(src).unwrap();
//
//         assert_eq!(&program[64..], vec![]);
//     }
// }
