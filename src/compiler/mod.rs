#![allow(dead_code)]
#![allow(unused)]
mod header;
mod ir;
use crate::ast::{Expr, Spanned};
use crate::parser::parser;
use chumsky::prelude::Parser;

pub use header::Header;
use ir::{Function, Ir, Value};

fn get_ast(src: &str) -> Result<Vec<Spanned<Expr>>, Vec<String>> {
    match parser().parse(src) {
        Ok(ast) => Ok(ast),
        Err(errors) => Err(errors
            .into_iter()
            .map(|e| e.to_string())
            .collect::<Vec<_>>()),
    }
}

pub fn compile(src: &str) -> Result<Vec<u8>, Vec<String>> {
    let ast = get_ast(src)?;
    let mut ir_code = Vec::new();
    let mut func = Vec::new();

    for spanned in ast.iter() {
        compile_expr(&mut func, &mut ir_code, spanned);
    }

    let mut func_lookup = std::collections::HashMap::new();
    let mut offset = Header::SIZE;
    for function in func.iter() {
        func_lookup.insert(function.name.clone(), offset);
        let function_size = function.size();
        offset += function_size;
    }

    // eprintln!("{:#?}", func);
    let mut program = Vec::new();
    let mut program_header = Header::new();

    for function in func.iter() {
        let bytecode = function.to_bytecode(&func_lookup);

        program.extend(bytecode);
        if function.name == "main" {
            let main_offset = func_lookup.get("main").unwrap();
            program_header.set_start(*main_offset);
        }
    }

    Ok(vec![program_header.to_bytecode(), program]
        .into_iter()
        .flatten()
        .collect())
}

fn compile_expr(func: &mut Vec<Function>, ir_code: &mut Vec<Ir>, spanned: &Spanned<Expr>) {
    match &spanned.expr {
        Expr::Bool(v) => ir_code.push(Ir::Value(Value::Bool(*v))),
        Expr::String(v) => ir_code.push(Ir::Value(Value::String(v.clone()))),
        Expr::Symbol(v) => match v.as_str() {
            _ => ir_code.push(Ir::Value(Value::Id(v.clone()))),
        },
        Expr::Number(v) => ir_code.push(Ir::Value(Value::F64(*v))),
        Expr::List(v) => compile_s_expr(func, ir_code, v),
        Expr::Builtin(_, _) => todo!(),
        Expr::Func(_) => todo!(),
        Expr::Lambda(_) => todo!(),
    }
}

fn compile_call(func: &mut Vec<Function>, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
    let Some(Spanned {
        expr: Expr::Symbol(name),
        ..
    }) = s_expr.get(0)
    else {
        panic!("expected function name");
    };

    let args = s_expr.iter().skip(1).fold(Vec::new(), |mut acc, s| {
        compile_expr(func, &mut acc, s);
        acc
    });
    ir_code.push(Ir::Call(name.clone(), args));
}

fn compile_s_expr(func: &mut Vec<Function>, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
    match s_expr.first() {
        Some(spanned) => match &spanned.expr {
            Expr::Symbol(v) if v.as_str() == "fn" => compile_function(func, s_expr),
            Expr::Symbol(_) => compile_call(func, ir_code, s_expr),
            _ => panic!("expected symbol"),
            // _ => {
            //     for spanned in s_expr.iter() {
            //         compile_expr(func, ir_code, spanned);
            //     }
            // }
        },
        None => {}
    }
}

fn compile_function(func: &mut Vec<Function>, s_expr: &[Spanned<Expr>]) {
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
    compile_s_expr(func, &mut instruction, body);
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

    func.push(function);
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    #[test]
    fn test_compiler_intermediate_code() {
        let src = "(fn add (x y) (+ x y )) (fn main () (print (add 1 2)))";
        let program = compile(src).unwrap();

        assert_eq!(&program[64..], vec![]);
    }
}
