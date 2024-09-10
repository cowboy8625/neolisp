#![allow(dead_code)]
#![allow(unused)]
mod decompiler;
mod header;
mod ir;
use crate::ast::{Expr, Spanned};
use crate::parser::parser;
use chumsky::prelude::Parser;

pub use decompiler::decompile;
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
    fn create_lookup_table(&mut self, global_ir_code: &[Ir]) {
        for function in self.functions.iter() {
            self.lookup_table
                .insert(function.name.clone(), Scope::Global(self.offset));
            let function_size = function.size();
            self.offset += function_size;
        }

        let mut global_var_index = 0;
        for code in global_ir_code.iter() {
            if let Ir::LoadGlobalVar(name) = code {
                self.lookup_table
                    .insert(name.clone(), Scope::Global(global_var_index));
                global_var_index += 1;
            }
        }
    }

    fn generate_ir_code(&mut self, ast: &[Spanned<Expr>]) -> Vec<Ir> {
        let mut global_ir_code = Vec::new();
        for spanned in ast.iter() {
            self.compile_expr(&mut global_ir_code, spanned);
        }

        {
            let Some(main) = self.functions.iter_mut().find(|f| f.name == "main") else {
                panic!("main function not found");
            };
            for instruction in global_ir_code.iter().rev() {
                main.instruction.insert(0, instruction.clone());
            }
        }
        global_ir_code
    }

    fn compile(mut self, ast: &[Spanned<Expr>]) -> Result<Vec<u8>, Vec<String>> {
        let global_ir_code = self.generate_ir_code(ast);
        self.create_lookup_table(&global_ir_code);
        for i in global_ir_code.iter() {
            eprintln!("{:#?}", i.to_bytecode(&self.lookup_table));
        }

        let mut program = Vec::new();

        // eprintln!("{:#?}", self.functions);
        for function in self.functions.iter() {
            let bytecode = function.to_bytecode(&self.lookup_table);

            program.extend(bytecode);
            if function.name == "main" {
                let Some(Scope::Global(main_offset)) = self.lookup_table.get("main") else {
                    panic!("main function not found");
                };
                self.header.set_start(*main_offset);
            }
        }

        Ok(vec![self.header.to_bytecode(), program]
            .into_iter()
            .flatten()
            .collect())
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

<<<<<<< HEAD
// pub fn compile_other(src: &str) -> Result<Vec<u8>, Vec<String>> {
//     let ast = get_ast(src)?;
//     let mut ir_code = Vec::new();
//     let mut func = Vec::new();
//
//     for spanned in ast.iter() {
//         compile_expr(&mut func, &mut ir_code, spanned);
//     }
//
//     let mut func_lookup = std::collections::HashMap::new();
//     let mut offset = Header::SIZE;
//
//     {
//         let Some(main) = func.iter_mut().find(|f| f.name == "main") else {
//             panic!("main function not found");
//         };
//         for instruction in ir_code.iter().rev() {
//             main.instruction.insert(0, instruction.clone());
//         }
//     }
//
//     for function in func.iter() {
//         func_lookup.insert(function.name.clone(), Scope::Global(offset));
//         let function_size = function.size();
//         offset += function_size;
//     }
//
//     let mut global_var_index = 0;
//     for code in ir_code.iter() {
//         if let Ir::LoadGlobalVar(name) = code {
//             func_lookup.insert(name.clone(), Scope::Global(global_var_index));
//             global_var_index += 1;
//         }
//     }
//
//     // eprintln!("{:?}", ir_code);
//     // eprintln!("{:#?}", func);
//
//     let mut program = Vec::new();
//     let mut program_header = Header::new();
//
//     for function in func.iter() {
//         let bytecode = function.to_bytecode(&func_lookup);
//
//         program.extend(bytecode);
//         if function.name == "main" {
//             let Some(Scope::Global(main_offset)) = func_lookup.get("main") else {
//                 panic!("main function not found");
//             };
//             program_header.set_start(*main_offset);
//         }
//     }
//
//     Ok(vec![program_header.to_bytecode(), program]
//         .into_iter()
//         .flatten()
//         .collect())
// }
//
// fn compile_expr(func: &mut Vec<Function>, ir_code: &mut Vec<Ir>, spanned: &Spanned<Expr>) {
//     match &spanned.expr {
//         Expr::Bool(v) => ir_code.push(Ir::Value(Value::Bool(*v))),
//         Expr::String(v) => ir_code.push(Ir::Value(Value::String(v.clone()))),
//         Expr::Symbol(v) => match v.as_str() {
//             _ => ir_code.push(Ir::Value(Value::Id(v.clone()))),
//         },
//         Expr::Number(v) => ir_code.push(Ir::Value(Value::F64(*v))),
//         Expr::List(v) => compile_s_expr(func, ir_code, v),
//         Expr::Builtin(_, _) => todo!(),
//         Expr::Func(_) => todo!(),
//         Expr::Lambda(_) => todo!(),
//     }
// }
//
// fn compile_call(func: &mut Vec<Function>, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
//     let Some(Spanned {
//         expr: Expr::Symbol(name),
//         ..
//     }) = s_expr.get(0)
//     else {
//         panic!("expected function name");
//     };
//
//     let args = s_expr.iter().skip(1).fold(Vec::new(), |mut acc, s| {
//         compile_expr(func, &mut acc, s);
//         acc
//     });
//     ir_code.push(Ir::Call(name.clone(), args));
// }
//
// fn compile_var(func: &mut Vec<Function>, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
//     let Some(Spanned {
//         expr: Expr::Symbol(name),
//         ..
//     }) = s_expr.get(1)
//     else {
//         panic!("expected variable name");
//     };
//     for spanned in s_expr.iter().skip(2) {
//         compile_expr(func, ir_code, spanned);
//     }
//     ir_code.push(Ir::LoadGlobalVar(name.to_string()));
// }
//
// fn compile_s_expr(func: &mut Vec<Function>, ir_code: &mut Vec<Ir>, s_expr: &[Spanned<Expr>]) {
//     match s_expr.first() {
//         Some(spanned) => match &spanned.expr {
//             Expr::Symbol(v) if v.as_str() == "fn" => compile_function(func, s_expr),
//             Expr::Symbol(v) if v.as_str() == "var" => compile_var(func, ir_code, s_expr),
//             Expr::Symbol(_) => compile_call(func, ir_code, s_expr),
//             v => unimplemented!("{:#?}", v),
//             // _ => {
//             //     for spanned in s_expr.iter() {
//             //         compile_expr(func, ir_code, spanned);
//             //     }
//             // }
//         },
//         None => {}
//     }
// }
//
// fn compile_function(func: &mut Vec<Function>, s_expr: &[Spanned<Expr>]) {
//     let Some(Spanned {
//         expr: Expr::Symbol(name),
//         ..
//     }) = s_expr.get(1)
//     else {
//         panic!("expected function name");
//     };
//
//     let Some(Spanned {
//         expr: Expr::List(params),
//         ..
//     }) = s_expr.get(2)
//     else {
//         panic!("expected function params");
//     };
//
//     let params = params
//         .iter()
//         .map(|param| match &param.expr {
//             Expr::Symbol(v) => v.to_string(),
//             v => panic!("expected function param to be a symbols found {:?}", v),
//         })
//         .collect();
//
//     let Some(Spanned {
//         expr: Expr::List(body),
//         ..
//     }) = s_expr.get(3)
//     else {
//         panic!("expected function body");
//     };
//
//     let mut instruction = Vec::new();
//     compile_s_expr(func, &mut instruction, body);
//     if name == "main" {
//         instruction.push(Ir::Halt);
//     } else {
//         instruction.push(Ir::Return);
//     }
//
//     let function = Function {
//         name: name.to_string(),
//         params,
//         instruction,
//     };
//
//     func.push(function);
// }
//
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
fn compile_test(tests: &mut Vec<String>, ast: &Vec<Spanned<Expr>>) -> Result<Vec<u8>, Vec<String>> {
    let mut errors = Vec::new();
    for spanned in ast.iter() {
        let Spanned { expr, .. } = spanned;
        let Expr::List(items) = expr else {
            continue;
        };

        let Some(Spanned { expr, .. }) = items.first() else {
            continue;
        };

        let Expr::Symbol(test) = expr else {
            continue;
        };

        if test != "test" {
            continue;
        }

        let Some(Spanned {
            expr: Expr::String(name),
            ..
        }) = items.get(1)
        else {
            errors.push("expected test name".to_string());
            continue;
        };

        let Some(Spanned { expr, .. }) = items.get(2) else {
            errors.push("expected test body".to_string());
            continue;
        };

        let mut ir_code = Vec::new();
        for item in items.iter().skip(2) {
            compile_expr(&mut Vec::new(), &mut ir_code, item);
        }
        eprintln!("{}: {:#?}", name, ir_code);
    }
    if !errors.is_empty() {
        return Err(errors);
    }

    eprintln!("tests: {:?}", tests);
    Ok(vec![])
}

