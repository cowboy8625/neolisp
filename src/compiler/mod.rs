#![allow(dead_code)]
#![allow(unused)]
mod decompiler;
mod header;
mod ir;
use crate::ast::{Expr, Spanned};
use crate::parser::parser;
use chumsky::prelude::Parser;

pub use decompiler::{decompile, decompile_chunk, display_chunk};
pub use header::Header;
use ir::{Function, Ir, LookupTable, Scope, Value};

pub fn compile(src: &str) -> Result<Vec<u8>, Vec<String>> {
    let ast = get_ast(src)?;
    let mut compiler = Compiler::default();
    let bytes = compiler.compile(&ast)?;

    Ok(bytes)
}

pub fn compile_chunk(src: &str) -> Result<Vec<u8>, Vec<String>> {
    let ast = get_ast(src)?;
    let mut compiler = Compiler::new(None);
    let bytes = compiler.compile(&ast)?;

    Ok(bytes)
}

pub fn compile_to_instructions(src: &str) -> Result<(Vec<Ir>, Vec<Function>), Vec<String>> {
    let ast = get_ast(src)?;
    let mut compiler = Compiler::new(None);
    let instructions = compiler.generate_ir_code(&ast);

    Ok((instructions, compiler.functions))
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
    header: Option<Header>,
    functions: Vec<Function>,
    lookup_table: LookupTable,
    offset: u32,
}

impl Default for Compiler {
    fn default() -> Self {
        Self {
            header: Some(Header::default()),
            functions: Vec::new(),
            lookup_table: LookupTable::new(),
            offset: Header::SIZE,
        }
    }
}

impl Compiler {
    pub fn new(header: Option<Header>) -> Self {
        let offset = if header.is_some() { Header::SIZE } else { 0 };
        Self {
            header,
            offset,
            ..Default::default()
        }
    }

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

        if self.header.is_some() {
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
        let mut program = Vec::new();

        for function in self.functions.iter() {
            let bytecode = function.to_bytecode(&self.lookup_table);
            eprintln!("Function: {}", function.name);
            display_chunk(&bytecode);
            debug_assert_eq!(
                bytecode.len() as u32,
                function.size(),
                "function {} size does not match its bytecode size",
                function.name
            );
            program.extend(bytecode);

            if self.header.is_none() {
                continue;
            }
            if function.name == "main" {
                let Some(Scope::Global(main_offset)) = self.lookup_table.get("main").copied()
                else {
                    if self.header.is_none() {
                        continue;
                    }
                    panic!("main function not found");
                };
                let Some(header) = self.header.as_mut() else {
                    continue;
                };
                header.set_start(main_offset)
            }
        }
        // eprintln!("{:#?}", self.lookup_table);

        let header_bytes = match self.header {
            Some(h) => h.to_bytecode(),
            None => vec![],
        };

        Ok(vec![header_bytes, program].into_iter().flatten().collect())
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

        match name.as_str() {
            "+" | "print" | "list" | "nth" | "length" => {
                ir_code.push(Ir::BuiltIn(name.clone(), args))
            }
            _ => ir_code.push(Ir::Call(name.clone(), args)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_compile_chunk() {
        // TODO: Check if the size count is correct for Instructions
        let src = "(fn add (a b) (+ a b))";
        let (globals, functions) = compile_to_instructions(src).unwrap();
        eprintln!(
            "Add Function IR {}:\n{:#?}",
            functions[0].size(),
            functions[0]
        );
        let bytes = compile_chunk(src).unwrap();
        eprintln!("length: {}\n{:#?}", bytes.len(), bytes);
        display_chunk(&bytes);
        assert_eq!(bytes.len(), 8);
    }
}
