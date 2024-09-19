#![allow(dead_code)]
#![allow(unused)]
use super::lir::{Lir, Type};
use crate::hir_generator::{CompiledHir, Function, Hir, If, Lambda, Operator, Test, Value, Var};
use crate::symbol_table::SymbolTable;

#[derive(Debug)]
pub struct Compiler {
    symbol_table: SymbolTable,
    code_section: Vec<Lir>,
}

impl Compiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            code_section: Vec::new(),
        }
    }

    pub fn compile(mut self, compiled_hir: &[CompiledHir]) -> Result<Vec<Lir>, Vec<String>> {
        for branch in compiled_hir {
            match branch {
                CompiledHir::Functions(functions) => self.compile_functions(functions),
                CompiledHir::Lambdas(lambdas) => self.compile_lambdas(lambdas),
                CompiledHir::Tests(tests) => self.compile_tests(tests),
                CompiledHir::GlobalVar(vars) => self.compile_vars(vars),
            }
        }
        Ok(self.code_section)
    }

    fn compile_functions(&self, functions: &[Function]) {
        for function in functions {
            self.compile_function(function);
        }
    }

    fn compile_lambdas(&self, lambdas: &[Lambda]) {
        for lambda in lambdas {
            self.compile_lambda(lambda);
        }
    }

    fn compile_tests(&self, tests: &[Test]) {
        for test in tests {
            todo!()
        }
    }

    fn compile_vars(&self, vars: &[Var]) {
        for var in vars {
            self.compile_var(var);
        }
    }

    fn compile_function(&self, function: &Function) {
        let Function { name, params, body } = function;

        let Some(symbol) = self.symbol_table.get_function_scope(name) else {
            panic!("Function `{}` is not defined", name);
        };

        // NOTE: No type checker yet so all numbers will be f64
        let mut lir_code = Vec::new();
        for instruction in body {
            self.compile_hir(&mut lir_code, instruction);
        }
        eprintln!("{:#?}", lir_code);
    }

    fn compile_lambda(&self, lambda: &Lambda) {
        let Lambda {
            name,
            params,
            instruction,
            captured,
        } = lambda;

        let Some(symbol) = self.symbol_table.get_function_scope(name) else {
            panic!("Lambda `{}` is not defined", name);
        };

        eprintln!("{} {:#?}", name, symbol);
    }

    fn compile_var(&self, var: &Var) {
        let Var { name, instruction } = var;

        let Some(symbol) = self.symbol_table.lookup(name) else {
            panic!("Variable `{}` is not defined", name);
        };

        eprintln!("{} {:#?}", name, symbol);
    }

    fn compile_hir(&self, lir_code: &mut Vec<Lir>, instruction: &Hir) {
        match instruction {
            Hir::Return => lir_code.push(Lir::Return),
            Hir::Halt => lir_code.push(Lir::Halt),
            Hir::Operator(op, args) => match op {
                Operator::Add => {
                    for arg in args {
                        self.compile_hir(lir_code, arg);
                    }
                    lir_code.push(Lir::Add {
                        ty: Type::Float,
                        count: args.len() as u32,
                    });
                }
                Operator::Eq => todo!(),
            },
            Hir::Value(value) => self.compile_value(lir_code, value),
            Hir::If(r#if) => todo!(),
            Hir::BuiltIn(name, args) => lir_code.push(Lir::BuiltIn {
                arg_count: args.len() as u32,
                name: name.clone(),
            }),
            Hir::Call(name, args) => todo!(),
            Hir::LoadGlobalVar(name) => todo!(),
            Hir::LoadTest(_, _) => todo!(),
            Hir::LoadLambda(_) => todo!(),
        }
    }

    fn compile_value(&self, lir_code: &mut Vec<Lir>, value: &Value) {
        match value {
            Value::Id(name) => {
                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Variable `{}` is not defined", name);
                };
                // let ty = symbol.symbol_type;
                // lir_code.push(Lir::Push { ty });
            }
            Value::U8(_) => todo!(),
            Value::U32(_) => todo!(),
            Value::F64(_) => todo!(),
            Value::String(_) => todo!(),
            Value::Bool(_) => todo!(),
        }
    }
}
