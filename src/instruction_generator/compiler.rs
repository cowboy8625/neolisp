use crate::hir_generator::Function as HirFunction;
use crate::hir_generator::Lambda as HirLambda;
use crate::hir_generator::Value as HirValue;
use crate::hir_generator::{CompiledHir, Hir, If, Operator, Test, Var};
use crate::symbol_table::{SymbolKind, SymbolTable};
use crate::vm::{Callee, Instruction, Value};

#[derive(Debug)]
struct Function {
    name: String,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
struct Lambda {
    name: String,
    instructions: Vec<Instruction>,
    captured: Vec<String>,
}

#[derive(Debug)]
pub struct Compiler {
    symbol_table: SymbolTable,
    functions: Vec<Function>,
    lambdas: Vec<Lambda>,
}

impl Compiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            functions: Vec::new(),
            lambdas: Vec::new(),
        }
    }

    pub fn compile(
        mut self,
        compiled_hir: &[CompiledHir],
    ) -> Result<Vec<Instruction>, Vec<String>> {
        for branch in compiled_hir {
            match branch {
                CompiledHir::Functions(functions) => self.compile_functions(functions),
                CompiledHir::Lambdas(lambdas) => self.compile_lambdas(lambdas),
                CompiledHir::Tests(tests) => self.compile_tests(tests),
                CompiledHir::GlobalVar(vars) => self.compile_vars(vars),
            }
        }
        Ok(vec![])
    }

    fn compile_functions(&mut self, functions: &[HirFunction]) {
        for function in functions {
            self.compile_function(function);
        }
    }

    fn compile_lambdas(&mut self, lambdas: &[HirLambda]) {
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

    fn compile_function(&mut self, function: &HirFunction) {
        let HirFunction { name, params, body } = function;

        let mut local_instruction = Vec::new();

        for _ in params.iter() {
            local_instruction.push(Instruction::Rot);
            local_instruction.push(Instruction::PopIntoLocalStack);
        }

        self.symbol_table.enter_scope(&name);

        for high_level_instruction in body {
            self.compile_hir(&mut local_instruction, high_level_instruction);
        }

        self.symbol_table.exit_scope();

        self.functions.push(Function {
            name: name.clone(),
            instructions: local_instruction,
        });
    }

    fn compile_lambda(&mut self, lambda: &HirLambda) {
        let HirLambda {
            name,
            params,
            instruction,
            captured,
        } = lambda;

        self.symbol_table.enter_scope(&name);
        let Some(symbol) = self.symbol_table.get_function_scope(name) else {
            panic!("Lambda `{}` is not defined", name);
        };

        let mut local_instruction = Vec::new();
        self.symbol_table.enter_scope(&name);
        for high_level_instruction in instruction {
            self.compile_hir(&mut local_instruction, high_level_instruction);
        }
        self.symbol_table.exit_scope();

        self.lambdas.push(Lambda {
            name: name.clone(),
            instructions: local_instruction,
            captured: captured.clone(),
        })
    }

    fn compile_var(&self, var: &Var) {
        let Var { name, instruction } = var;

        let Some(symbol) = self.symbol_table.lookup(name) else {
            panic!("Variable `{}` is not defined", name);
        };

        // eprintln!("{} {:#?}", name, symbol);
    }

    fn compile_hir(&self, instructions: &mut Vec<Instruction>, hir: &Hir) {
        match hir {
            Hir::Return => instructions.push(Instruction::Return),
            Hir::Halt => instructions.push(Instruction::Halt),
            Hir::Operator(op, args) => match op {
                Operator::Add => {
                    for arg in args {
                        self.compile_hir(instructions, arg);
                    }
                    for _ in 0..args.len() - 1 {
                        instructions.push(Instruction::Add);
                    }
                }
                Operator::Eq => todo!(),
            },
            Hir::Value(value) => self.compile_value(instructions, value),
            Hir::If(r#if) => todo!(),
            Hir::BuiltIn(name, args) => {
                for arg in args {
                    self.compile_hir(instructions, arg);
                }
                instructions.push(Instruction::Call(
                    Callee::Builtin(name.clone()),
                    args.len() as u8,
                ));
            }
            Hir::Call(name, args) => {
                for arg in args {
                    self.compile_hir(instructions, arg);
                }
                instructions.push(Instruction::Call(
                    Callee::UnSetFunctionLocation(name.clone()),
                    args.len() as u8,
                ));
            }
            Hir::LoadGlobalVar(_) => todo!(),
            Hir::LoadTest(_, _) => todo!(),
            Hir::LoadLambda(_) => todo!(),
        }
    }

    fn compile_value(&self, instructions: &mut Vec<Instruction>, value: &HirValue) {
        match value {
            HirValue::Id(name) => {
                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Variable `{}` is not defined", name);
                };

                let SymbolKind::Parameter(index) = symbol.kind else {
                    panic!("Variable `{}` is not a parameter", name);
                };

                instructions.push(Instruction::LoadLocal(index));
            }
            HirValue::U8(value) => instructions.push(Instruction::Push(Value::U8(*value))),
            HirValue::U32(value) => instructions.push(Instruction::Push(Value::U32(*value))),
            HirValue::F64(value) => instructions.push(Instruction::Push(Value::F64(*value))),
            HirValue::String(value) => {
                instructions.push(Instruction::Push(Value::String(value.to_string())))
            }
            HirValue::Bool(value) => instructions.push(Instruction::Push(Value::Bool(*value))),
        }
    }
}
