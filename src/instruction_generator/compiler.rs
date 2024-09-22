use crate::hir_generator::Function as HirFunction;
use crate::hir_generator::Lambda as HirLambda;
use crate::hir_generator::Value as HirValue;
use crate::hir_generator::Var as HirVar;
use crate::hir_generator::{CompiledHir, Hir, If, Operator, Test};
use crate::symbol_table::{Scope, SymbolKind, SymbolTable};
use crate::vm::{Callee, IState, Instruction, Value};

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

#[derive(Debug, Clone)]
struct Var {
    name: String,
    instructions: Vec<Instruction>,
}

#[derive(Debug)]
pub struct Compiler {
    symbol_table: SymbolTable,
    functions: Vec<Function>,
    lambdas: Vec<Lambda>,
    global_vars: Vec<Var>,
}

impl Compiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            functions: Vec::new(),
            lambdas: Vec::new(),
            global_vars: Vec::new(),
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

        let mut instructions = Vec::new();

        let mut location = 1;

        for lambda in self.lambdas.into_iter() {
            self.symbol_table
                .set_location(Some(&lambda.name), &lambda.name, location);
            location += lambda.instructions.len() as u32;
            instructions.extend(lambda.instructions);
        }

        for function in self.functions.into_iter() {
            self.symbol_table
                .set_location(Some(&function.name), &function.name, location);
            if function.name == "main" {
                for var in self.global_vars.clone().into_iter() {
                    self.symbol_table.set_location(None, &var.name, location);
                    location += var.instructions.len() as u32;
                    instructions.extend(var.instructions);
                }
                // instructions.push(Instruction::Call(Callee::Function(IState::Set(0)), 0));
            }
            location += function.instructions.len() as u32;
            instructions.extend(function.instructions);
        }

        // Resolve symbol locations
        for instruction in instructions.iter_mut() {
            match instruction {
                Instruction::Push(Value::Callable(istate))
                    if matches!(istate, IState::Unset(_)) =>
                {
                    let IState::Unset(name) = istate else {
                        unreachable!();
                    };
                    let Some(symbol) = self.symbol_table.lookup(name) else {
                        panic!("Function `{}` is not defined", name);
                    };
                    let Some(location) = symbol.location else {
                        panic!("Function `{}` location has not been set", name);
                    };
                    *istate = IState::Set(location as usize);
                }
                _ => {}
            }
        }

        // Set start
        let Some(main_symbol) = self.symbol_table.lookup("main") else {
            panic!("Function `main` is not defined");
        };

        let Some(location) = main_symbol.location else {
            panic!("Function `main` location has not been set");
        };

        instructions.insert(0, Instruction::StartAt(location as usize));

        Ok(instructions)
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

    fn compile_vars(&mut self, vars: &[HirVar]) {
        for var in vars {
            self.compile_var(var);
        }
    }

    fn compile_function(&mut self, function: &HirFunction) {
        let HirFunction { name, params, body } = function;

        let mut local_instruction = Vec::new();

        for _ in params.iter() {
            local_instruction.push(Instruction::Rot);
            local_instruction.push(Instruction::LoadLocal);
        }

        self.symbol_table.enter_scope(&name);

        for high_level_instruction in body {
            self.compile_hir(&mut local_instruction, high_level_instruction);
        }

        self.symbol_table.exit_scope();

        // let location = self
        //     .functions
        //     .iter()
        //     .fold(0, |acc, f| acc + f.instructions.len() as u32)
        //     + 1;
        // self.symbol_table.set_location(Some(&name), &name, location);
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

        let mut local_instruction = Vec::new();
        for _ in params.iter() {
            local_instruction.push(Instruction::Rot);
            local_instruction.push(Instruction::LoadLocal);
        }

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

    fn compile_var(&mut self, var: &HirVar) {
        let HirVar {
            name,
            instruction: hir_instructions,
        } = var;
        let mut instructions = Vec::new();
        for hir_instruction in hir_instructions {
            self.compile_hir(&mut instructions, hir_instruction);
        }

        self.global_vars.push(Var {
            name: name.clone(),
            instructions,
        });
    }

    fn compile_hir(&self, instructions: &mut Vec<Instruction>, hir: &Hir) {
        match hir {
            Hir::Return => {
                instructions.push(Instruction::Rot);
                instructions.push(Instruction::Return);
            }
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
            Hir::If(r#if) => {
                let If {
                    condition,
                    then_block,
                    else_block,
                } = r#if;
                eprintln!("{condition:#?}");
                eprintln!("{then_block:#?}");
                eprintln!("{else_block:#?}");
            }
            Hir::BuiltIn(name, args) => {
                for arg in args {
                    self.compile_hir(instructions, arg);
                }

                let arg_count = args.iter().fold(0, |acc, arg| match arg {
                    Hir::LoadGlobal(..) => acc,
                    Hir::LoadLambda(..) => acc,
                    _ => acc + 1,
                });

                instructions.push(Instruction::Call(
                    Callee::Builtin(name.clone()),
                    arg_count as u8,
                ));
            }
            Hir::Call(name, args) => {
                for arg in args {
                    self.compile_hir(instructions, arg);
                }

                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Function `{}` is not defined", name);
                };

                let state = IState::Set(symbol.id);
                let get_callable = if let Scope::Global = symbol.scope {
                    Instruction::GetGlobal(state)
                } else {
                    Instruction::GetLocal(state)
                };

                let arg_count = args.len() as u8;
                instructions.push(get_callable);
                instructions.push(Instruction::Call(Callee::Function, arg_count));
            }
            Hir::LoadTest(_, _) => todo!(),
            Hir::LoadLambda(name) => {
                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Lambda `{}` is not defined", name);
                };

                let state = if let Some(location) = symbol.location {
                    IState::Set(location as usize)
                } else {
                    IState::Unset(name.clone())
                };

                instructions.push(Instruction::Push(Value::Callable(state)));

                let load = if let Scope::Global = symbol.scope {
                    Instruction::LoadGlobal
                } else {
                    Instruction::LoadLocal
                };
                instructions.push(load);
            }
            Hir::LoadGlobal(name, ip) => {
                eprintln!("LoadGlobal {name:#?} -> {ip:#?}");
                instructions.push(Instruction::LoadGlobal);
            }
            Hir::LoadLocal(name, ip) => {
                eprintln!("LoadLocal {name:#?} -> {ip:#?}");
                instructions.push(Instruction::LoadLocal);
            }
            Hir::GetLocalAndLoadFree(name) => {
                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Variable `{}` is not defined", name);
                };
                if let Scope::Global = symbol.scope {
                    instructions.push(Instruction::GetGlobal(IState::Set(symbol.id)));
                } else {
                    instructions.push(Instruction::GetLocal(IState::Set(symbol.id)));
                }
                instructions.push(Instruction::LoadFree);
            }
        }
    }

    fn compile_value(&self, instructions: &mut Vec<Instruction>, value: &HirValue) {
        match value {
            HirValue::Id(name) => {
                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Variable `{}` is not defined", name);
                };

                match symbol.kind {
                    SymbolKind::Parameter => {
                        instructions.push(Instruction::GetLocal(IState::Set(symbol.id)));
                    }
                    SymbolKind::Variable => todo!("Variable: {}, id: {}", name, symbol.id),
                    SymbolKind::Function => todo!("Function: {}, id: {}", name, symbol.id),
                    SymbolKind::Test => todo!("Test {}", name),
                    SymbolKind::Lambda => todo!("Lambda {}", name),
                    SymbolKind::FreeVariable => {
                        instructions.push(Instruction::GetFree(IState::Set(symbol.id)));
                    }
                }
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
