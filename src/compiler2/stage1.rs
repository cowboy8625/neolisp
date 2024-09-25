use super::{BUILTINS, KEYWORDS, OPERATORS};
use crate::ast::{Expr, Spanned};
use crate::symbol_table::{Scope as SymbolScope, SymbolKind, SymbolTable};

#[derive(Debug)]
pub struct Stage1Data {
    pub functions: Vec<Stage1Function>,
    pub lambdas: Vec<Stage1Lambda>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stage1Function {
    pub name: String,
    pub params: Vec<Stage1Instruction>,
    pub prelude: Vec<Stage1Instruction>,
    pub body: Vec<Stage1Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stage1Lambda {
    pub name: String,
    pub params: Vec<Stage1Instruction>,
    pub body: Vec<Stage1Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stage1Instruction {
    Halt,
    Return,
    Push(Stage1Value),
    Add,
    Rot,
    Call(Stage1Callee, u8),
    LoadLocal,
    GetLocal(IState),
    LoadGlobal,
    GetGlobal(IState),
    LoadFree,
    GetFree(IState),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IState {
    Set(usize),
    Unset(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stage1Callee {
    Function,
    Builtin(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stage1Value {
    U8(u8),
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
    String(String),
    Bool(bool),
    List(Vec<Stage1Value>),
    Callable(IState),
}

#[derive(Debug)]
pub struct Stage1Compiler {
    pub symbol_table: SymbolTable,
    pub functions: Vec<Stage1Function>,
    pub lambdas: Vec<Stage1Lambda>,
    lambda_counter: usize,
}

impl Stage1Compiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            functions: Vec::new(),
            lambdas: Vec::new(),
            lambda_counter: 0,
        }
    }

    fn get_lambda_name(&mut self) -> String {
        let name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    pub fn compiler(mut self, ast: &[Spanned<Expr>]) -> Stage1Data {
        let mut instructions = Vec::new();
        for expr in ast {
            self.compile_expr(&mut instructions, expr);
        }
        for function in self.functions.iter_mut() {
            if function.name == "main" {
                function.prelude.append(&mut instructions);
                break;
            }
        }

        Stage1Data {
            functions: self.functions,
            lambdas: self.lambdas,
        }
    }

    fn compile_expr(&mut self, instructions: &mut Vec<Stage1Instruction>, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(value) => {
                instructions.push(Stage1Instruction::Push(Stage1Value::Bool(*value)))
            }
            Expr::String(value) => instructions.push(Stage1Instruction::Push(Stage1Value::String(
                value.to_string(),
            ))),
            Expr::Number(value) => {
                instructions.push(Stage1Instruction::Push(Stage1Value::F64(*value)))
            }
            Expr::Symbol(name) => {
                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Variable `{}` is not defined", name);
                };

                match symbol.kind {
                    SymbolKind::Parameter => {
                        instructions.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                    }
                    SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
                        instructions.push(Stage1Instruction::GetGlobal(IState::Set(symbol.id)));
                    }
                    SymbolKind::Variable if symbol.scope == SymbolScope::Function => {
                        instructions.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                    }
                    SymbolKind::Variable => todo!("Variable: {}, id: {}", name, symbol.id),
                    SymbolKind::Function => todo!("Function: {}, id: {}", name, symbol.id),
                    SymbolKind::Test => todo!("Test {}", name),
                    SymbolKind::Lambda => todo!("Lambda {}", name),
                    SymbolKind::FreeVariable => {
                        instructions.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
                    }
                }
            }
            Expr::List(_) => self.compile_list(instructions, spanned),
            Expr::Builtin(_, _) => todo!(),
            Expr::Func(_) => todo!(),
            Expr::Lambda(_) => todo!(),
        }
    }

    fn compile_list(&mut self, instructions: &mut Vec<Stage1Instruction>, spanned: &Spanned<Expr>) {
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };

        let Some(first) = list.first() else {
            todo!("empty list {:#?}", spanned);
        };

        match &first.expr {
            Expr::Bool(_) => todo!(),
            Expr::String(_) => todo!(),
            Expr::Symbol(name) if KEYWORDS.contains(&name.as_str()) => {
                self.compile_keyword(instructions, spanned, name)
            }
            Expr::Symbol(name) if OPERATORS.contains(&name.as_str()) => {
                self.compile_operator(instructions, spanned)
            }
            Expr::Symbol(_) => {
                self.compile_symbol_call(instructions, spanned);
            }
            Expr::Number(_) => todo!(),
            Expr::List(_) => {
                let mut count = 0;
                for item in list.iter().skip(1) {
                    count += 1;
                    self.compile_expr(instructions, item);
                }
                self.compile_list(instructions, first);
                // let Some(Stage1Instruction::Push(Stage1Value::Callable(_))) = instructions.last()
                // else {
                //     eprintln!("{list:#?} INSTRUCTIONS {:#?}", instructions);
                //     return;
                // };
                instructions.push(Stage1Instruction::Call(Stage1Callee::Function, count));
            }
            Expr::Builtin(_, _) => todo!(),
            Expr::Func(_) => todo!(),
            Expr::Lambda(_) => todo!(),
        }
    }

    fn compile_symbol_call(
        &mut self,
        instructions: &mut Vec<Stage1Instruction>,
        spanned: &Spanned<Expr>,
    ) {
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };

        let Some(name_spanned) = list.first() else {
            unreachable!();
        };

        let Expr::Symbol(name) = &name_spanned.expr else {
            unreachable!();
        };

        let count = list.len() - 1;
        for spanned in list.iter().skip(1) {
            self.compile_expr(instructions, spanned);
        }

        if let Some(symbol) = self.symbol_table.lookup(&name) {
            match symbol.kind {
                SymbolKind::FreeVariable => todo!(),
                SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
                    instructions.push(Stage1Instruction::GetGlobal(IState::Set(symbol.id)));
                }
                SymbolKind::Variable if symbol.scope == SymbolScope::Function => {
                    instructions.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                }
                SymbolKind::Variable if symbol.scope == SymbolScope::Free => {
                    instructions.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
                }
                SymbolKind::Parameter => {
                    instructions.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                }
                SymbolKind::Function => {
                    instructions.push(Stage1Instruction::Push(Stage1Value::Callable(
                        IState::Unset(name.clone()),
                    )));
                }
                SymbolKind::Test => todo!(),
                SymbolKind::Lambda => todo!(),
                _ => todo!(),
            }
        };

        let callee = if BUILTINS.contains(&name.as_str()) {
            Stage1Callee::Builtin(name.clone())
        } else {
            Stage1Callee::Function
        };
        instructions.push(Stage1Instruction::Call(callee, count as u8));
    }

    fn compile_operator(
        &mut self,
        instructions: &mut Vec<Stage1Instruction>,
        spanned: &Spanned<Expr>,
    ) {
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };
        let Some(operator_spanned) = list.first() else {
            unreachable!();
        };
        let Expr::Symbol(operator) = &operator_spanned.expr else {
            unreachable!();
        };
        let op = match operator.as_str() {
            "+" => Stage1Instruction::Add,
            _ => unreachable!("unknown operator: {}", operator),
        };

        let count = list.len() - 1;
        for spanned in list.iter().skip(1) {
            self.compile_expr(instructions, spanned);
        }
        for _ in 0..count - 1 {
            instructions.push(op.clone());
        }
    }

    fn compile_keyword(
        &mut self,
        instructions: &mut Vec<Stage1Instruction>,
        spanned: &Spanned<Expr>,
        name: &str,
    ) {
        match name {
            "fn" => self.compile_fn(spanned),
            "lambda" => self.compile_lambda(instructions, spanned),
            "var" => self.compile_var(instructions, spanned),
            // "let", "if"
            _ => unreachable!("unknown keyword: {}", name),
        }
    }

    fn compile_var(&mut self, instructions: &mut Vec<Stage1Instruction>, spanned: &Spanned<Expr>) {
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };
        let Some(name_spanned) = list.get(1) else {
            unreachable!();
        };
        let Expr::Symbol(name) = &name_spanned.expr else {
            unreachable!();
        };

        for expr in list.iter().skip(2) {
            self.compile_expr(instructions, expr);
        }

        let Some(symbol) = self.symbol_table.lookup(name) else {
            panic!("unknown symbol: {}", name);
        };

        let instruction = if symbol.scope == SymbolScope::Global {
            Stage1Instruction::LoadGlobal
        } else {
            Stage1Instruction::LoadLocal
        };
        instructions.push(instruction);
    }

    fn compile_fn(&mut self, spanned: &Spanned<Expr>) {
        const NAME: usize = 1;
        const PARAMS: usize = 2;
        const BODY: usize = 3;
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };

        debug_assert!(list.len() == 4, "(fn <name> (<params>) ( <body> ))");

        let Some(name_spanned) = list.get(NAME) else {
            unreachable!();
        };

        let Expr::Symbol(name) = &name_spanned.expr else {
            unreachable!();
        };

        self.symbol_table.enter_scope(name);

        let Some(params_spanned) = &list.get(PARAMS) else {
            panic!("expected list for params");
        };

        let Expr::List(expr_params) = &params_spanned.expr else {
            panic!(
                "expected list for params but found {:?}",
                params_spanned.expr
            );
        };

        let mut params = Vec::new();
        for param in expr_params.iter() {
            let Expr::Symbol(_) = &param.expr else {
                panic!("expected symbol for param");
            };
            params.push(Stage1Instruction::Rot);
            params.push(Stage1Instruction::LoadLocal);
        }

        let mut body = Vec::new();
        for expr in list.iter().skip(BODY) {
            self.compile_expr(&mut body, expr);
        }

        if name == "main" {
            body.push(Stage1Instruction::Halt);
        } else {
            body.push(Stage1Instruction::Rot);
            body.push(Stage1Instruction::Return);
        }

        self.symbol_table.exit_scope();

        self.functions.push(Stage1Function {
            name: name.to_string(),
            prelude: vec![],
            params,
            body,
        });
    }

    fn compile_lambda(
        &mut self,
        instructions: &mut Vec<Stage1Instruction>,
        spanned: &Spanned<Expr>,
    ) {
        const PARAMS: usize = 1;
        const BODY: usize = 2;
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };
        debug_assert!(list.len() == 3, "(lambda (<params>) ( <body> ))");

        let name = self.get_lambda_name();

        self.symbol_table.enter_scope(&name);

        let Some(params_spanned) = &list.get(PARAMS) else {
            panic!("expected list for params");
        };

        let Expr::List(expr_params) = &params_spanned.expr else {
            panic!(
                "expected list for params but found {:?}",
                params_spanned.expr
            );
        };

        let mut params = Vec::new();
        for param in expr_params.iter() {
            let Expr::Symbol(_) = &param.expr else {
                panic!("expected symbol for param");
            };
            params.push(Stage1Instruction::Rot);
            params.push(Stage1Instruction::LoadLocal);
        }

        let mut body = Vec::new();
        for expr in list.iter().skip(BODY) {
            self.compile_expr(&mut body, expr);
        }

        let Some(everything) = self.symbol_table.get_function_scope(&name) else {
            panic!("no scope for {name:?}");
        };
        for (name, symbol) in everything.iter() {
            if symbol.kind == SymbolKind::FreeVariable {
                let instruction = if symbol.scope == SymbolScope::Global {
                    Stage1Instruction::GetGlobal(IState::Unset(name.to_string()))
                } else {
                    Stage1Instruction::GetLocal(IState::Unset(name.to_string()))
                };
                instructions.push(instruction);
                instructions.push(Stage1Instruction::LoadFree);
            }
        }
        // let captured = get_variables(&body);

        // eprintln!("name: {name:?}");
        // eprintln!("captured: {captured:?}");
        // for name in captured.iter() {
        //     let Some(symbol) = self.symbol_table.lookup(name) else {
        //         panic!("unknown symbol: {}", name);
        //     };
        //     let instruction = if symbol.scope == SymbolScope::Global {
        //         Stage1Instruction::GetGlobal(IState::Unset(name.to_string()))
        //     } else {
        //         Stage1Instruction::GetLocal(IState::Unset(name.to_string()))
        //     };
        //     instructions.push(instruction);
        // }

        body.push(Stage1Instruction::Rot);
        body.push(Stage1Instruction::Return);

        self.symbol_table.exit_scope();

        instructions.push(Stage1Instruction::Push(Stage1Value::Callable(
            IState::Unset(name.to_string()),
        )));

        self.lambdas.push(Stage1Lambda {
            name: name.to_string(),
            params,
            body,
        });
    }
}
