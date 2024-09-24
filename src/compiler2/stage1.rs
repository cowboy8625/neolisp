use super::{BUILTINS, KEYWORDS, OPERATORS};
use crate::ast::{Expr, Spanned};
use crate::symbol_table::{Scope as SymbolScope, Symbol, SymbolKind, SymbolTable};
use crate::vm::{Callee, Value};

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: String,
    pub params: Vec<Stage1Instruction>,
    pub prelude: Vec<Stage1Instruction>,
    pub body: Vec<Stage1Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub name: String,
    pub instructions: Vec<Stage1Instruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stage1Instruction {
    StartAt(usize),
    Noop,
    Halt,
    Return,
    Push(Value),
    Add,
    Rot,
    Call(Callee, u8),
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

#[derive(Debug)]
pub struct Stage1Compiler {
    pub symbol_table: SymbolTable,
    pub functions: Vec<Function>,
    pub lambdas: Vec<Lambda>,
}

impl Stage1Compiler {
    pub fn new(symbol_table: SymbolTable) -> Self {
        Self {
            symbol_table,
            functions: Vec::new(),
            lambdas: Vec::new(),
        }
    }
    pub fn compiler(&mut self, ast: &[Spanned<Expr>]) {
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
    }

    fn compile_expr(&mut self, instructions: &mut Vec<Stage1Instruction>, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(value) => instructions.push(Stage1Instruction::Push(Value::Bool(*value))),
            Expr::String(value) => {
                instructions.push(Stage1Instruction::Push(Value::String(value.to_string())))
            }
            Expr::Number(value) => instructions.push(Stage1Instruction::Push(Value::F64(*value))),
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
                    SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
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
            Expr::Symbol(_) => self.compile_symbol_call(instructions, spanned),
            Expr::Number(_) => todo!(),
            Expr::List(_) => todo!(),
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

        // let Some(symbol) = self.symbol_table.lookup(&name) else {
        //     panic!("unknown symbol to call: {}", name);
        // };

        let callee = if BUILTINS.contains(&name.as_str()) {
            Callee::Builtin(name.clone())
        } else {
            Callee::Function
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
            "var" => self.compile_var(instructions, spanned),
            // "let", "if", "lambda",
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
        // (fn <name> (<params>) ( <body> ))
        debug_assert!(list.len() == 4);

        let Some(name_spanned) = list.get(NAME) else {
            unreachable!();
        };

        let Expr::Symbol(name) = &name_spanned.expr else {
            unreachable!();
        };

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
            body.push(Stage1Instruction::Return);
        }

        self.functions.push(Function {
            name: name.to_string(),
            prelude: vec![],
            params,
            body,
        });
    }
}
