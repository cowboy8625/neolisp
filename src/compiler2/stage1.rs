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
    pub params: Chunk,
    pub prelude: Chunk,
    pub body: Chunk,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Stage1Lambda {
    pub name: String,
    pub params: Chunk,
    pub body: Chunk,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stage1Instruction {
    Halt,
    Return,
    Push(Stage1Value),
    Add,
    Sub,
    Eq,
    Or,
    Rot,
    Call(Stage1Callee, u8),
    LoadLocal,
    GetLocal(IState),
    LoadGlobal,
    GetGlobal(IState),
    LoadFree,
    GetFree(IState),
    JumpIf(usize),
    Jump(usize),
}
impl Stage1Instruction {
    pub fn size(&self) -> usize {
        match self {
            Self::Add
            | Self::Sub
            | Self::Eq
            | Self::Or
            | Self::Rot
            | Self::Halt
            | Self::LoadGlobal
            | Self::LoadLocal
            | Self::LoadFree
            | Self::Return => 1,
            Self::Push(value) => 1 + value.size(),
            Self::Call(callee, _) => 1 + callee.size() + 1,
            Self::GetLocal(_) | Self::GetGlobal(_) | Self::GetFree(_) => 2,
            Self::JumpIf(_) | Self::Jump(_) => 5,
        }
    }
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

impl Stage1Callee {
    pub fn size(&self) -> usize {
        match self {
            Self::Function => 1,
            Self::Builtin(name) => 1 + 1 + name.len(),
        }
    }
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

impl Stage1Value {
    pub fn size(&self) -> usize {
        let conent_size = match self {
            Self::U8(_) => 1,
            Self::I32(_) => 4,
            Self::U32(_) => 4,
            Self::F32(_) => 4,
            Self::F64(_) => 8,
            Self::String(v) => 4 + v.len(),
            Self::Bool(_) => 1,
            Self::List(vec) => 4 + vec.iter().map(|v| v.size()).sum::<usize>(),
            Self::Callable(_) => 4,
        };
        // opcode + content
        1 + conent_size
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Chunk {
    pub items: Vec<Stage1Instruction>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push(&mut self, instruction: Stage1Instruction) {
        self.items.push(instruction);
    }

    pub fn extend(&mut self, other: Chunk) {
        self.items.extend(other.items);
    }

    pub fn size(&self) -> usize {
        self.items.iter().map(|i| i.size()).sum()
    }
}

impl From<Vec<Stage1Instruction>> for Chunk {
    fn from(items: Vec<Stage1Instruction>) -> Self {
        Self { items }
    }
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
        let mut chunk = Chunk::new();
        for expr in ast {
            self.compile_expr(&mut chunk, expr);
        }
        for function in self.functions.iter_mut() {
            if function.name == "main" {
                function.prelude.extend(chunk);
                break;
            }
        }

        Stage1Data {
            functions: self.functions,
            lambdas: self.lambdas,
        }
    }

    fn compile_expr(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
        match &spanned.expr {
            Expr::Bool(value) => chunk.push(Stage1Instruction::Push(Stage1Value::Bool(*value))),
            Expr::String(value) => chunk.push(Stage1Instruction::Push(Stage1Value::String(
                value.to_string(),
            ))),
            Expr::Number(value) => chunk.push(Stage1Instruction::Push(Stage1Value::F64(*value))),
            Expr::Symbol(name) => {
                let Some(symbol) = self.symbol_table.lookup(name) else {
                    panic!("Variable `{}` is not defined", name);
                };

                match symbol.kind {
                    SymbolKind::Parameter => {
                        chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                    }
                    SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
                        chunk.push(Stage1Instruction::GetGlobal(IState::Set(symbol.id)));
                    }
                    SymbolKind::Variable if symbol.scope == SymbolScope::Function => {
                        chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                    }
                    SymbolKind::Variable => todo!("Variable: {}, id: {}", name, symbol.id),
                    SymbolKind::Function => todo!("Function: {}, id: {}", name, symbol.id),
                    SymbolKind::Test => todo!("Test {}", name),
                    SymbolKind::Lambda => todo!("Lambda {}", name),
                    SymbolKind::FreeVariable => {
                        chunk.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
                    }
                }
            }
            Expr::List(_) => self.compile_list(chunk, spanned),
            Expr::Builtin(_, _) => todo!(),
            Expr::Func(_) => todo!(),
            Expr::Lambda(_) => todo!(),
        }
    }

    fn compile_list(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
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
                self.compile_keyword(chunk, spanned, name)
            }
            Expr::Symbol(name) if OPERATORS.contains(&name.as_str()) => {
                self.compile_operator(chunk, spanned)
            }
            Expr::Symbol(_) => {
                self.compile_symbol_call(chunk, spanned);
            }
            Expr::Number(_) => todo!(),
            Expr::List(_) => {
                let mut count = 0;
                for item in list.iter().skip(1) {
                    count += 1;
                    self.compile_expr(chunk, item);
                }
                self.compile_list(chunk, first);
                chunk.push(Stage1Instruction::Call(Stage1Callee::Function, count));
            }
            Expr::Builtin(_, _) => todo!(),
            Expr::Func(_) => todo!(),
            Expr::Lambda(_) => todo!(),
        }
    }

    fn compile_symbol_call(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };

        let Some(name_spanned) = list.first() else {
            unreachable!();
        };

        let Expr::Symbol(name) = &name_spanned.expr else {
            unreachable!();
        };

        if let Some(symbol) = self.symbol_table.lookup(&name) {
            match symbol.kind {
                SymbolKind::Parameter => {
                    chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                }
                _ => {}
            }
        };

        let count = list.len() - 1;
        for spanned in list.iter().skip(1) {
            self.compile_expr(chunk, spanned);
        }

        if let Some(symbol) = self.symbol_table.lookup(&name) {
            match symbol.kind {
                SymbolKind::FreeVariable => todo!(),
                SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
                    chunk.push(Stage1Instruction::GetGlobal(IState::Set(symbol.id)));
                }
                SymbolKind::Variable if symbol.scope == SymbolScope::Function => {
                    chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
                }
                SymbolKind::Variable if symbol.scope == SymbolScope::Free => {
                    chunk.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
                }
                SymbolKind::Parameter => {}
                SymbolKind::Function => {
                    chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
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
        chunk.push(Stage1Instruction::Call(callee, count as u8));
    }

    fn compile_operator(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
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
            "-" => Stage1Instruction::Sub,
            "=" => Stage1Instruction::Eq,
            "or" => Stage1Instruction::Or,
            _ => unreachable!("unknown operator: {}", operator),
        };

        let count = list.len() - 1;
        for spanned in list.iter().skip(1) {
            self.compile_expr(chunk, spanned);
        }
        for _ in 0..count - 1 {
            chunk.push(op.clone());
        }
    }

    fn compile_keyword(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>, name: &str) {
        match name {
            "fn" => self.compile_fn(spanned),
            "lambda" => self.compile_lambda(chunk, spanned),
            "var" => self.compile_var(chunk, spanned),
            "if" => self.compile_if(chunk, spanned),
            // "let", "if"
            _ => unreachable!("unknown keyword: {}", name),
        }
    }

    fn compile_if(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
        const CONDITION: usize = 1;
        const THEN: usize = 2;
        const ELSE: usize = 3;
        let Expr::List(list) = &spanned.expr else {
            unreachable!();
        };

        let Some(condition_spanned) = list.get(CONDITION) else {
            panic!("if requires condition");
        };

        // TODO: Not sure how to get the offset here.  Maybe pass a differ vec in to collect the
        // instructions to count for the offset to jump if the condition is false?
        self.compile_expr(chunk, condition_spanned);

        let Some(then_spanned) = list.get(THEN) else {
            panic!("if without then");
        };

        let mut then_chunk = Chunk::new();
        self.compile_expr(&mut then_chunk, then_spanned);
        let then_offset = then_chunk.size() + Stage1Instruction::Jump(0).size();
        // 1. jump to else if false 4
        // 2. then
        // 3. jump to end 5
        // 4. else
        // 5. end

        let mut else_chunk = Chunk::new();
        if let Some(else_spanned) = list.get(ELSE) {
            self.compile_expr(&mut else_chunk, else_spanned);
        };
        let else_offset = else_chunk.size();
        chunk.push(Stage1Instruction::JumpIf(then_offset));
        chunk.extend(then_chunk);
        chunk.push(Stage1Instruction::Jump(else_offset));
        chunk.extend(else_chunk);
    }

    fn compile_var(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
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
            self.compile_expr(chunk, expr);
        }

        let Some(symbol) = self.symbol_table.lookup(name) else {
            panic!("unknown symbol: {}", name);
        };

        let instruction = if symbol.scope == SymbolScope::Global {
            Stage1Instruction::LoadGlobal
        } else {
            Stage1Instruction::LoadLocal
        };
        chunk.push(instruction);
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

        let mut params = Chunk::new();
        for param in expr_params.iter() {
            let Expr::Symbol(_) = &param.expr else {
                panic!("expected symbol for param");
            };
            params.push(Stage1Instruction::Rot);
            params.push(Stage1Instruction::LoadLocal);
        }

        let mut body = Chunk::new();
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
            prelude: Chunk::new(),
            params,
            body,
        });
    }

    fn compile_lambda(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
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

        let mut body = Chunk::new();
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
                chunk.push(instruction);
                chunk.push(Stage1Instruction::LoadFree);
            }
        }

        body.push(Stage1Instruction::Rot);
        body.push(Stage1Instruction::Return);

        self.symbol_table.exit_scope();

        chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
            IState::Unset(name.to_string()),
        )));

        self.lambdas.push(Stage1Lambda {
            name: name.to_string(),
            params: Chunk::from(params),
            body: Chunk::from(body),
        });
    }
}
