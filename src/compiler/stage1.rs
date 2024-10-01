use crate::ast::{Expr, Spanned};
use crate::expr_walker::{
    AstWalker, CallExpr, FunctionExpr, IfElseExpr, LambdaExpr, LoopExpr, VarExpr,
};
use crate::symbol_table::{SymbolKind, SymbolScope, SymbolTable};
use crate::vm::Direction;

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
    Mul,
    Div,
    Eq,
    GreaterThan,
    LessThan,
    GreaterThanOrEqual,
    LessThanOrEqual,
    And,
    Or,
    Not,
    Mod,
    Rot,
    Call(Stage1Callee, u8),
    LoadLocal,
    GetLocal(IState),
    LoadGlobal,
    GetGlobal(IState),
    LoadFree,
    GetFree(IState),
    JumpIf(usize),
    Jump(Direction),
}

impl Stage1Instruction {
    pub fn size(&self) -> usize {
        match self {
            Self::Add
            | Self::Sub
            | Self::Mul
            | Self::Div
            | Self::Eq
            | Self::GreaterThan
            | Self::LessThan
            | Self::GreaterThanOrEqual
            | Self::LessThanOrEqual
            | Self::And
            | Self::Or
            | Self::Not
            | Self::Mod
            | Self::Rot
            | Self::Halt
            | Self::LoadGlobal
            | Self::LoadLocal
            | Self::LoadFree
            | Self::Return => 1,
            Self::Push(value) => 1 + value.size(),
            Self::Call(callee, _) => 1 + callee.size() + 1,
            Self::GetLocal(_) | Self::GetGlobal(_) | Self::GetFree(_) => 2,
            Self::JumpIf(_) => 5,
            Self::Jump(direction) => 1 + direction.size(),
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

#[allow(dead_code)]
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
pub struct Stage1Compiler<'a> {
    symbol_table: &'a mut SymbolTable,
    functions: Vec<Stage1Function>,
    lambdas: Vec<Stage1Lambda>,
    lambda_counter: usize,
}

impl<'a> Stage1Compiler<'a> {
    pub fn new(symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            functions: Vec::new(),
            lambdas: Vec::new(),
            lambda_counter: 0,
        }
    }

    #[allow(dead_code)]
    pub fn compile_to_chunk(&mut self, ast: &[Spanned<Expr>]) -> Chunk {
        let mut chunk = Chunk::new();
        self.walk(&mut chunk, ast);
        chunk
    }

    pub fn compile(mut self, ast: &[Spanned<Expr>]) -> Stage1Data {
        let mut chunk = Chunk::new();
        self.walk(&mut chunk, ast);
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
}

impl<'a> AstWalker<Chunk> for Stage1Compiler<'a> {
    fn get_lambda_name(&mut self) -> String {
        let name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    fn handle_operator(&mut self, chunk: &mut Chunk, operator: &str, list: &[Spanned<Expr>]) {
        const ARGS: usize = 1;
        let Some(op) = get_operator_opcode(operator) else {
            // TODO: REPORT ERROR
            panic!("unknown operator: {}", operator);
        };

        let count = list.len() - 1;
        for spanned in list.iter().skip(ARGS) {
            self.walk_expr(chunk, spanned);
        }
        for _ in 0..count - 1 {
            chunk.push(op.clone());
        }
    }

    fn handle_builtin(&mut self, chunk: &mut Chunk, name: &str, spanned: &[Spanned<Expr>]) {
        const ARGS: usize = 1;
        for spanned in spanned.iter().skip(ARGS) {
            self.walk_expr(chunk, spanned);
        }
        chunk.push(Stage1Instruction::Call(
            Stage1Callee::Builtin(name.to_string()),
            (spanned.len() - 1) as u8,
        ));
    }

    fn handle_function(&mut self, _: &mut Chunk, function: &FunctionExpr) {
        let Expr::Symbol(name) = &function.name.expr else {
            // TODO: REPORT ERROR
            panic!(
                "expected symbol for function name but found {:?}",
                function.name.expr
            );
        };

        self.symbol_table.enter_scope(name);

        let Expr::List(expr_params) = &function.params.expr else {
            // TODO: REPORT ERROR
            panic!(
                "expected list for params but found {:?}",
                &function.params.expr
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
        self.walk_expr(&mut body, function.body);

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

    fn handle_lambda(&mut self, chunk: &mut Chunk, lambda: &LambdaExpr) {
        let name = self.get_lambda_name();

        let Expr::List(expr_params) = &lambda.params.expr else {
            // TODO: REPORT ERROR
            panic!(
                "expected list for params but found {:?}",
                lambda.params.expr
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
        self.symbol_table.enter_scope(&name);
        self.walk_expr(&mut body, lambda.body);
        self.symbol_table.exit_scope();

        let Some(local_scope) = self.symbol_table.get_function_scope(&name) else {
            // TODO: REPORT ERROR
            panic!("no scope for {name:?}");
        };

        for (name, symbol) in local_scope.iter() {
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
            body,
        });
    }

    fn handle_call(&mut self, chunk: &mut Chunk, call: &CallExpr) {
        let arg_count = call.args.len() as u8;
        for arg in call.args.iter().rev() {
            self.walk_expr(chunk, arg);
        }
        self.walk_expr(chunk, call.callee);

        chunk.push(Stage1Instruction::Call(Stage1Callee::Function, arg_count));
    }

    fn handle_if_else(&mut self, chunk: &mut Chunk, if_else: &IfElseExpr) {
        self.walk_expr(chunk, if_else.condition);

        let mut then_chunk = Chunk::new();
        self.walk_expr(&mut then_chunk, if_else.then);
        let then_offset = then_chunk.size() + Stage1Instruction::Jump(Direction::Forward(0)).size();

        let mut else_chunk = Chunk::new();
        if let Some(else_spanned) = if_else.otherwise.as_ref() {
            self.walk_expr(&mut else_chunk, else_spanned);
        };
        let else_offset = else_chunk.size();
        chunk.push(Stage1Instruction::JumpIf(then_offset));
        chunk.extend(then_chunk);
        chunk.push(Stage1Instruction::Jump(Direction::Forward(else_offset)));
        chunk.extend(else_chunk);
    }

    fn handle_var(&mut self, chunk: &mut Chunk, var: &VarExpr) {
        let Expr::Symbol(name) = &var.name.expr else {
            // TODO: REPORT ERROR
            panic!("var name must be a symbol but found {:?}", var.name.expr);
        };

        self.walk_expr(chunk, var.body);

        let Some(symbol) = self.symbol_table.lookup(name) else {
            // TODO: REPORT ERROR
            panic!("unknown symbol: {}", name);
        };

        let instruction = if symbol.scope == SymbolScope::Global {
            Stage1Instruction::LoadGlobal
        } else {
            Stage1Instruction::LoadLocal
        };

        chunk.push(instruction);
    }

    fn handle_loop(&mut self, chunk: &mut Chunk, r#loop: &LoopExpr) {
        let mut chunk_condition = Chunk::new();
        self.walk_expr(&mut chunk_condition, r#loop.condition);

        let mut chunk_body = Chunk::new();
        self.walk_expr(&mut chunk_body, r#loop.body);

        let body_offset = chunk_body.size();
        let start_offset = body_offset
            + chunk_condition.size()
            + Stage1Instruction::JumpIf(0).size()
            + Stage1Instruction::Jump(Direction::Backward(0)).size();
        chunk.extend(chunk_condition);
        chunk.push(Stage1Instruction::JumpIf(body_offset));
        chunk.extend(chunk_body);
        chunk.push(Stage1Instruction::Jump(Direction::Backward(start_offset)));
    }

    fn handle_symbol(&mut self, chunk: &mut Chunk, name: &str) {
        let Some(symbol) = self.symbol_table.lookup(name) else {
            // TODO: REPORT ERROR
            panic!("Variable `{}` is not defined", name,);
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
            SymbolKind::Function => {
                chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
                    IState::Unset(name.to_string()),
                )));
            }
            // SymbolKind::Test => todo!("Test {}", name),
            SymbolKind::Lambda => todo!("Lambda {}", name),
            SymbolKind::FreeVariable => {
                chunk.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
            }
        }
    }

    fn handle_bool(&mut self, chunk: &mut Chunk, value: bool) {
        chunk.push(Stage1Instruction::Push(Stage1Value::Bool(value)));
    }

    fn handle_string(&mut self, chunk: &mut Chunk, value: &str) {
        chunk.push(Stage1Instruction::Push(Stage1Value::String(
            value.to_string(),
        )));
    }

    fn handle_number(&mut self, chunk: &mut Chunk, value: f64) {
        chunk.push(Stage1Instruction::Push(Stage1Value::F64(value)));
    }
}

fn get_operator_opcode(op: &str) -> Option<Stage1Instruction> {
    // NOTE: When implementing a new operator if this step is skipped the compiler will crash
    // here reminding you to add the new operator to this list. ONLY if you added the operator
    // to the OPERATORS list in main.rs
    match op {
        "+" => Some(Stage1Instruction::Add),
        "-" => Some(Stage1Instruction::Sub),
        "*" => Some(Stage1Instruction::Mul),
        "/" => Some(Stage1Instruction::Div),
        "=" => Some(Stage1Instruction::Eq),
        ">" => Some(Stage1Instruction::GreaterThan),
        "<" => Some(Stage1Instruction::LessThan),
        ">=" => Some(Stage1Instruction::GreaterThanOrEqual),
        "<=" => Some(Stage1Instruction::LessThanOrEqual),
        "and" => Some(Stage1Instruction::And),
        "or" => Some(Stage1Instruction::Or),
        "not" => Some(Stage1Instruction::Not),
        "mod" => Some(Stage1Instruction::Mod),
        _ => None,
    }
}

// #[derive(Debug)]
// pub struct Stage1Compiler {
//     pub symbol_table: SymbolTable,
//     pub functions: Vec<Stage1Function>,
//     pub lambdas: Vec<Stage1Lambda>,
//     lambda_counter: usize,
// }
//
// impl Stage1Compiler {
//     pub fn new(symbol_table: SymbolTable) -> Self {
//         Self {
//             symbol_table,
//             functions: Vec::new(),
//             lambdas: Vec::new(),
//             lambda_counter: 0,
//         }
//     }
//
//     fn get_lambda_name(&mut self) -> String {
//         let name = format!("lambda_{}", self.lambda_counter);
//         self.lambda_counter += 1;
//         name
//     }
//
//     pub fn compiler(mut self, ast: &[Spanned<Expr>]) -> Stage1Data {
//         let mut chunk = Chunk::new();
//         for expr in ast {
//             self.compile_expr(&mut chunk, expr);
//         }
//         for function in self.functions.iter_mut() {
//             if function.name == "main" {
//                 function.prelude.extend(chunk);
//                 break;
//             }
//         }
//
//         Stage1Data {
//             functions: self.functions,
//             lambdas: self.lambdas,
//         }
//     }
//
//     fn compile_expr(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         match &spanned.expr {
//             Expr::Bool(value) => chunk.push(Stage1Instruction::Push(Stage1Value::Bool(*value))),
//             Expr::String(value) => chunk.push(Stage1Instruction::Push(Stage1Value::String(
//                 value.to_string(),
//             ))),
//             Expr::Number(value) => chunk.push(Stage1Instruction::Push(Stage1Value::F64(*value))),
//             Expr::Symbol(name) => {
//                 let Some(symbol) = self.symbol_table.lookup(name) else {
//                     panic!("Variable `{}` is not defined", name);
//                 };
//
//                 match symbol.kind {
//                     SymbolKind::Parameter => {
//                         chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
//                     }
//                     SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
//                         chunk.push(Stage1Instruction::GetGlobal(IState::Set(symbol.id)));
//                     }
//                     SymbolKind::Variable if symbol.scope == SymbolScope::Function => {
//                         chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
//                     }
//                     SymbolKind::Variable => todo!("Variable: {}, id: {}", name, symbol.id),
//                     SymbolKind::Function => {
//                         chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
//                             IState::Unset(name.clone()),
//                         )));
//                     }
//                     // SymbolKind::Test => todo!("Test {}", name),
//                     SymbolKind::Lambda => todo!("Lambda {}", name),
//                     SymbolKind::FreeVariable => {
//                         chunk.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
//                     }
//                 }
//             }
//             Expr::List(_) => self.compile_list(chunk, spanned),
//         }
//     }
//
//     fn compile_list(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//
//         let Some(first) = list.first() else {
//             todo!("empty list {:#?}", spanned);
//         };
//
//         match &first.expr {
//             Expr::Bool(_) => todo!(),
//             Expr::String(_) => todo!(),
//             Expr::Symbol(name) if KEYWORDS.contains(&name.as_str()) => {
//                 self.compile_keyword(chunk, spanned, name)
//             }
//             Expr::Symbol(name) if OPERATORS.contains(&name.as_str()) => {
//                 self.compile_operator(chunk, spanned)
//             }
//             Expr::Symbol(_) => {
//                 self.compile_symbol_call(chunk, spanned);
//             }
//             Expr::Number(_) => todo!(),
//             Expr::List(_) => {
//                 let mut count = 0;
//                 for item in list.iter().skip(1) {
//                     count += 1;
//                     self.compile_expr(chunk, item);
//                 }
//                 self.compile_list(chunk, first);
//                 chunk.push(Stage1Instruction::Call(Stage1Callee::Function, count));
//             }
//         }
//     }
//
//     fn compile_symbol_call(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//
//         let Some(name_spanned) = list.first() else {
//             unreachable!();
//         };
//
//         let Expr::Symbol(name) = &name_spanned.expr else {
//             unreachable!();
//         };
//
//         if let Some(symbol) = self.symbol_table.lookup(name) {
//             if symbol.kind == SymbolKind::Parameter {
//                 chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
//             }
//         } else if !BUILTINS.contains(&name.as_str()) {
//             panic!("{} is not defined", name);
//         };
//
//         let count = list.len() - 1;
//         for spanned in list.iter().skip(1) {
//             self.compile_expr(chunk, spanned);
//         }
//
//         if let Some(symbol) = self.symbol_table.lookup(name) {
//             match symbol.kind {
//                 SymbolKind::FreeVariable => todo!(),
//                 SymbolKind::Variable if symbol.scope == SymbolScope::Global => {
//                     chunk.push(Stage1Instruction::GetGlobal(IState::Set(symbol.id)));
//                 }
//                 SymbolKind::Variable if symbol.scope == SymbolScope::Function => {
//                     chunk.push(Stage1Instruction::GetLocal(IState::Set(symbol.id)));
//                 }
//                 SymbolKind::Variable if symbol.scope == SymbolScope::Free => {
//                     chunk.push(Stage1Instruction::GetFree(IState::Set(symbol.id)));
//                 }
//                 SymbolKind::Parameter => {}
//                 SymbolKind::Function => {
//                     chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
//                         IState::Unset(name.clone()),
//                     )));
//                 }
//                 // SymbolKind::Test => todo!(),
//                 SymbolKind::Lambda => todo!(),
//                 _ => todo!(),
//             }
//         };
//
//         let callee = if BUILTINS.contains(&name.as_str()) {
//             Stage1Callee::Builtin(name.clone())
//         } else {
//             Stage1Callee::Function
//         };
//         chunk.push(Stage1Instruction::Call(callee, count as u8));
//     }
//
//     fn compile_operator(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//         let Some(operator_spanned) = list.first() else {
//             unreachable!();
//         };
//         let Expr::Symbol(operator) = &operator_spanned.expr else {
//             unreachable!();
//         };
//
//         let op = get_operator_opcode(operator.as_str()).expect("unknown operator");
//
//         let count = list.len() - 1;
//         for spanned in list.iter().skip(1) {
//             self.compile_expr(chunk, spanned);
//         }
//         for _ in 0..count - 1 {
//             chunk.push(op.clone());
//         }
//     }
//
//     fn compile_keyword(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>, name: &str) {
//         match name {
//             "fn" => self.compile_fn(spanned),
//             "lambda" => self.compile_lambda(chunk, spanned),
//             "var" => self.compile_var(chunk, spanned),
//             "if" => self.compile_if(chunk, spanned),
//             "loop" => self.compile_loop(chunk, spanned),
//             // "let"
//             _ => unreachable!("unknown keyword: {}", name),
//         }
//     }
//
//     fn compile_loop(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         const CONDITION: usize = 1;
//         const BODY: usize = 2;
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//
//         let Some(condition_spanned) = list.get(CONDITION) else {
//             panic!("loop requires condition");
//         };
//
//         let mut chunk_condition = Chunk::new();
//         self.compile_expr(&mut chunk_condition, condition_spanned);
//         eprintln!("{:?}\n{:#?}", chunk_condition, condition_spanned);
//
//         let Some(body_spanned) = list.get(BODY) else {
//             panic!("loop without body");
//         };
//
//         let mut body_chunk = Chunk::new();
//         self.compile_expr(&mut body_chunk, body_spanned);
//
//         let body_offset = body_chunk.size();
//         let start_offset = body_offset
//             + chunk_condition.size()
//             + Stage1Instruction::JumpIf(0).size()
//             + Stage1Instruction::Jump(Direction::Backward(0)).size();
//         chunk.extend(chunk_condition);
//         chunk.push(Stage1Instruction::JumpIf(body_offset));
//         chunk.extend(body_chunk);
//         chunk.push(Stage1Instruction::Jump(Direction::Backward(start_offset)));
//     }
//
//     fn compile_if(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         const CONDITION: usize = 1;
//         const THEN: usize = 2;
//         const ELSE: usize = 3;
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//
//         let Some(condition_spanned) = list.get(CONDITION) else {
//             panic!("if requires condition");
//         };
//
//         // TODO: Not sure how to get the offset here.  Maybe pass a differ vec in to collect the
//         // instructions to count for the offset to jump if the condition is false?
//         self.compile_expr(chunk, condition_spanned);
//
//         let Some(then_spanned) = list.get(THEN) else {
//             panic!("if without then");
//         };
//
//         let mut then_chunk = Chunk::new();
//         self.compile_expr(&mut then_chunk, then_spanned);
//         let then_offset = then_chunk.size() + Stage1Instruction::Jump(Direction::Forward(0)).size();
//         // 1. jump to else if false 4
//         // 2. then
//         // 3. jump to end 5
//         // 4. else
//         // 5. end
//
//         let mut else_chunk = Chunk::new();
//         if let Some(else_spanned) = list.get(ELSE) {
//             self.compile_expr(&mut else_chunk, else_spanned);
//         };
//         let else_offset = else_chunk.size();
//         chunk.push(Stage1Instruction::JumpIf(then_offset));
//         chunk.extend(then_chunk);
//         chunk.push(Stage1Instruction::Jump(Direction::Forward(else_offset)));
//         chunk.extend(else_chunk);
//     }
//
//     fn compile_var(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//         let Some(name_spanned) = list.get(1) else {
//             unreachable!();
//         };
//         let Expr::Symbol(name) = &name_spanned.expr else {
//             unreachable!();
//         };
//
//         for expr in list.iter().skip(2) {
//             self.compile_expr(chunk, expr);
//         }
//
//         let Some(symbol) = self.symbol_table.lookup(name) else {
//             panic!("unknown symbol: {}", name);
//         };
//
//         let instruction = if symbol.scope == SymbolScope::Global {
//             Stage1Instruction::LoadGlobal
//         } else {
//             Stage1Instruction::LoadLocal
//         };
//         chunk.push(instruction);
//     }
//
//     fn compile_fn(&mut self, spanned: &Spanned<Expr>) {
//         const NAME: usize = 1;
//         const PARAMS: usize = 2;
//         const BODY: usize = 3;
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//
//         debug_assert!(list.len() == 4, "(fn <name> (<params>) ( <body> ))");
//
//         let Some(name_spanned) = list.get(NAME) else {
//             unreachable!();
//         };
//
//         let Expr::Symbol(name) = &name_spanned.expr else {
//             unreachable!();
//         };
//
//         self.symbol_table.enter_scope(name);
//
//         let Some(params_spanned) = &list.get(PARAMS) else {
//             panic!("expected list for params");
//         };
//
//         let Expr::List(expr_params) = &params_spanned.expr else {
//             panic!(
//                 "expected list for params but found {:?}",
//                 params_spanned.expr
//             );
//         };
//
//         let mut params = Chunk::new();
//         for param in expr_params.iter() {
//             let Expr::Symbol(_) = &param.expr else {
//                 panic!("expected symbol for param");
//             };
//             params.push(Stage1Instruction::Rot);
//             params.push(Stage1Instruction::LoadLocal);
//         }
//
//         let mut body = Chunk::new();
//         for expr in list.iter().skip(BODY) {
//             self.compile_expr(&mut body, expr);
//         }
//
//         if name == "main" {
//             body.push(Stage1Instruction::Halt);
//         } else {
//             body.push(Stage1Instruction::Rot);
//             body.push(Stage1Instruction::Return);
//         }
//
//         self.symbol_table.exit_scope();
//
//         self.functions.push(Stage1Function {
//             name: name.to_string(),
//             prelude: Chunk::new(),
//             params,
//             body,
//         });
//     }
//
//     fn compile_lambda(&mut self, chunk: &mut Chunk, spanned: &Spanned<Expr>) {
//         const PARAMS: usize = 1;
//         const BODY: usize = 2;
//         let Expr::List(list) = &spanned.expr else {
//             unreachable!();
//         };
//         debug_assert!(list.len() == 3, "(lambda (<params>) ( <body> ))");
//
//         let name = self.get_lambda_name();
//
//         self.symbol_table.enter_scope(&name);
//
//         let Some(params_spanned) = &list.get(PARAMS) else {
//             panic!("expected list for params");
//         };
//
//         let Expr::List(expr_params) = &params_spanned.expr else {
//             panic!(
//                 "expected list for params but found {:?}",
//                 params_spanned.expr
//             );
//         };
//
//         let mut params = Vec::new();
//         for param in expr_params.iter() {
//             let Expr::Symbol(_) = &param.expr else {
//                 panic!("expected symbol for param");
//             };
//             params.push(Stage1Instruction::Rot);
//             params.push(Stage1Instruction::LoadLocal);
//         }
//
//         let mut body = Chunk::new();
//         for expr in list.iter().skip(BODY) {
//             self.compile_expr(&mut body, expr);
//         }
//
//         let Some(everything) = self.symbol_table.get_function_scope(&name) else {
//             panic!("no scope for {name:?}");
//         };
//         for (name, symbol) in everything.iter() {
//             if symbol.kind == SymbolKind::FreeVariable {
//                 let instruction = if symbol.scope == SymbolScope::Global {
//                     Stage1Instruction::GetGlobal(IState::Unset(name.to_string()))
//                 } else {
//                     Stage1Instruction::GetLocal(IState::Unset(name.to_string()))
//                 };
//                 chunk.push(instruction);
//                 chunk.push(Stage1Instruction::LoadFree);
//             }
//         }
//
//         body.push(Stage1Instruction::Rot);
//         body.push(Stage1Instruction::Return);
//
//         self.symbol_table.exit_scope();
//
//         chunk.push(Stage1Instruction::Push(Stage1Value::Callable(
//             IState::Unset(name.to_string()),
//         )));
//
//         self.lambdas.push(Stage1Lambda {
//             name: name.to_string(),
//             params: Chunk::from(params),
//             body,
//         });
//     }
// }
//
// fn get_operator_opcode(op: &str) -> Option<Stage1Instruction> {
//     // NOTE: When implementing a new operator if this step is skipped the compiler will crash
//     // here reminding you to add the new operator to this list. ONLY if you added the operator
//     // to the OPERATORS list in main.rs
//     match op {
//         "+" => Some(Stage1Instruction::Add),
//         "-" => Some(Stage1Instruction::Sub),
//         "*" => Some(Stage1Instruction::Mul),
//         "/" => Some(Stage1Instruction::Div),
//         "=" => Some(Stage1Instruction::Eq),
//         ">" => Some(Stage1Instruction::GreaterThan),
//         "<" => Some(Stage1Instruction::LessThan),
//         ">=" => Some(Stage1Instruction::GreaterThanOrEqual),
//         "<=" => Some(Stage1Instruction::LessThanOrEqual),
//         "and" => Some(Stage1Instruction::And),
//         "or" => Some(Stage1Instruction::Or),
//         "not" => Some(Stage1Instruction::Not),
//         "mod" => Some(Stage1Instruction::Mod),
//         _ => None,
//     }
// }

#[cfg(test)]
mod tests {
    use super::{Stage1Instruction::*, *};
    use crate::parser::parser;
    use crate::symbol_table::SymbolTableBuilder;
    use crate::vm::Direction;
    use chumsky::prelude::Parser;
    use pretty_assertions::assert_eq;
    #[test]
    fn test_loop() {
        let src = r#"
(fn main () (loop true (print "Hello World!\n")))
"#;
        let ast = parser().parse(src).unwrap();
        let mut symbol_table = SymbolTableBuilder::default().build(&ast);
        let stage1_compiler = Stage1Compiler::new(&mut symbol_table).compile(&ast);
        assert_eq!(stage1_compiler.functions.len(), 1, "one function");
        let main = &stage1_compiler.functions[0];
        assert_eq!(main.name, "main", "main function name");
        assert_eq!(main.params, Chunk::new(), "main function params");
        assert_eq!(main.prelude, Chunk::new(), "main function prelude");
        assert_eq!(
            main.body,
            Chunk::from(vec![
                Push(Stage1Value::Bool(true)),
                JumpIf(28),
                Push(Stage1Value::String("Hello World!\n".to_string())),
                Call(Stage1Callee::Builtin("print".to_string()), 1),
                Jump(Direction::Backward(42)),
                Halt,
            ]),
            "main function body"
        );
    }
}
