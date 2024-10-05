use super::{
    ast::{Expr, Spanned},
    expr_walker::{AstWalker, CallExpr, FunctionExpr, IfElseExpr, LoopExpr, OperatorExpr, VarExpr},
    parser::parser,
    symbol_table::{Symbol, SymbolTable, SymbolTableBuilder},
    BUILTINS,
};
use anyhow::{anyhow, Result};
use chumsky::prelude::Parser;
use std::{ops::Range, usize};

#[derive(Debug, Clone, PartialEq)]
pub enum OpCode {
    Halt,
    Return,
    Push,
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
    CallFunction,
    CallBuiltin,
    SetLocal,
    SetGlobal,
    SetFree,
    GetLocal,
    GetGlobal,
    GetFree,
    JumpIf,
    JumpForward,
    JumpBackward,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    Halt,
    Return,
    Push(Box<Value>),
    Add(usize),
    Sub(usize),
    Mul(usize),
    Div(usize),
    Eq(usize),
    GreaterThan(usize),
    LessThan(usize),
    GreaterThanOrEqual(usize),
    LessThanOrEqual(usize),
    And(usize),
    Or(usize),
    Not,
    Mod,
    Rot,
    CallFunction(usize, usize),
    CallBuiltin(usize, usize),
    SetLocal,
    SetGlobal,
    SetFree,
    GetLocal(usize),
    GetGlobal(usize),
    GetFree(usize),
    JumpIf(usize),
    JumpForward(usize),
    JumpBackward(usize),
}

impl Instruction {
    pub fn size(&self) -> usize {
        match self {
            Self::Halt => 1,
            Self::Return => 1,
            Self::Push(value) => 1 + value.size(),
            Self::Add(_) => 2,
            Self::Sub(_) => 2,
            Self::Mul(_) => 2,
            Self::Div(_) => 2,
            Self::Eq(_) => 2,
            Self::GreaterThan(_) => 2,
            Self::LessThan(_) => 2,
            Self::GreaterThanOrEqual(_) => 2,
            Self::LessThanOrEqual(_) => 2,
            Self::And(_) => 2,
            Self::Or(_) => 2,
            Self::Not => 1,
            Self::Mod => 1,
            Self::Rot => 1,
            Self::CallFunction(_, _) => 9,
            Self::CallBuiltin(_, _) => 9,
            Self::SetLocal => 1,
            Self::SetGlobal => 1,
            Self::SetFree => 1,
            Self::GetLocal(_) => 5,
            Self::GetGlobal(_) => 5,
            Self::GetFree(_) => 5,
            Self::JumpIf(_) => 5,
            Self::JumpForward(_) => 5,
            Self::JumpBackward(_) => 5,
        }
    }

    pub fn opcode(&self) -> OpCode {
        match self {
            Self::Halt => OpCode::Halt,
            Self::Return => OpCode::Return,
            Self::Push(_) => OpCode::Push,
            Self::Add(_) => OpCode::Add,
            Self::Sub(_) => OpCode::Sub,
            Self::Mul(_) => OpCode::Mul,
            Self::Div(_) => OpCode::Div,
            Self::Eq(_) => OpCode::Eq,
            Self::GreaterThan(_) => OpCode::GreaterThan,
            Self::LessThan(_) => OpCode::LessThan,
            Self::GreaterThanOrEqual(_) => OpCode::GreaterThanOrEqual,
            Self::LessThanOrEqual(_) => OpCode::LessThanOrEqual,
            Self::And(_) => OpCode::And,
            Self::Or(_) => OpCode::Or,
            Self::Not => OpCode::Not,
            Self::Mod => OpCode::Mod,
            Self::Rot => OpCode::Rot,
            Self::CallFunction(..) => OpCode::CallFunction,
            Self::CallBuiltin(..) => OpCode::CallBuiltin,
            Self::SetLocal => OpCode::SetLocal,
            Self::SetGlobal => OpCode::SetGlobal,
            Self::SetFree => OpCode::SetFree,
            Self::GetLocal(_) => OpCode::GetLocal,
            Self::GetGlobal(_) => OpCode::GetGlobal,
            Self::GetFree(_) => OpCode::GetFree,
            Self::JumpIf(_) => OpCode::JumpIf,
            Self::JumpForward(_) => OpCode::JumpForward,
            Self::JumpBackward(_) => OpCode::JumpBackward,
        }
    }

    pub fn to_bytecode(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        match self {
            Self::Halt => bytes.push(OpCode::Halt as u8),
            Self::Return => bytes.push(OpCode::Return as u8),
            Self::Push(value) => {
                bytes.push(OpCode::Push as u8);
                bytes.extend(&value.to_bytecode());
            }
            Self::Add(count)
            | Self::Sub(count)
            | Self::Mul(count)
            | Self::Div(count)
            | Self::Eq(count)
            | Self::GreaterThan(count)
            | Self::LessThan(count)
            | Self::GreaterThanOrEqual(count)
            | Self::LessThanOrEqual(count)
            | Self::And(count)
            | Self::Or(count) => {
                bytes.push(self.opcode() as u8);
                bytes.push(*count as u8);
            }
            Self::Not
            | Self::Mod
            | Self::Rot
            | Self::SetLocal
            | Self::SetGlobal
            | Self::SetFree => bytes.push(self.opcode() as u8),
            Self::CallFunction(address, count) | Self::CallBuiltin(address, count) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&(*address as u32).to_le_bytes());
                bytes.extend(&(*count as u32).to_le_bytes());
            }

            Self::GetLocal(address)
            | Self::GetGlobal(address)
            | Self::GetFree(address)
            | Self::JumpIf(address)
            | Self::JumpForward(address)
            | Self::JumpBackward(address) => {
                bytes.push(self.opcode() as u8);
                bytes.extend(&(*address as u32).to_le_bytes());
            }
        }

        bytes
    }
}

/*


(fn add (a b) (+ a b))

LoadLocal a
LoadLocal b
GetLocal 0
GetLocal 1
Add
Return

DefineGlobal add 0 0..6


*/

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    U8(u8),
    I32(i32),
    U32(u32),
    F32(f32),
    F64(f64),
    String(Box<String>),
    Bool(bool),
    List(Box<Vec<Value>>),
    Callable(Box<Range<usize>>),
}

impl Value {
    pub const CODE_U8: u8 = 0x00;
    pub const CODE_I32: u8 = 0x01;
    pub const CODE_U32: u8 = 0x02;
    pub const CODE_F32: u8 = 0x03;
    pub const CODE_F64: u8 = 0x04;
    pub const CODE_STRING: u8 = 0x05;
    pub const CODE_BOOL: u8 = 0x06;
    pub const CODE_LIST: u8 = 0x07;
    pub const CODE_CALLABLE: u8 = 0x08;

    pub fn to_bytecode(&self) -> Vec<u8> {
        match self {
            Value::U8(v) => vec![Self::CODE_U8, *v],
            Value::I32(v) => {
                let mut bytes = vec![Self::CODE_I32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::U32(v) => {
                let mut bytes = vec![Self::CODE_U32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F32(v) => {
                let mut bytes = vec![Self::CODE_F32];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::F64(v) => {
                let mut bytes = vec![Self::CODE_F64];
                bytes.extend_from_slice(&v.to_le_bytes());
                bytes
            }
            Value::String(v) => {
                let mut bytes = vec![Self::CODE_STRING];
                bytes.extend_from_slice(&(v.len() as u32).to_le_bytes());
                bytes.extend_from_slice(v.as_bytes());
                bytes
            }
            Value::Bool(v) => vec![Self::CODE_BOOL, if *v { 1 } else { 0 }],
            Value::List(vec) => {
                let mut bytes = vec![Self::CODE_LIST];
                bytes.extend_from_slice(&(vec.len() as u32).to_le_bytes());
                for v in vec.iter() {
                    bytes.extend(v.to_bytecode());
                }
                bytes
            }
            Value::Callable(range) => {
                let mut bytes = vec![Self::CODE_CALLABLE];
                bytes.extend_from_slice(&(range.start as u32).to_le_bytes());
                bytes.extend_from_slice(&(range.end as u32).to_le_bytes());
                bytes
            }
        }
    }

    pub fn size(&self) -> usize {
        let conent_size = match self {
            Value::U8(_) => 1,
            Value::I32(_) => 4,
            Value::U32(_) => 4,
            Value::F32(_) => 4,
            Value::F64(_) => 8,
            Value::String(v) => 4 + v.len(),
            Value::Bool(_) => 1,
            Value::List(vec) => 4 + vec.iter().map(|v| v.size()).sum::<usize>(),
            // 4 bytes    4 bytes
            // start......end
            Value::Callable(_) => 8,
        };
        // opcode + content
        1 + conent_size
    }

    pub fn type_of(&self) -> String {
        match self {
            Self::U8(_) => "u8".to_string(),
            Self::I32(_) => "i32".to_string(),
            Self::U32(_) => "u32".to_string(),
            Self::F32(_) => "f32".to_string(),
            Self::F64(_) => "f64".to_string(),
            Self::String(_) => "String".to_string(),
            Self::Bool(_) => "Bool".to_string(),
            Self::List(_) => "List".to_string(),
            Self::Callable(_) => "Function".to_string(),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::U8(value) => write!(f, "{value}"),
            Self::I32(value) => write!(f, "{value}"),
            Self::U32(value) => write!(f, "{value}"),
            Self::F32(value) => write!(f, "{value}"),
            Self::F64(value) => write!(f, "{value}"),
            Self::String(value) => write!(f, "{value}"),
            Self::Bool(value) => write!(f, "{value}"),
            Self::List(value) => {
                write!(
                    f,
                    "({})",
                    value
                        .iter()
                        .map(|v| format!("{}", v))
                        .collect::<Vec<String>>()
                        .join(" ")
                )
            }
            Self::Callable(index) => write!(f, "<function {index:?}>"),
        }
    }
}

pub fn compile(src: &str) -> Result<Vec<Instruction>> {
    let ast = parser()
        .parse(src)
        .map_err(|e| anyhow::anyhow!(e.iter().map(|e| e.to_string() + "\n").collect::<String>()))?;

    let mut symbol_table = SymbolTableBuilder::default().build(&ast);
    Compiler::new(&mut symbol_table).compile(&ast)
}

type Program = Vec<Instruction>;

#[derive(Debug)]
pub struct Compiler<'a> {
    symbol_table: &'a mut SymbolTable,
    lambda_counter: usize,
}

impl<'a> Compiler<'a> {
    fn new(symbol_table: &'a mut SymbolTable) -> Self {
        Self {
            symbol_table,
            lambda_counter: 0,
        }
    }

    fn compile(&mut self, ast: &[Spanned<Expr>]) -> Result<Vec<Instruction>> {
        let mut program = Vec::new();
        self.walk(&mut program, ast);
        Ok(program)
    }

    fn get_operator_opcode(op: &str, count: usize) -> Option<Instruction> {
        // NOTE: When implementing a new operator if this step is skipped the compiler will crash
        // here reminding you to add the new operator to this list. ONLY if you added the operator
        // to the OPERATORS list in lib.rs
        match op {
            "+" => Some(Instruction::Add(count)),
            "-" => Some(Instruction::Sub(count)),
            "*" => Some(Instruction::Mul(count)),
            "/" => Some(Instruction::Div(count)),
            "=" => Some(Instruction::Eq(count)),
            ">" => Some(Instruction::GreaterThan(count)),
            "<" => Some(Instruction::LessThan(count)),
            ">=" => Some(Instruction::GreaterThanOrEqual(count)),
            "<=" => Some(Instruction::LessThanOrEqual(count)),
            "and" => Some(Instruction::And(count)),
            "or" => Some(Instruction::Or(count)),
            "not" => Some(Instruction::Not),
            "mod" => Some(Instruction::Mod),
            _ => None,
        }
    }

    fn emit_set_instruction(program: &mut Program, symbol: &Symbol) {
        match symbol.kind {
            crate::symbol_table::SymbolKind::FreeVariable => program.push(Instruction::SetFree),
            crate::symbol_table::SymbolKind::Variable => match symbol.scope {
                crate::symbol_table::SymbolScope::Global => program.push(Instruction::SetGlobal),
                crate::symbol_table::SymbolScope::Function => program.push(Instruction::SetLocal),
                crate::symbol_table::SymbolScope::Free => program.push(Instruction::SetFree),
            },
            crate::symbol_table::SymbolKind::Parameter => program.push(Instruction::SetLocal),
            crate::symbol_table::SymbolKind::Function => todo!(),
            crate::symbol_table::SymbolKind::Lambda => todo!(),
        }
    }

    fn emit_get_instruction(program: &mut Program, symbol: &Symbol) {
        let id = symbol.id;
        match symbol.kind {
            crate::symbol_table::SymbolKind::FreeVariable => program.push(Instruction::GetFree(id)),
            crate::symbol_table::SymbolKind::Variable => match symbol.scope {
                crate::symbol_table::SymbolScope::Global => {
                    program.push(Instruction::GetGlobal(id))
                }
                crate::symbol_table::SymbolScope::Function => {
                    program.push(Instruction::GetLocal(id))
                }
                crate::symbol_table::SymbolScope::Free => program.push(Instruction::GetFree(id)),
            },
            crate::symbol_table::SymbolKind::Parameter => program.push(Instruction::GetLocal(id)),
            crate::symbol_table::SymbolKind::Function => todo!(),
            crate::symbol_table::SymbolKind::Lambda => todo!(),
        }
    }
}

impl AstWalker<Program> for Compiler<'_> {
    fn get_lambda_name(&mut self) -> String {
        let name = format!("lambda_{}", self.lambda_counter);
        self.lambda_counter += 1;
        name
    }

    fn handle_operator(
        &mut self,
        program: &mut Program,
        operator_name: &str,
        oper_expr: &OperatorExpr,
    ) {
        for arg in oper_expr.args.iter() {
            self.walk_expr(program, arg);
        }
        let Some(instruction) = Self::get_operator_opcode(operator_name, oper_expr.args.len())
        else {
            // TODO: ERROR REPORTING
            panic!("Unknown operator: {operator_name}");
        };

        program.push(instruction);
    }

    fn handle_builtin(&mut self, program: &mut Program, name: &str, args: &[Spanned<Expr>]) {
        const ARGS: usize = 1;
        for arg in args.iter().skip(ARGS) {
            self.walk_expr(program, arg);
        }
        let Some(_id) = BUILTINS.iter().position(|b| b == &name) else {
            // TODO: ERROR REPORTING
            panic!("Unknown builtin: {name}");
        };
        program.push(Instruction::CallBuiltin(0, args.len() - ARGS));
    }

    fn handle_function(&mut self, program: &mut Program, function: &FunctionExpr) {
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

        let mut params = Vec::new();
        for param in expr_params.iter() {
            let Expr::Symbol(_) = &param.expr else {
                panic!("expected symbol for param");
            };
            params.push(Instruction::SetLocal);
        }

        let mut body = Vec::new();
        self.walk_expr(&mut body, function.body);

        if name == "main" {
            body.push(Instruction::Halt);
        } else {
            body.push(Instruction::Rot);
            body.push(Instruction::Return);
        }

        self.symbol_table.exit_scope();

        let start = program.iter().map(|i| i.size()).sum::<usize>();
        let end = start
            + params.iter().map(|i| i.size()).sum::<usize>()
            + body.iter().map(|i| i.size()).sum::<usize>();
        program.extend(params);
        program.extend(body);
        program.push(Instruction::Push(Box::new(Value::Callable(Box::new(
            start..end,
        )))));
        program.push(Instruction::SetGlobal);
    }

    fn handle_lambda(&mut self, _: &mut Program, _: &crate::expr_walker::LambdaExpr) {
        todo!()
    }

    fn handle_call(&mut self, program: &mut Program, call: &CallExpr) {
        for arg in call.args.iter() {
            self.walk_expr(program, arg);
        }
        program.push(Instruction::CallFunction(0, call.args.len()));
    }

    fn handle_var(&mut self, _: &mut Program, _: &VarExpr) {
        todo!()
    }

    fn handle_if_else(&mut self, _: &mut Program, _: &IfElseExpr) {
        todo!()
    }

    fn handle_loop(&mut self, _: &mut Program, _: &LoopExpr) {
        todo!()
    }

    fn handle_bool(&mut self, _: &mut Program, _: bool) {
        todo!()
    }

    fn handle_string(&mut self, _: &mut Program, _: &str) {
        todo!()
    }

    fn handle_number(&mut self, program: &mut Program, value: f64) {
        program.push(Instruction::Push(Box::new(Value::F64(value))));
    }

    fn handle_symbol(&mut self, program: &mut Program, name: &str) {
        let Some(symbol) = self.symbol_table.lookup(name) else {
            // TODO: REPORT ERROR
            panic!("Variable `{}` is not defined", name,);
        };
        Self::emit_set_instruction(program, symbol);
    }
}

const INSTRUCTION_CALL: [fn(&mut Machine) -> Result<()>; 28] = [
    Machine::instruction_halt,
    Machine::instruction_return,
    Machine::instruction_push,
    Machine::instruction_add,
    Machine::instruction_sub,
    Machine::instruction_mul,
    Machine::instruction_div,
    Machine::instruction_eq,
    Machine::instruction_greater_than,
    Machine::instruction_less_than,
    Machine::instruction_greater_than_or_equal,
    Machine::instruction_less_than_or_equal,
    Machine::instruction_and,
    Machine::instruction_or,
    Machine::instruction_not,
    Machine::instruction_mod,
    Machine::instruction_rot,
    Machine::instruction_call_function,
    Machine::instruction_call_builtin,
    Machine::instruction_load_local,
    Machine::instruction_get_local,
    Machine::instruction_load_global,
    Machine::instruction_get_global,
    Machine::instruction_load_free,
    Machine::instruction_get_free,
    Machine::instruction_jump_if,
    Machine::instruction_jump_forward,
    Machine::instruction_jump_backward,
];

const INTRISICS: [fn(&mut Machine, u8) -> Result<()>; 1] = [Intrinsic::intrinsic_print];

#[derive(Debug)]
struct Frame {
    pub return_address: Option<usize>,
    pub args: Vec<Value>,
    pub stack: Vec<Value>,
}

#[derive(Debug)]
pub struct Machine {
    program: Vec<u8>,
    global: Vec<Value>,
    free: Vec<Value>,
    params: Vec<Value>,
    stack: Vec<Frame>,
    ip: usize,
    is_running: bool,
    #[cfg(any(debug_assertions, test))]
    cycle_count: usize,
}

impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        Self {
            program,
            global: Vec::with_capacity(1024),
            free: Vec::with_capacity(1024),
            params: Vec::with_capacity(1024),
            stack: Vec::with_capacity(1024),
            ip: 0,
            is_running: true,
            #[cfg(any(debug_assertions, test))]
            cycle_count: 0,
        }
    }

    pub fn run_once(&mut self) -> Result<()> {
        #[cfg(any(debug_assertions, test))]
        {
            self.cycle_count += 1;
        }
        let opcode = self.get_u8()? as usize;
        INSTRUCTION_CALL[opcode](self)?;

        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        while self.is_running {
            self.run_once()?;
        }
        #[cfg(any(debug_assertions, test))]
        eprintln!("cycles: {}", self.cycle_count);
        #[cfg(any(debug_assertions, test))]
        eprintln!("stack {:#?}", self.stack);
        Ok(())
    }
}

// Helpers
impl Machine {
    fn get_value(&mut self) -> Result<Value> {
        let value_opcode = self.get_u8()?;
        match value_opcode {
            Value::CODE_U8 => {
                let value = self.get_u8()?;
                Ok(Value::U8(value))
            }
            Value::CODE_I32 => {
                let value = self.get_i32()?;
                Ok(Value::I32(value))
            }
            Value::CODE_U32 => {
                let value = self.get_u32()?;
                Ok(Value::U32(value))
            }
            Value::CODE_F32 => {
                let value = self.get_f32()?;
                Ok(Value::F32(value))
            }
            Value::CODE_F64 => {
                let value = self.get_f64()?;
                Ok(Value::F64(value))
            }
            Value::CODE_STRING => {
                let value = self.get_string()?;
                Ok(Value::String(Box::new(value)))
            }
            Value::CODE_BOOL => {
                let value = self.get_u8()? != 0;
                Ok(Value::Bool(value))
            }
            Value::CODE_LIST => {
                let length = self.get_u32()?;
                let mut values = Vec::new();
                for _ in 0..length {
                    values.push(self.get_value()?);
                }
                Ok(Value::List(Box::new(values)))
            }
            Value::CODE_CALLABLE => {
                let start = self.get_u32()? as usize;
                let end = self.get_u32()? as usize;
                Ok(Value::Callable(Box::new(start..end)))
            }
            _ => Err(anyhow!("Unknown value `{}`", self.program[self.ip])),
        }
    }

    fn get_u8(&mut self) -> Result<u8> {
        let byte = self.program[self.ip];
        self.ip += 1;
        Ok(byte)
    }

    #[allow(dead_code)]
    fn get_u16(&mut self) -> Result<u16> {
        let byte2 = self.get_u8()? as u16;
        let byte1 = self.get_u8()? as u16;
        Ok(byte1 << 8 | byte2)
    }

    fn get_i32(&mut self) -> Result<i32> {
        let byte4 = self.get_u8()? as i32;
        let byte3 = self.get_u8()? as i32;
        let byte2 = self.get_u8()? as i32;
        let byte1 = self.get_u8()? as i32;
        Ok(byte1 << 24 | byte2 << 16 | byte3 << 8 | byte4)
    }

    fn get_u32(&mut self) -> Result<u32> {
        let byte4 = self.get_u8()? as u32;
        let byte3 = self.get_u8()? as u32;
        let byte2 = self.get_u8()? as u32;
        let byte1 = self.get_u8()? as u32;
        Ok(byte1 << 24 | byte2 << 16 | byte3 << 8 | byte4)
    }

    #[allow(dead_code)]
    fn get_u64(&mut self) -> Result<u64> {
        let byte8 = self.get_u8()? as u64;
        let byte7 = self.get_u8()? as u64;
        let byte6 = self.get_u8()? as u64;
        let byte5 = self.get_u8()? as u64;
        let byte4 = self.get_u8()? as u64;
        let byte3 = self.get_u8()? as u64;
        let byte2 = self.get_u8()? as u64;
        let byte1 = self.get_u8()? as u64;
        Ok(byte1 << 56
            | byte2 << 48
            | byte3 << 40
            | byte4 << 32
            | byte5 << 24
            | byte6 << 16
            | byte7 << 8
            | byte8)
    }

    fn get_f32(&mut self) -> Result<f32> {
        let byte1 = self.get_u8()? as u32;
        let byte2 = self.get_u8()? as u32;
        let byte3 = self.get_u8()? as u32;
        let byte4 = self.get_u8()? as u32;
        Ok(f32::from_le_bytes([
            byte1 as u8,
            byte2 as u8,
            byte3 as u8,
            byte4 as u8,
        ]))
    }

    fn get_f64(&mut self) -> Result<f64> {
        let byte1 = self.get_u8()? as u64;
        let byte2 = self.get_u8()? as u64;
        let byte3 = self.get_u8()? as u64;
        let byte4 = self.get_u8()? as u64;
        let byte5 = self.get_u8()? as u64;
        let byte6 = self.get_u8()? as u64;
        let byte7 = self.get_u8()? as u64;
        let byte8 = self.get_u8()? as u64;
        Ok(f64::from_le_bytes([
            byte1 as u8,
            byte2 as u8,
            byte3 as u8,
            byte4 as u8,
            byte5 as u8,
            byte6 as u8,
            byte7 as u8,
            byte8 as u8,
        ]))
    }

    fn get_string(&mut self) -> Result<String> {
        let len = self.get_u32()? as usize;
        let bytes = self.program[self.ip..self.ip + len].to_vec();
        self.ip += len;
        Ok(String::from_utf8(bytes)?)
    }

    fn get_current_frame(&self) -> Result<&Frame> {
        self.stack
            .last()
            .map_or_else(|| Err(anyhow!("Stack is empty")), Ok)
    }

    fn get_current_frame_mut(&mut self) -> Result<&mut Frame> {
        self.stack
            .last_mut()
            .map_or_else(|| Err(anyhow!("Stack is empty")), Ok)
    }
}

impl Machine {
    fn instruction_halt(&mut self) -> Result<()> {
        self.is_running = false;
        Ok(())
    }

    fn instruction_return(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_push(&mut self) -> Result<()> {
        let value = self.get_value()?;
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_add(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_sub(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_mul(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l * r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l * r);
                }
                _ => panic!("invalid types for Mul"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_div(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_eq(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_greater_than(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_less_than(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_greater_than_or_equal(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_less_than_or_equal(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_and(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_or(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_not(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_mod(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_rot(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_call_function(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_call_builtin(&mut self) -> Result<()> {
        let address = self.get_u32()?;
        let count = self.get_u8()?;
        INTRISICS[address as usize](self, count)
    }

    fn instruction_load_local(&mut self) -> Result<()> {
        Ok(())
    }

    fn instruction_get_local(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_load_global(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_get_global(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_load_free(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_get_free(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_jump_if(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_jump_forward(&mut self) -> Result<()> {
        todo!()
    }

    fn instruction_jump_backward(&mut self) -> Result<()> {
        todo!()
    }
}

struct Intrinsic;
impl Intrinsic {
    fn intrinsic_print(machine: &mut Machine, count: u8) -> Result<()> {
        use std::io::Write;
        let lock = std::io::stdout().lock();
        let mut writer = std::io::BufWriter::new(lock);
        let mut output = String::new();

        let mut items = Vec::new();
        let frame = machine.get_current_frame_mut()?;
        for i in 0..count {
            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for print")
            };
            output.insert_str(0, &format!("{}", value));
            if i == count - 1 {
                items.push(value);
            }
        }
        writer.write_all(output.as_bytes())?;
        writer.flush()?;

        for items in items.into_iter().rev() {
            frame.stack.push(items);
        }

        Ok(())
    }
}
