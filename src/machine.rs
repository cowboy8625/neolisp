use super::{
    ast::Span,
    compiler::Compiler,
    error::Error,
    instruction::{Callable, Instruction, OpCode, RuntimeMetadata, Value},
    symbol_table::SymbolTable,
};
use crate::intrinsic::Intrinsic;
use anyhow::{anyhow, Result};
use crossterm::style::Stylize;
use num_traits::FromPrimitive;

const INSTRUCTION_CALL: [fn(&mut Machine) -> Result<()>; 31] = [
    Machine::instruction_halt,
    Machine::instruction_return,
    Machine::instruction_return_from_test,
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
    Machine::instruction_call_test,
    Machine::instruction_tail_call_function,
    Machine::instruction_set_local,
    Machine::instruction_set_global,
    Machine::instruction_set_free,
    Machine::instruction_get_local,
    Machine::instruction_get_global,
    Machine::instruction_get_free,
    Machine::instruction_jump_if,
    Machine::instruction_jump_forward,
    Machine::instruction_jump_backward,
    Machine::instruction_jump,
];

const INTRISICS: [fn(&mut Machine, u8) -> Result<()>; 24] = [
    Intrinsic::intrinsic_sleep,
    Intrinsic::intrinsic_is_atom,
    Intrinsic::intrinsic_is_number,
    Intrinsic::intrinsic_slice,
    Intrinsic::intrinsic_join,
    Intrinsic::intrinsic_split,
    Intrinsic::intrinsic_to_string,
    Intrinsic::intrinsic_filter,
    Intrinsic::intrinsic_fold_right,
    Intrinsic::intrinsic_fold,
    Intrinsic::intrinsic_map,
    Intrinsic::intrinsic_nth,
    Intrinsic::intrinsic_reverse,
    Intrinsic::intrinsic_append,
    Intrinsic::intrinsic_last,
    Intrinsic::intrinsic_cdr,
    Intrinsic::intrinsic_typeof,
    Intrinsic::intrinsic_print,
    Intrinsic::intrinsic_length,
    Intrinsic::intrinsic_assert_eq,
    Intrinsic::intrinsic_assert,
    Intrinsic::intrinsic_create_list,
    Intrinsic::intrinsic_cons,
    Intrinsic::intrinsic_car,
];

#[derive(Debug, Clone)]
pub(crate) struct Frame {
    pub return_address: Option<usize>,
    pub scope_name: String,
    pub span: Span,
    pub args: Vec<Value>,
    pub stack: Vec<Value>,
}

impl Default for Frame {
    fn default() -> Self {
        Self {
            return_address: None,
            scope_name: "main".to_string(),
            span: Span::default(),
            args: Vec::with_capacity(256),
            stack: Vec::with_capacity(1024),
        }
    }
}

impl Frame {
    fn new(
        return_address: usize,
        scope_name: impl Into<String>,
        span: Span,
        args: Vec<Value>,
    ) -> Self {
        Self {
            return_address: Some(return_address),
            scope_name: scope_name.into(),
            span,
            args,
            stack: Vec::new(),
        }
    }

    fn bring_to_top_of_stack(&mut self, count: usize) {
        let length = self.stack.len();
        self.stack[length - count..].rotate_left(count.saturating_sub(1));
    }
}

#[derive(Debug, Default, Clone)]
pub struct MachineOptions {
    pub quiet: bool,
}

#[derive(Debug, Clone)]
pub struct Machine {
    pub(crate) options: MachineOptions,
    pub(crate) program: Vec<u8>,
    pub(crate) global: Vec<Value>,
    pub(crate) free: Vec<Value>,
    pub(crate) stack: Vec<Frame>,
    pub(crate) ip: usize,
    pub(crate) is_running: bool,
    pub(crate) symbol_table: SymbolTable,
    #[cfg(any(debug_assertions, test))]
    pub(crate) cycle_count: usize,
}

impl Default for Machine {
    fn default() -> Self {
        let program = Vec::new();
        let symbol_table = SymbolTable::default();
        Self::new(program, symbol_table)
    }
}

// Constants
impl Machine {
    const MAX_STACK_FRAME_SIZE: usize = 1024;
}

// Public
impl Machine {
    pub fn new(program: Vec<u8>, symbol_table: SymbolTable) -> Self {
        let mut stack = Vec::with_capacity(1024);
        stack.push(Frame::default());
        Self {
            options: MachineOptions::default(),
            program,
            global: Vec::with_capacity(1024),
            free: Vec::with_capacity(1024),
            stack,
            ip: 0,
            is_running: true,
            symbol_table,
            #[cfg(any(debug_assertions, test))]
            cycle_count: 0,
        }
    }

    pub fn with_program(mut self, program: Vec<u8>) -> Self {
        self.program = program;
        self
    }

    pub fn set_options(&mut self, options: MachineOptions) {
        self.options = options;
    }

    pub fn is_running(&self) -> bool {
        self.is_running
    }

    pub fn load_from_string(&mut self, src: &str) -> std::result::Result<(), Vec<Error>> {
        self.is_running = true;
        let compiler = Compiler::default()
            .with_offset(self.program.len())
            .no_main(true)
            .compile(src, &mut self.symbol_table);
        let instructions = match compiler {
            Ok(Some(instructions)) => instructions,
            Ok(None) => return Ok(()),
            Err(errors) => {
                for error in errors {
                    error.report("repl", src).expect("unable to report error");
                }
                return Ok(());
            }
        };

        let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
        self.program.extend(program);
        Ok(())
    }

    pub fn run_from_string(&mut self, src: &str) -> Result<()> {
        match self.load_from_string(src) {
            Ok(_) => (),
            Err(errors) => {
                for error in errors {
                    error.report("repl", src)?;
                }
                return Ok(());
            }
        }
        self.run()?;
        Ok(())
    }

    pub fn peek_last_stack_value(&self) -> Option<&Value> {
        let frame = self.get_current_frame().ok()?;
        frame.stack.last()
    }

    pub fn push(&mut self, value: Value) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    pub fn pop(&mut self) -> Result<Option<Value>> {
        let frame = self.get_current_frame_mut()?;
        Ok(frame.stack.pop())
    }

    pub fn call_from_address(
        &mut self,
        address: usize,
        span: Span,
        count: u8,
        scope_name: &str,
    ) -> Result<()> {
        let param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count as usize)
        };

        let new_frame = Frame::new(self.ip, scope_name, span, param_values);
        self.stack.push(new_frame);
        self.ip = address;

        while self.ip < self.program.len() {
            let Some(byte) = self.peek_u8() else {
                // TODO: ERROR REPORTING
                anyhow::bail!("missing opcode on stack");
            };
            if let Some(OpCode::Return) = OpCode::from_u8(*byte) {
                self.instruction_return()?;
                break;
            }
            self.run_once()?;
        }
        Ok(())
    }

    pub fn run_once(&mut self) -> Result<()> {
        #[cfg(any(debug_assertions, test))]
        {
            self.cycle_count += 1;
        }

        let Ok(opcode) = self.get_u8() else {
            self.shutdown();
            #[cfg(debug_assertions)]
            if !self.options.quiet {
                eprintln!("END PROGRAM {}..{}", self.ip, self.program.len());
            }
            return Ok(());
        };
        INSTRUCTION_CALL[opcode as usize](self)?;

        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        while self.is_running {
            self.run_once()?;
        }
        #[cfg(any(debug_assertions, test))]
        if !self.options.quiet {
            eprintln!("cycles: {}", self.cycle_count);
            if !self.free.is_empty() {
                eprintln!("free {:#?}", self.free);
            }
            if !self.stack.is_empty() {
                eprintln!("stack {:#?}", self.stack);
            }
        }
        Ok(())
    }

    pub(crate) fn get_current_function_name(&self) -> Option<String> {
        self.get_current_frame()
            .map(|f| f.scope_name.to_string())
            .ok()
    }
}

// Private
impl Machine {
    fn shutdown(&mut self) {
        self.is_running = false;
    }

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
                let index = self.get_u32()? as usize;
                let name = self.get_string()?;
                // TODO: use leb128 at some point.
                let start = self.get_u32()? as usize;
                let end = self.get_u32()? as usize;
                Ok(Value::Callable(Box::new(Callable::new(
                    index,
                    name,
                    start..end,
                ))))
            }
            Value::CODE_BUILTIN => {
                let index = self.get_u32()? as usize;
                let name = self.get_string()?;
                // TODO: use leb128 at some point.
                let start = self.get_u32()? as usize;
                let end = self.get_u32()? as usize;
                Ok(Value::Builtin(Box::new(Callable::new(
                    index,
                    name,
                    start..end,
                ))))
            }
            Value::CODE_SYMBOL => {
                let value = self.get_string()?;
                Ok(Value::Symbol(Box::new(value)))
            }
            Value::CODE_KEYWORD => {
                let value = self.get_string()?;
                Ok(Value::Keyword(Box::new(value)))
            }
            _ => Err(anyhow!("Unknown value `{}`", self.program[self.ip])),
        }
    }

    fn peek_u8(&mut self) -> Option<&u8> {
        self.program.get(self.ip)
    }

    fn get_u8(&mut self) -> Result<u8> {
        let Some(byte) = self.program.get(self.ip) else {
            return Err(anyhow!("index out of bounds"));
        };
        self.ip += 1;
        Ok(*byte)
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

    fn get_metadata(&mut self) -> Result<RuntimeMetadata> {
        let id = self.get_u32()? as usize;
        let len = self.get_u8()? as usize;
        let bytes = self.program[self.ip..self.ip + len].to_vec();
        self.ip += len;
        let name = String::from_utf8(bytes)?;
        let start = self.get_u32()? as usize;
        let end = self.get_u32()? as usize;
        Ok(RuntimeMetadata::new(id, &name, start..end))
    }

    pub(crate) fn get_current_frame(&self) -> Result<&Frame> {
        self.stack
            .last()
            .map_or_else(|| Err(anyhow!("Stack is empty")), Ok)
    }

    pub(crate) fn get_current_frame_mut(&mut self) -> Result<&mut Frame> {
        self.stack
            .last_mut()
            .map_or_else(|| Err(anyhow!("Stack is empty")), Ok)
    }

    fn buildin_function_call(&mut self, callable: Callable, count: usize) -> Result<()> {
        let mut param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count)
        };

        param_values.reverse();

        let new_frame = Frame::new(self.ip, callable.name, callable.span, param_values);
        self.stack.push(new_frame);
        INTRISICS[callable.address](self, count as u8)?;

        let value = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORTING
                anyhow::bail!("missing return value on stack");
            };
            if let Some(address) = frame.return_address {
                self.ip = address;
            }
            value
        };
        self.stack.pop();
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }
}

impl Machine {
    fn instruction_halt(&mut self) -> Result<()> {
        self.is_running = false;
        Ok(())
    }

    fn instruction_return(&mut self) -> Result<()> {
        self.symbol_table.exit_scope();
        let value = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORTING
                anyhow::bail!("missing return value on stack");
            };
            if let Some(address) = frame.return_address {
                self.ip = address;
            }
            value
        };
        self.stack.pop();
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_return_from_test(&mut self) -> Result<()> {
        self.symbol_table.exit_scope();
        let (value, test_name) = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORTING
                anyhow::bail!("missing return value on stack");
            };
            let test_name = frame.scope_name.clone();
            if let Some(address) = frame.return_address {
                self.ip = address;
            }
            (value, test_name)
        };

        eprintln!(
            "test {}: {}",
            test_name.split("()").collect::<Vec<_>>()[1],
            if matches!(value, Value::Bool(true)) {
                "pass".green()
            } else {
                "fail".red()
            }
        );
        self.stack.pop();
        if matches!(value, Value::Bool(false)) {
            std::process::exit(1);
        }
        Ok(())
    }

    fn instruction_push(&mut self) -> Result<()> {
        let value = self.get_value()?;
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_add(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l + r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l + r);
                }
                _ => anyhow::bail!("invalid types for Add"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_sub(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l - r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l - r);
                }
                _ => anyhow::bail!("invalid types for Sub"),
            }
        }
        frame.stack.push(left);
        Ok(())
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
                _ => anyhow::bail!("invalid types for Mul"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_div(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::I32(l), Value::I32(r)) => {
                    left = Value::I32(l / r);
                }
                (Value::F64(l), Value::F64(r)) => {
                    left = Value::F64(l / r);
                }
                _ => anyhow::bail!("invalid types for Div"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_eq(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::U8(l), Value::U8(r)) => l == r,
            (Value::F32(l), Value::F32(r)) => l == r,
            (Value::I32(l), Value::I32(r)) => l == r,
            (Value::F64(l), Value::F64(r)) => l == r,
            (Value::Keyword(l), Value::Keyword(r)) => l == r,
            (Value::Symbol(l), Value::Symbol(r)) => l == r,
            (Value::String(l), Value::String(r)) => l == r,
            // TODO: 🤔 Should we throw an error or just return false
            _ => false,
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_greater_than(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l > r,
            (Value::F64(l), Value::F64(r)) => l > r,
            // TODO: 🤔 Should we throw an error or just return false
            _ => false,
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_less_than(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l < r,
            (Value::F64(l), Value::F64(r)) => l < r,
            // TODO: 🤔 Should we throw an error or just return false
            _ => false,
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_greater_than_or_equal(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l >= r,
            (Value::F64(l), Value::F64(r)) => l >= r,
            // TODO: 🤔 Should we throw an error or just return false
            _ => false,
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_less_than_or_equal(&mut self) -> Result<()> {
        // (> 3 4 2 2 2 2 2 2 "lskdjfdslkfj")
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l <= r,
            (Value::F64(l), Value::F64(r)) => l <= r,
            // TODO: 🤔 Should we throw an error or just return false
            _ => false,
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_and(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::Bool(l), Value::Bool(r)) => {
                    left = Value::Bool(l && *r);
                }
                _ => anyhow::bail!("invalid types for Or"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_or(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let length = frame.stack.len();
        let args = frame.stack.split_off(length - count);
        let mut left = args[0].clone();
        for right in args.iter().skip(1) {
            match (left, right) {
                (Value::Bool(true), Value::Bool(_)) => {
                    left = Value::Bool(true);
                    break;
                }
                (Value::Bool(_), Value::Bool(true)) => {
                    left = Value::Bool(true);
                    break;
                }
                (Value::Bool(l), Value::Bool(r)) => {
                    left = Value::Bool(l || *r);
                }
                _ => anyhow::bail!("invalid types for And"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_not(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            anyhow::bail!("expected value on stack for Not")
        };
        match value {
            Value::Bool(b) => {
                frame.stack.push(Value::Bool(!b));
            }
            _ => anyhow::bail!("invalid types for Not"),
        }
        Ok(())
    }

    fn instruction_mod(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(right) = frame.stack.pop() else {
            anyhow::bail!("expected value on stack for Mod")
        };
        let Some(left) = frame.stack.last() else {
            anyhow::bail!("expected value on stack for Mod")
        };
        let last_index = frame.stack.len() - 1;
        match (left, right) {
            (Value::I32(left), Value::I32(right)) => {
                frame.stack[last_index] = Value::I32(left % right);
            }
            (Value::F64(left), Value::F64(right)) => {
                frame.stack[last_index] = Value::F64(left % right);
            }
            _ => anyhow::bail!("invalid types for Mod"),
        }
        Ok(())
    }

    fn instruction_rot(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        frame.bring_to_top_of_stack(2);
        Ok(())
    }

    fn instruction_call_function(&mut self) -> Result<()> {
        if self.stack.len() >= Self::MAX_STACK_FRAME_SIZE {
            return Err(anyhow!("stack frame overflow at {}", self.ip));
        }
        let count = self.get_u8()? as usize;
        let value = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORT;
                anyhow::bail!("missing callable value on stack");
            };
            value
        };

        if let Value::Builtin(callable) = value {
            self.buildin_function_call(*callable, count)?;
            return Ok(());
        }

        let Value::Callable(callable) = value else {
            // TODO: ERROR REPORT;
            anyhow::bail!("can not call '{}' type", value);
        };
        self.symbol_table.enter_scope(&callable.name);
        let mut param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count)
        };

        param_values.reverse();

        let new_frame = Frame::new(self.ip, callable.name, callable.span, param_values);
        self.stack.push(new_frame);
        self.ip = callable.address;
        Ok(())
    }

    fn instruction_call_test(&mut self) -> Result<()> {
        if self.stack.len() >= Self::MAX_STACK_FRAME_SIZE {
            return Err(anyhow!("stack frame overflow at {}", self.ip));
        }
        let value = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORT;
                anyhow::bail!("missing callable value on stack");
            };
            value
        };
        let Value::Callable(callable) = value else {
            // TODO: ERROR REPORT;
            anyhow::bail!("can not call '{}' type", value);
        };
        self.symbol_table.enter_scope(&callable.name);
        let new_frame = Frame::new(self.ip, callable.name, callable.span, Vec::new());
        self.stack.push(new_frame);
        self.ip = callable.address;
        Ok(())
    }

    fn instruction_tail_call_function(&mut self) -> Result<()> {
        if self.stack.len() >= Self::MAX_STACK_FRAME_SIZE {
            return Err(anyhow!("stack frame overflow at {}", self.ip));
        }
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;

        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORT;
            anyhow::bail!("missing callable value on stack");
        };
        let Value::Callable(callable) = value else {
            // TODO: ERROR REPORT;
            anyhow::bail!("can not call '{}' type", value);
        };

        let length = frame.stack.len();
        let mut param_values = frame.stack.split_off(length - count);
        param_values.reverse();

        frame.args = param_values;
        self.ip = callable.address;
        Ok(())
    }

    fn instruction_set_local(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORT;
            anyhow::bail!("no value on stack for SetLocal");
        };
        if let Some(stack_value) = frame.args.get_mut(index) {
            *stack_value = value;
            return Ok(());
        }
        if index == frame.args.len() {
            frame.args.push(value);
            return Ok(());
        }
        anyhow::bail!("something in the compiler is wrong with SetLocal");
    }

    fn instruction_get_local(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.args.get(index) else {
            // TODO: ERROR REPORTING
            anyhow::bail!("no args on the arg stack");
        };
        frame.stack.push(value.clone());
        Ok(())
    }

    fn instruction_set_global(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORTING
            anyhow::bail!("missing value on stack frame for SetGlobal instruction")
        };

        if let Some(stack_value) = self.global.get_mut(index) {
            *stack_value = value;
            return Ok(());
        }
        if index == self.global.len() {
            self.global.push(value);
            return Ok(());
        }
        anyhow::bail!(
            "something in the compiler is wrong with SetGlobal\nindex: {}\nvalue: {}\nlen: {}",
            index,
            value,
            self.global.len()
        );
    }

    fn instruction_get_global(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let Some(value) = self.global.get(index).cloned() else {
            // TODO: ERROR REPORTING
            anyhow::bail!("no value on the global stack");
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_set_free(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORTING
            anyhow::bail!("no value on the stack for SetFree");
        };
        if let Some(stack_value) = self.free.get_mut(index) {
            *stack_value = value;
            return Ok(());
        }
        if index == self.free.len() {
            self.free.push(value);
            return Ok(());
        }
        anyhow::bail!("something in the compiler is wrong with SetFree");
    }

    fn instruction_get_free(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let Some(value) = self.free.get(index).cloned() else {
            // TODO: ERROR REPORTING
            anyhow::bail!("no value on the free stack at index {index}");
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_jump_if(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            anyhow::bail!("expected value on stack for JumpIf")
        };
        if value == Value::Bool(false) {
            self.ip += address;
        }
        Ok(())
    }

    fn instruction_jump_forward(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        self.ip += address;
        Ok(())
    }

    fn instruction_jump_backward(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        self.ip -= address;
        Ok(())
    }

    fn instruction_jump(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        self.ip = address;
        Ok(())
    }
}

// --------------------- Decompiler ---------------------
impl Machine {
    pub fn decompile(&mut self) -> Result<Vec<Instruction>> {
        let mut instructions = Vec::new();
        let program_size = self.program.len();
        let ip_address = self.ip;
        self.ip = 0;
        while self.ip < program_size {
            let byte = self.get_u8()?;
            let opcode = OpCode::from_u8(byte).ok_or(anyhow!("Unknown opcode"))?;
            match opcode {
                OpCode::Halt => instructions.push(Instruction::Halt),
                OpCode::Return => instructions.push(Instruction::Return),
                OpCode::ReturnFromTest => instructions.push(Instruction::ReturnFromTest),
                OpCode::Push => instructions.push(Instruction::Push(Box::new(self.get_value()?))),
                OpCode::Add => instructions.push(Instruction::Add(self.get_u8()? as usize)),
                OpCode::Sub => instructions.push(Instruction::Sub(self.get_u8()? as usize)),
                OpCode::Mul => instructions.push(Instruction::Mul(self.get_u8()? as usize)),
                OpCode::Div => instructions.push(Instruction::Div(self.get_u8()? as usize)),
                OpCode::Eq => instructions.push(Instruction::Eq(self.get_u8()? as usize)),
                OpCode::GreaterThan => {
                    instructions.push(Instruction::GreaterThan(self.get_u8()? as usize))
                }
                OpCode::LessThan => {
                    instructions.push(Instruction::LessThan(self.get_u8()? as usize))
                }
                OpCode::GreaterThanOrEqual => {
                    instructions.push(Instruction::GreaterThanOrEqual(self.get_u8()? as usize))
                }
                OpCode::LessThanOrEqual => {
                    instructions.push(Instruction::LessThanOrEqual(self.get_u8()? as usize))
                }
                OpCode::And => instructions.push(Instruction::And(self.get_u8()? as usize)),
                OpCode::Or => instructions.push(Instruction::Or(self.get_u8()? as usize)),
                OpCode::Not => instructions.push(Instruction::Not),
                OpCode::Mod => instructions.push(Instruction::Mod),
                OpCode::Rot => instructions.push(Instruction::Rot),
                OpCode::Call => instructions.push(Instruction::Call(self.get_u8()? as usize)),
                OpCode::CallTest => instructions.push(Instruction::CallTest),
                OpCode::TailCall => {
                    instructions.push(Instruction::TailCall(self.get_u8()? as usize))
                }
                OpCode::SetLocal => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::SetLocal(metadata));
                }
                OpCode::SetGlobal => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::SetGlobal(metadata));
                }
                OpCode::SetFree => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::SetFree(metadata));
                }
                OpCode::GetLocal => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::GetLocal(metadata));
                }
                OpCode::GetGlobal => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::GetGlobal(metadata));
                }
                OpCode::GetFree => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::GetFree(metadata));
                }
                OpCode::JumpIf => instructions.push(Instruction::JumpIf(self.get_u32()? as usize)),
                OpCode::JumpForward => {
                    instructions.push(Instruction::JumpForward(self.get_u32()? as usize))
                }
                OpCode::JumpBackward => {
                    instructions.push(Instruction::JumpBackward(self.get_u32()? as usize))
                }
                OpCode::Jump => instructions.push(Instruction::Jump(self.get_u32()? as usize)),
            }
        }
        self.ip = ip_address;
        Ok(instructions)
    }
}

#[cfg(test)]
mod tests {
    use super::{Machine, Value};
    use anyhow::Result;
    #[test]
    fn test_single_value() -> Result<()> {
        let mut machine = Machine::default();
        machine.run_from_string(r#"12345"#)?;
        machine.run()?;
        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(12345.0));
        assert_eq!(machine.cycle_count, 2);

        let mut machine = Machine::default();
        machine.run_from_string(r#"321.123"#)?;
        machine.run()?;
        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(321.123));

        let mut machine = Machine::default();
        machine.run_from_string(r#""string :)""#)?;
        machine.run()?;
        let frame = machine.get_current_frame()?;
        assert_eq!(
            frame.stack[0],
            Value::String(Box::new(String::from("string :)")))
        );
        assert_eq!(machine.cycle_count, 2);
        assert_eq!(machine.cycle_count, 2);
        Ok(())
    }

    #[test]
    fn test_add_function() -> Result<()> {
        let src = r#"(+ 1 2)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(3.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_sub_function() -> Result<()> {
        let src = r#"(- 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(1.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_mul_function() -> Result<()> {
        let src = r#"(* 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(2.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_div_function() -> Result<()> {
        let src = r#"(/ 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(2.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_eq_function() -> Result<()> {
        let src = r#"(= 2 2 2)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
        Ok(())
    }

    #[test]
    fn test_gt_function() -> Result<()> {
        let src = r#"(> 3 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
        Ok(())
    }

    #[test]
    fn test_lt_function() -> Result<()> {
        let src = r#"(< 2 3 4)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
        Ok(())
    }

    #[test]
    fn test_gte_function() -> Result<()> {
        let src = r#"(>= 3 3 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 6);
        Ok(())
    }

    #[test]
    fn test_lte_function() -> Result<()> {
        let src = r#"(<= 1 3 2 1)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 6);
        Ok(())
    }

    #[test]
    fn test_and_function() -> Result<()> {
        let src = r#"(and true true)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_or_function() -> Result<()> {
        let src = r#"(or false true)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_not_function() -> Result<()> {
        let src = r#"(not false)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 3);
        Ok(())
    }

    #[test]
    fn test_mod_function() -> Result<()> {
        let src = r#"(mod 4 2)"#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(0.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_calling_a_function() -> Result<()> {
        let src = r#"
        (fn add (x y) (+ x y))
        (add 123 321)
        "#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(444.0));
        assert_eq!(machine.cycle_count, 12);
        Ok(())
    }

    #[test]
    fn test_calling_a_lambda() -> Result<()> {
        let src = r#"
        (var add (lambda (x y) (+ x y)))
        (add 123 321)
        "#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(444.0));
        assert_eq!(machine.cycle_count, 12);
        Ok(())
    }

    #[test]
    fn test_var_function() -> Result<()> {
        let src = r#"
        (var X 100)
        (var Y 300)
        (fn add (x y) (+ x y))
        (add X Y)
        "#;
        let mut machine = Machine::default();
        machine.run_from_string(src)?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(400.0));
        assert_eq!(machine.cycle_count, 16);
        Ok(())
    }

    #[test]
    fn test_if_else_function() -> Result<()> {
        let mut machine = Machine::default();
        machine.run_from_string("(if true 1 3)")?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(1.0));
        assert_eq!(machine.cycle_count, 5);

        let mut machine = Machine::default();
        machine.run_from_string("(if false 1 3)")?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(3.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }

    #[test]
    fn test_let_function() -> Result<()> {
        let mut machine = Machine::default();
        machine.run_from_string("(let (x 10) x)")?;
        machine.run()?;

        let frame = machine.get_current_frame()?;
        assert_eq!(frame.stack[0], Value::F64(10.0));
        assert_eq!(machine.cycle_count, 4);
        Ok(())
    }
}
