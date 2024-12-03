use super::{
    compiler::Compiler,
    error::Error,
    instruction::{Callable, Instruction, OpCode, Value},
    symbol_table::SymbolTable,
};
use anyhow::{anyhow, Result};
use crossterm::style::Stylize;
use intrinsic::Intrinsic;
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

const INTRISICS: [fn(&mut Machine, u8) -> Result<()>; 25] = [
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
    Intrinsic::intrinsic_nth,
    Intrinsic::intrinsic_length,
    Intrinsic::intrinsic_assert_eq,
    Intrinsic::intrinsic_assert,
    Intrinsic::intrinsic_create_list,
    Intrinsic::intrinsic_cons,
    Intrinsic::intrinsic_car,
];

#[derive(Debug)]
pub(crate) struct Frame {
    pub return_address: Option<usize>,
    pub args: Vec<Value>,
    pub stack: Vec<Value>,
}

impl Frame {
    fn new(return_address: usize, args: Vec<Value>) -> Self {
        Self {
            return_address: Some(return_address),
            args,
            stack: Vec::new(),
        }
    }

    fn bring_to_top_of_stack(&mut self, count: usize) {
        let length = self.stack.len();
        self.stack[length - count..].rotate_left(count.saturating_sub(1));
    }
}

#[derive(Debug, Default)]
pub struct MachineOptions {
    pub quiet: bool,
}

#[derive(Debug)]
pub struct Machine {
    pub(crate) options: MachineOptions,
    pub(crate) program: Vec<u8>,
    pub(crate) global: Vec<Value>,
    pub(crate) free: Vec<Value>,
    pub(crate) stack: Vec<Frame>,
    pub(crate) ip: usize,
    pub(crate) is_running: bool,
    pub(crate) symbol_table: Option<SymbolTable>,
    #[cfg(any(debug_assertions, test))]
    pub(crate) cycle_count: usize,
}

impl Default for Machine {
    fn default() -> Self {
        Self::new(Vec::new())
    }
}

// Constants
impl Machine {
    const MAX_STACK_FRAME_SIZE: usize = 1024;
}

// Public
impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        let mut stack = Vec::with_capacity(1024);
        stack.push(Frame {
            return_address: None,
            args: Vec::with_capacity(256),
            stack: Vec::with_capacity(1024),
        });
        Self {
            options: MachineOptions::default(),
            program,
            global: Vec::with_capacity(1024),
            free: Vec::with_capacity(1024),
            stack,
            ip: 0,
            is_running: true,
            symbol_table: None,
            #[cfg(any(debug_assertions, test))]
            cycle_count: 0,
        }
    }

    pub fn with_symbol_table(mut self, symbol_table: SymbolTable) -> Self {
        self.symbol_table = Some(symbol_table);
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
            .with_maybe_a_symbol_table(self.symbol_table.take())
            .with_offset(self.program.len())
            .no_main(true)
            .compile(src);
        let (st, instructions) = match compiler {
            Ok(Some((symbol_table, instructions))) => (symbol_table, instructions),
            Ok(None) => return Ok(()),
            Err(errors) => {
                for error in errors {
                    error.report("repl", src).expect("unable to report error");
                }
                return Ok(());
            }
        };
        self.symbol_table = Some(st);

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

    pub fn call_from_address(&mut self, address: usize, count: u8) -> Result<()> {
        let param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count as usize)
        };

        let new_frame = Frame::new(self.ip, param_values);
        self.stack.push(new_frame);
        self.ip = address;

        while self.ip < self.program.len() {
            let Some(byte) = self.peek_u8() else {
                // TODO: ERROR REPORTING
                panic!("missing opcode on stack");
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
                Ok(Value::Callable(Box::new(Callable::new(index, name))))
            }
            Value::CODE_BUILTIN => {
                let index = self.get_u32()? as usize;
                Ok(Value::Builtin(index))
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

    pub(crate) fn get_current_frame(&self) -> Result<&Frame> {
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
        let value = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORTING
                panic!("missing return value on stack");
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
        let (value, test_name) = {
            let frame = self.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORTING
                panic!("missing return value on stack");
            };
            let test_name = frame.args[0].clone();
            if let Some(address) = frame.return_address {
                self.ip = address;
            }
            (value, test_name)
        };

        eprintln!(
            "test {}: {}",
            test_name.to_string().split("()").collect::<Vec<_>>()[1],
            if matches!(value, Value::Bool(true)) {
                "pass".green()
            } else {
                "fail".red()
            }
        );
        self.stack.pop();
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
                _ => panic!("invalid types for Add"),
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
                _ => panic!("invalid types for Sub"),
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
                _ => panic!("invalid types for Mul"),
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
                _ => panic!("invalid types for Div"),
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
            (Value::I32(l), Value::I32(r)) => l == r,
            (Value::F64(l), Value::F64(r)) => l == r,
            _ => panic!("invalid types for Less Than"),
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
            _ => panic!("invalid types for Less Than"),
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
            _ => panic!("invalid types for Less Than"),
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
            _ => panic!("invalid types for Greater Than Or Equal"),
        });
        frame.stack.push(Value::Bool(value));
        Ok(())
    }

    fn instruction_less_than_or_equal(&mut self) -> Result<()> {
        let count = self.get_u8()? as usize;
        let frame = self.get_current_frame_mut()?;
        let index = frame.stack.len() - count;
        let args = frame.stack.split_off(index);
        let left = &args[0];
        let value = args.iter().skip(1).all(|right| match (left, right) {
            (Value::I32(l), Value::I32(r)) => l <= r,
            (Value::F64(l), Value::F64(r)) => l <= r,
            _ => panic!("invalid types for Greater Than Or Equal"),
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
                _ => panic!("invalid types for Or"),
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
                _ => panic!("invalid types for And"),
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    fn instruction_not(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            panic!("expected value on stack for Not")
        };
        match value {
            Value::Bool(b) => {
                frame.stack.push(Value::Bool(!b));
            }
            _ => panic!("invalid types for Not"),
        }
        Ok(())
    }

    fn instruction_mod(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(right) = frame.stack.pop() else {
            panic!("expected value on stack for Mod")
        };
        let Some(left) = frame.stack.last() else {
            panic!("expected value on stack for Mod")
        };
        let last_index = frame.stack.len() - 1;
        match (left, right) {
            (Value::I32(left), Value::I32(right)) => {
                frame.stack[last_index] = Value::I32(left % right);
            }
            (Value::F64(left), Value::F64(right)) => {
                frame.stack[last_index] = Value::F64(left % right);
            }
            _ => panic!("invalid types for Mod"),
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
                panic!("missing callable value on stack");
            };
            value
        };
        let callable = match value {
            Value::Builtin(index) => {
                INTRISICS[index](self, count as u8)?;
                return Ok(());
            }
            Value::Callable(data) => data,
            _ => {
                // TODO: ERROR REPORT;
                panic!("can not call '{}' type", value);
            }
        };
        let mut param_values = {
            let frame = self.get_current_frame_mut()?;
            let length = frame.stack.len();
            frame.stack.split_off(length - count)
        };

        param_values.reverse();

        let new_frame = Frame::new(self.ip, param_values);
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
                panic!("missing callable value on stack");
            };
            value
        };
        let Value::Callable(callable) = value else {
            // TODO: ERROR REPORT;
            panic!("can not call '{}' type", value);
        };
        let test_name = Value::String(Box::new(callable.name));
        let new_frame = Frame::new(self.ip, vec![test_name]);
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
            panic!("missing callable value on stack");
        };
        let Value::Callable(callable) = value else {
            // TODO: ERROR REPORT;
            panic!("can not call '{}' type", value);
        };

        let length = frame.stack.len();
        let mut param_values = frame.stack.split_off(length - count);
        param_values.reverse();

        frame.args = param_values;
        self.ip = callable.address;
        Ok(())
    }

    fn instruction_set_local(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORT;
            panic!("no value on stack for SetLocal");
        };
        frame.args.push(value);
        Ok(())
    }

    fn instruction_get_local(&mut self) -> Result<()> {
        let index = self.get_u32()? as usize;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.args.get(index) else {
            // TODO: ERROR REPORTING
            panic!("no args on the arg stack");
        };
        frame.stack.push(value.clone());
        Ok(())
    }

    fn instruction_set_global(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORTING
            panic!("missing value on stack frame for SetGlobal instruction")
        };

        self.global.push(value);

        Ok(())
    }

    fn instruction_get_global(&mut self) -> Result<()> {
        let index = self.get_u32()? as usize;
        let Some(value) = self.global.get(index).cloned() else {
            // TODO: ERROR REPORTING
            panic!("no value on the global stack");
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_set_free(&mut self) -> Result<()> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            // TODO: ERROR REPORTING
            panic!("no value on the stack for SetFree");
        };
        self.free.push(value);
        Ok(())
    }

    fn instruction_get_free(&mut self) -> Result<()> {
        let index = self.get_u32()? as usize;
        let Some(value) = self.free.get(index).cloned() else {
            // TODO: ERROR REPORTING
            panic!("no value on the free stack at index {index}");
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_jump_if(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            panic!("expected value on stack for JumpIf")
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
                OpCode::SetLocal => instructions.push(Instruction::SetLocal),
                OpCode::SetGlobal => instructions.push(Instruction::SetGlobal),
                OpCode::SetFree => instructions.push(Instruction::SetFree),
                OpCode::GetLocal => {
                    instructions.push(Instruction::GetLocal(self.get_u32()? as usize))
                }
                OpCode::GetGlobal => {
                    instructions.push(Instruction::GetGlobal(self.get_u32()? as usize))
                }
                OpCode::GetFree => {
                    instructions.push(Instruction::GetFree(self.get_u32()? as usize))
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

// --------------------- Intrinsics ---------------------
mod intrinsic {
    use super::{Machine, Value};
    use anyhow::Result;
    use crossterm::style::Stylize;
    use std::collections::HashMap;

    pub(crate) struct Intrinsic;
    impl Intrinsic {
        pub(crate) fn intrinsic_sleep(machine: &mut Machine, count: u8) -> Result<()> {
            // (sleep 1000) ; -> false
            if count != 1 {
                anyhow::bail!("sleep only support 1 arg");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::F64(value)) = frame.stack.pop() else {
                anyhow::bail!("expected number on stack for sleep")
            };
            std::thread::sleep(std::time::Duration::from_millis(value as u64));
            Ok(())
        }

        pub(crate) fn intrinsic_is_atom(machine: &mut Machine, count: u8) -> Result<()> {
            // (atom? 10) ; -> true
            // (atom? "10") ; -> true
            // (atom? "abc") ; -> true
            // (atom? '(1 2 3)) ; -> false
            if count != 1 {
                anyhow::bail!("atom? only support 1 arg");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(item) = frame.stack.pop() else {
                anyhow::bail!("expected atom on stack for atom?")
            };
            let result = !matches!(item, Value::List(_) | Value::Callable(_));
            frame.stack.push(Value::Bool(result));
            Ok(())
        }

        pub(crate) fn intrinsic_is_number(machine: &mut Machine, count: u8) -> Result<()> {
            // (number? 10) ; -> true
            // (number? "10") ; -> true
            // (number? "abc") ; -> false
            // (number? '(1 2 3)) ; -> false
            if count != 1 {
                anyhow::bail!("number? only support 1 arg");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(item) = frame.stack.pop() else {
                anyhow::bail!("expected number on stack for number?")
            };
            let result = matches!(
                item,
                Value::F64(_) | Value::U32(_) | Value::I32(_) | Value::F32(_) | Value::U8(_)
            );
            frame.stack.push(Value::Bool(result));
            Ok(())
        }

        pub(crate) fn intrinsic_slice(machine: &mut Machine, count: u8) -> Result<()> {
            // (slice "abc" 1 2) ; -> "b"
            // (slice (list 1 2 3) 1 2) ; -> (2)
            if count != 3 {
                anyhow::bail!("slice only support 3 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::F64(end)) = frame.stack.pop() else {
                anyhow::bail!("expected int on stack for slice")
            };
            let Some(Value::F64(start)) = frame.stack.pop() else {
                anyhow::bail!("expected int on stack for slice")
            };
            let Some(item) = frame.stack.pop() else {
                anyhow::bail!("expected string or list on stack for slice")
            };

            match item {
                Value::String(string) => {
                    let result = string
                        .chars()
                        .skip(start as usize)
                        .take((end - start) as usize)
                        .collect::<String>();
                    frame.stack.push(Value::String(Box::new(result)));
                }
                Value::List(list) => {
                    let result = list
                        .iter()
                        .skip(start as usize)
                        .take((end - start) as usize)
                        .cloned()
                        .collect::<Vec<_>>();
                    frame.stack.push(Value::List(Box::new(result)));
                }
                _ => panic!("expected string or list on stack for slice"),
            }
            Ok(())
        }

        pub(crate) fn intrinsic_join(machine: &mut Machine, count: u8) -> Result<()> {
            // (join " " "abc") ; -> "abc"
            // (join " " (list "1" "2" "3")) ; -> "1 2 3"
            if count != 2 {
                anyhow::bail!("join only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(list)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for join")
            };
            let Some(Value::String(string)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for join")
            };
            let result = list
                .iter()
                .map(|i| i.to_string())
                .collect::<Vec<_>>()
                .join(&string);
            frame.stack.push(Value::String(Box::new(result)));
            Ok(())
        }

        pub(crate) fn intrinsic_split(machine: &mut Machine, count: u8) -> Result<()> {
            // (split " " "abc") ; -> ("a" "b" "c")
            // (split " " "(+ 1 1)") ; ->  ("(+" "1" "1)")
            if count != 2 {
                anyhow::bail!("split only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::String(string)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for split")
            };
            let Some(Value::String(item)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for split")
            };
            let result = string
                .split(&*item)
                .map(|i| Value::String(Box::new(i.to_string())))
                .collect::<Vec<_>>();
            frame.stack.push(Value::List(Box::new(result)));

            Ok(())
        }
        pub(crate) fn intrinsic_to_string(machine: &mut Machine, count: u8) -> Result<()> {
            // (to-string 1000) => "1000"
            if count != 1 {
                anyhow::bail!("to-string only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for to-string")
            };
            frame.stack.push(Value::String(Box::new(value.to_string())));

            Ok(())
        }

        pub(crate) fn intrinsic_filter(machine: &mut Machine, count: u8) -> Result<()> {
            // (filter (lambda (x) (> x 1)) (list 1 2 3)) ; -> (2 3)

            if count != 2 {
                anyhow::bail!("filter only support 2 args");
            }

            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for filter")
            };
            let Some(Value::Callable(callable)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for filter")
            };
            let mut result = Vec::new();
            for value in list.drain(..) {
                machine.push(value.clone())?;
                machine.call_from_address(callable.address, 1)?;
                let Some(Value::Bool(true)) = machine.pop()? else {
                    continue;
                };
                result.push(value);
            }
            machine.push(Value::List(Box::new(result)))?;
            Ok(())
        }
        pub(crate) fn intrinsic_fold_right(machine: &mut Machine, count: u8) -> Result<()> {
            // (fold-right 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
            if count != 3 {
                anyhow::bail!("fold only support 3 args");
            }
            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for fold")
            };
            let Some(Value::Callable(callable)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for fold")
            };
            let Some(initial) = machine.pop()? else {
                anyhow::bail!("expected number on stack for fold")
            };
            let mut result = initial;
            for value in list.drain(..).rev() {
                machine.push(result)?;
                machine.push(value)?;
                machine.call_from_address(callable.address, 2)?;
                let Some(v) = machine.pop()? else {
                    // TODO: ERROR REPORTING
                    anyhow::bail!("expected value on stack for fold right")
                };
                result = v;
            }
            machine.push(result)?;
            Ok(())
        }

        pub(crate) fn intrinsic_fold(machine: &mut Machine, count: u8) -> Result<()> {
            // (fold 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
            if count != 3 {
                anyhow::bail!("fold only support 3 args");
            }
            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for fold")
            };
            let Some(Value::Callable(callable)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for fold")
            };
            let Some(initial) = machine.pop()? else {
                anyhow::bail!("expected number on stack for fold")
            };
            let mut result = initial;
            for value in list.drain(..) {
                machine.push(result)?;
                machine.push(value)?;
                machine.call_from_address(callable.address, 2)?;
                let Some(v) = machine.pop()? else {
                    // TODO: ERROR REPORTING
                    anyhow::bail!("expected value on stack for fold")
                };
                result = v;
            }
            machine.push(result)?;
            Ok(())
        }

        pub(crate) fn intrinsic_map(machine: &mut Machine, count: u8) -> Result<()> {
            // (map (lambda (x) (+ x 1)) (list 1 2 3)) => (2 3 4)
            if count != 2 {
                anyhow::bail!("map only support 2 args");
            }

            let Some(Value::List(mut list)) = machine.pop()? else {
                anyhow::bail!("expected list on stack for map")
            };

            let Some(Value::Callable(callable)) = machine.pop()? else {
                anyhow::bail!("expected lambda on stack for map")
            };
            let mut result = Vec::new();
            for value in list.drain(..) {
                machine.push(value)?;
                machine.call_from_address(callable.address, 1)?;
                let Some(v) = machine.pop()? else {
                    // TODO: ERROR REPORTING
                    anyhow::bail!("expected value on stack for map")
                };
                result.push(v);
            }

            machine.push(Value::List(Box::new(result)))?;
            Ok(())
        }

        pub(crate) fn intrinsic_nth(machine: &mut Machine, count: u8) -> Result<()> {
            // (nth (list 1 2 3) 1) => 2
            if count != 2 {
                anyhow::bail!("nth only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::F64(index)) = frame.stack.pop() else {
                anyhow::bail!("expected number on stack for nth")
            };
            let Some(Value::List(mut list)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for nth")
            };
            let index = index as usize;
            if index >= list.len() {
                anyhow::bail!("nth index out of range");
            }
            let value = list.remove(index);
            frame.stack.push(value);
            Ok(())
        }

        pub(crate) fn intrinsic_reverse(machine: &mut Machine, count: u8) -> Result<()> {
            // (reverse (list 1 2 3)) => (3 2 1)
            if count != 1 {
                anyhow::bail!("reverse only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(Value::List(mut list)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for reverse")
            };
            list.reverse();
            frame.stack.push(Value::List(list));
            Ok(())
        }

        pub(crate) fn intrinsic_append(machine: &mut Machine, count: u8) -> Result<()> {
            // (append (list 1 2) (list 3 4)) => (1 2 3 4)
            if count != 2 {
                anyhow::bail!("append only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(mut list2)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for append")
            };
            let Some(Value::List(mut list1)) = frame.stack.pop() else {
                anyhow::bail!("expected list on stack for append")
            };

            list1.append(&mut list2);
            frame.stack.push(Value::List(list1));
            Ok(())
        }

        pub(crate) fn intrinsic_last(machine: &mut Machine, count: u8) -> Result<()> {
            // (last (list 1 2 3 4)) => 4
            if count != 1 {
                anyhow::bail!("last only support 1 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for last")
            };
            match &value {
                Value::List(list) if list.is_empty() => {
                    frame.stack.push(Value::List(Box::default()));
                }
                Value::List(list) => {
                    if let Some(value) = list.last() {
                        frame.stack.push(value.clone());
                        return Ok(());
                    }
                    anyhow::bail!("expected list to have at least 1 for `last`");
                }
                _ => anyhow::bail!("expected list on stack for last"),
            }
            Ok(())
        }

        pub(crate) fn intrinsic_cdr(machine: &mut Machine, count: u8) -> Result<()> {
            // (cdr (list 1 2)) => (2)
            if count != 1 {
                anyhow::bail!("cdr only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for cdr")
            };
            match &value {
                Value::List(list) if list.is_empty() => {
                    frame.stack.push(Value::List(Box::default()));
                }
                Value::List(list) => {
                    frame.stack.push(Value::List(Box::new(list[1..].to_vec())));
                }
                _ => anyhow::bail!("expected list on stack for cdr"),
            }

            if let Value::List(list) = value {
                if let Some(Value::List(list)) = list.last() {
                    frame.stack.push(list[0].clone());
                }
            }
            Ok(())
        }

        pub(crate) fn intrinsic_typeof(machine: &mut Machine, count: u8) -> Result<()> {
            // (typeof 1) => "Number"
            if count != 1 {
                anyhow::bail!("typeof only support 1 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(value) = frame.stack.pop() else {
                panic!("expected value on stack for typeof")
            };
            frame.stack.push(Value::String(Box::new(value.type_of())));
            Ok(())
        }

        pub(crate) fn intrinsic_print(machine: &mut Machine, count: u8) -> Result<()> {
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

        pub(crate) fn intrinsic_length(machine: &mut Machine, count: u8) -> Result<()> {
            // (length (list 1 2 3)) -> 3
            if count != 1 {
                anyhow::bail!("length only support 1 args");
            }

            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(list)) = frame.stack.pop() else {
                anyhow::bail!("expected a List on stack for nth");
            };

            frame.stack.push(Value::F64(list.len() as f64));
            Ok(())
        }

        pub(crate) fn intrinsic_assert_eq(machine: &mut Machine, count: u8) -> Result<()> {
            // (assert-eq :expected 1 :actual 2)
            if count < 2 {
                anyhow::bail!("assert-eq at least 2 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let index = frame.stack.len() - count as usize;
            let args = frame.stack.split_off(index);

            let mut keys: HashMap<String, Value> = HashMap::new();
            let mut iter = args.into_iter();
            while let Some(key) = iter.next() {
                let Some(value) = iter.next() else {
                    // TODO: RUNTIME ERROR
                    panic!("expected value to key: {key:?}")
                };
                keys.insert(key.to_string(), value);
            }

            let Some(expected) = keys.get(":expected") else {
                // TODO: RUNTIME ERROR
                anyhow::bail!("expected value to key: :expected")
            };
            let Some(actual) = keys.get(":actual") else {
                // TODO: RUNTIME ERROR
                anyhow::bail!("expected value to key: :actual")
            };

            let failed = expected != actual;
            if failed {
                eprintln!("{}", format!("expected: {expected}").green());
                eprintln!("{}", format!("actual: {actual}").red());
            }

            if let (Some(message), true) = (keys.get(":description"), failed) {
                eprintln!("{}", format!("description: {message}").yellow());
            }

            frame.stack.push(Value::Bool(!failed));
            Ok(())
        }

        pub(crate) fn intrinsic_assert(machine: &mut Machine, count: u8) -> Result<()> {
            // (assert (> 1 2) "1 is not greater than 2")
            if count != 2 {
                anyhow::bail!("assert only support 2 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(Value::String(message)) = frame.stack.pop() else {
                anyhow::bail!("expected string on stack for assert")
            };
            let Some(Value::Bool(value)) = frame.stack.pop() else {
                anyhow::bail!("expected boolean on stack for assert")
            };
            if !value {
                eprintln!("assertion failed: {message}");
            }

            frame.stack.push(Value::Bool(value));

            Ok(())
        }

        pub(crate) fn intrinsic_create_list(machine: &mut Machine, count: u8) -> Result<()> {
            // (create-list 1 2 3) => (1 2 3)
            let mut items = Vec::new();
            let frame = machine.get_current_frame_mut()?;
            for _ in 0..count {
                let Some(item) = frame.stack.pop() else {
                    panic!("expected value on stack for CreateList")
                };
                items.insert(0, item);
            }
            frame.stack.push(Value::List(Box::new(items)));
            Ok(())
        }

        pub(crate) fn intrinsic_cons(machine: &mut Machine, count: u8) -> Result<()> {
            // (cons 1 (list 2 3)) => (1 2 3)
            if count != 2 {
                anyhow::bail!("cons only support 2 args");
            }
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(mut list)) = frame.stack.pop() else {
                panic!("expected a List on stack for cons")
            };
            let Some(item) = frame.stack.pop() else {
                panic!("expected value on stack for cons")
            };
            list.insert(0, item);
            frame.stack.push(Value::List(Box::new(*list)));
            Ok(())
        }

        pub(crate) fn intrinsic_car(machine: &mut Machine, count: u8) -> Result<()> {
            // (car (list 1 2)) => 1
            if count != 1 {
                anyhow::bail!("car only support 2 args");
            }

            let frame = machine.get_current_frame_mut()?;

            let Some(Value::List(list)) = frame.stack.pop() else {
                panic!("expected a List on stack for car")
            };
            let item = list.first().cloned().unwrap_or(Value::Bool(false));

            frame.stack.push(item);
            Ok(())
        }
    }
}
