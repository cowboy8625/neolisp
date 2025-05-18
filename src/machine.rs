use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::{c_char, CStr, CString},
    rc::Rc,
};

use super::{
    ast::Span,
    builtin::Function,
    compiler::Compiler,
    error::Error,
    instruction::{
        CallFfi, Callable, Instruction, LoadLibrary, OpCode, RuntimeMetadata, Struct, Value,
        ValueKind,
    },
    symbol_table::SymbolTable,
    symbol_table::Type,
};

use crossterm::style::Stylize;
use libloading::Library;
use num_traits::FromPrimitive;

type Result<T> = std::result::Result<T, Box<Error>>;

macro_rules! generate_instruction_operator {
    ($name:ident, $token:tt) => {
        fn $name(&mut self) -> Result<()> {
            let count = self.get_u8()? as usize;
            let frame = self.get_current_frame_mut()?;
            let index = frame.stack.len() - count;
            let args = frame.stack.split_off(index);
            let left = &args[0];

            for right in args.iter().skip(1) {
                match (left, right) {
                    (Value::U8(l), Value::U8(r)) if l $token r => continue,
                    (Value::F32(l), Value::F32(r)) if l $token r => continue,
                    (Value::I32(l), Value::I32(r)) if l $token r => continue,
                    (Value::F64(l), Value::F64(r)) if l $token r => continue,
                    (Value::Keyword(l), Value::Keyword(r)) if l $token r => continue,
                    (Value::Symbol(l), Value::Symbol(r)) if l $token r => continue,
                    (Value::String(l), Value::Symbol(r)) if l $token r => continue,
                    (Value::Symbol(l), Value::String(r)) if l $token r => continue,
                    (Value::String(l), Value::String(r)) if l $token r => continue,
                    (l @ Value::Nil, r @ Value::Nil) if l $token r => continue,
                    (r, l) if l.type_of() != r.type_of() => {
                        frame.stack.push(Value::Bool(false));
                        return Ok(());
                    }
                    _ => {
                        frame.stack.push(Value::Bool(false));
                        return Ok(());
                    }
                }
            }

            frame.stack.push(Value::Bool(true));
            Ok(())
        }
    };
}

macro_rules! make_static_array {
    ($name:ident, $type:ty, $kind:path, $( $func:ident ),+ $(,)?) => {
        const $name: [$type; count_idents!($($func),+)] = [
            $(
                <$kind>::$func,
            )+
        ];
    };
}

// Helper macro to count the number of identifiers
macro_rules! count_idents {
    ($($idents:ident),*) => {
        <[()]>::len(&[$(count_idents!(@sub $idents)),*])
    };
    (@sub $ident:ident) => { () };
}

make_static_array!(
    INSTRUCTION_CALL,
    fn(&mut Machine) -> Result<()>,
    Machine,
    instruction_halt,
    instruction_return,
    instruction_return_from_test,
    instruction_push,
    instruction_add,
    instruction_sub,
    instruction_mul,
    instruction_div,
    instruction_eq,
    instruction_greater_than,
    instruction_less_than,
    instruction_greater_than_or_equal,
    instruction_less_than_or_equal,
    instruction_and,
    instruction_or,
    instruction_not,
    instruction_mod,
    instruction_rot,
    instruction_call_function,
    instruction_call_test,
    instruction_tail_call_function,
    instruction_set_local,
    instruction_set_global,
    instruction_set_free,
    instruction_get_local,
    instruction_get_global,
    instruction_get_free,
    instruction_jump_if,
    instruction_jump_forward,
    instruction_jump_backward,
    instruction_jump,
    instruction_load_library,
    instruction_call_ffi,
    instruction_struct_init,
    instruction_struct_setter,
    instruction_struct_getter,
);

make_static_array!(
    INTRISICS,
    fn(&mut Machine) -> Result<()>,
    Function,
    fn_sleep,
    fn_is_atom,
    fn_is_number,
    fn_slice,
    fn_join,
    fn_split,
    fn_to_string,
    fn_filter,
    fn_fold_right,
    fn_fold,
    fn_map,
    fn_nth,
    fn_reverse,
    fn_append,
    fn_last,
    fn_cdr,
    fn_typeof,
    fn_print,
    fn_input,
    fn_length,
    fn_assert_eq,
    fn_assert,
    fn_create_list,
    fn_cons,
    fn_car,
    fn_string_trim_left,
    fn_string_trim_right,
    fn_string_trim,
    fn_print_fmt,
    fn_string_to_number,
    fn_random_int,
    fn_max,
    fn_min,
    fn_floor,
);

#[derive(Debug, Clone)]
pub(crate) struct Frame {
    pub return_address: usize,
    pub scope_name: String,
    pub span: Span,
    pub args: Vec<Value>,
    pub stack: Vec<Value>,
}

impl Frame {
    fn new(
        return_address: usize,
        scope_name: impl Into<String>,
        span: Span,
        args: Vec<Value>,
    ) -> Self {
        Self {
            return_address,
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

/// When Cloning use `save_state` and `load_state`
#[derive(Debug)]
pub struct Machine {
    pub(crate) ffi_libs: HashMap<String, Library>,
    pub(crate) c_strings: Vec<CString>,
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

impl Clone for Machine {
    fn clone(&self) -> Self {
        Self {
            ffi_libs: HashMap::new(),
            c_strings: Vec::new(),
            options: self.options.clone(),
            program: self.program.clone(),
            global: self.global.clone(),
            free: self.free.clone(),
            stack: self.stack.clone(),
            ip: self.ip,
            is_running: self.is_running,
            symbol_table: self.symbol_table.clone(),
            #[cfg(any(debug_assertions, test))]
            cycle_count: self.cycle_count,
        }
    }
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
        let frame = Frame {
            return_address: program.len().saturating_sub(1),
            scope_name: "global".to_string(),
            span: 0..program.len(),
            args: Vec::with_capacity(1024),
            stack: Vec::with_capacity(1024),
        };
        stack.push(frame);
        Self {
            ffi_libs: HashMap::new(),
            c_strings: Vec::new(),
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

    pub fn save_state(&self) -> Self {
        let mut new = self.clone();
        for name in self.ffi_libs.keys() {
            let lib = unsafe { Library::new(name).unwrap() };
            new.ffi_libs.insert(name.clone(), lib);
        }
        new
    }

    pub fn load_state(&mut self, state: Machine) {
        self.ffi_libs = state.ffi_libs;
        self.options = state.options;
        self.program = state.program;
        self.global = state.global;
        self.free = state.free;
        self.stack = state.stack;
        self.ip = state.ip;
        self.is_running = state.is_running;
        self.symbol_table = state.symbol_table;
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
            Err(errors) => return Err(errors),
        };

        let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
        self.program.extend(program);
        Ok(())
    }

    pub fn run_from_string(&mut self, src: &str) -> std::result::Result<(), Vec<Error>> {
        self.load_from_string(src)?;
        self.run().map_err(|e| vec![*e])
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

    pub fn check_arg_count(&self, expected_count: usize) -> Result<()> {
        let frame = self.get_current_frame()?;
        let scope_name = frame.scope_name.to_string();
        if frame.args.len() != expected_count {
            let args_len = frame.args.len();
            let span = frame.span.clone();
            let stack_trace = self.create_stack_trace();
            let error = Error::RunTimeError {
                name: "Argument Mismatch".to_string(),
                span,
                stack_trace,
                message: format!(
                    "{} takes {} arg(s) but {} was given",
                    scope_name, expected_count, args_len,
                ),
                code: "E010".to_string(),
                note: None,
                help: None,
            };

            return Err(Box::new(error));
        }
        Ok(())
    }

    pub fn pop(&mut self) -> Result<Value> {
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.stack.pop() else {
            return Err(Box::new(Error::StackUnderflow));
        };
        Ok(value)
    }

    pub fn pop_arg(&mut self, arg_count: usize, ty: Option<&[ValueKind]>) -> Result<Value> {
        let frame = self.get_current_frame_mut()?;
        let item = frame.args.pop().ok_or(Error::StackUnderflow)?;
        let Some(ty) = ty else { return Ok(item) };
        if !ty.contains(&item.kind()) {
            let span = frame.span.clone();
            let scope_name = frame.scope_name.to_string();
            let stack_trace = self.create_stack_trace();
            let error = Error::RunTimeError {
                name: "Type Mismatch".to_string(),
                span,
                stack_trace,
                message: format!(
                    "{} arg for {} should be a type {} but found {}",
                    arg_count,
                    scope_name,
                    ty.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(" or "),
                    item.type_of()
                ),
                code: "E011".to_string(),
                note: None,
                help: None,
            };
            return Err(Box::new(error));
        }
        Ok(item)
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
            let byte = self
                .peek_u8()
                .map_or_else(|| Err(Box::new(Error::StackUnderflow)), Ok)?;
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
        match INSTRUCTION_CALL[opcode as usize](self) {
            Ok(_) => (),
            Err(err) => {
                self.shutdown();
                return Err(err);
            }
        }

        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        while self.is_running {
            self.run_once()?;
        }
        #[cfg(any(debug_assertions, test))]
        if !self.options.quiet {
            eprintln!("cycles: {}", self.cycle_count);
            if !self.global.is_empty() {
                eprintln!("global:");
                for (index, value) in self.global.iter().enumerate() {
                    eprintln!("{}: {}", index, value);
                }
            }
            if !self.free.is_empty() {
                eprintln!("free:");
                for (index, value) in self.free.iter().enumerate() {
                    eprintln!("{}: {}", index, value.debugger_display());
                }
            }
            if !self.stack.is_empty() {
                eprintln!("frames:");
                for (index, frame) in self.stack.iter().enumerate() {
                    eprintln!("{}: {}", index, frame.scope_name);
                    eprintln!("  return address: {}", frame.return_address);
                    eprintln!("  args:");
                    for (index, value) in frame.args.iter().enumerate() {
                        eprintln!("    {}: {}", index, value.debugger_display());
                    }
                    eprintln!("  stack:");
                    for (index, value) in frame.stack.iter().enumerate() {
                        eprintln!("    {}: {}", index, value.debugger_display());
                    }
                }
            }
        }
        Ok(())
    }

    pub(crate) fn get_current_function_name(&self) -> Option<String> {
        self.get_current_frame()
            .map(|f| f.scope_name.to_string())
            .ok()
    }

    pub(crate) fn create_stack_trace(&self) -> Vec<(String, Span)> {
        self.stack
            .iter()
            .map(|f| (f.scope_name.to_string(), f.span.clone()))
            .collect()
    }

    pub(crate) fn create_run_time_error(
        &self,
        name: &str,
        message: impl Into<String>,
        code: impl Into<String>,
        help: Option<impl Into<String>>,
    ) -> Result<Box<Error>> {
        let frame = self.get_current_frame()?;
        let span = frame.span.clone();
        let stack_trace = self.create_stack_trace();
        Ok(Box::new(Error::RunTimeError {
            name: name.to_string(),
            span,
            stack_trace,
            message: message.into(),
            code: code.into(),
            note: None,
            help: help.map(Into::into),
        }))
    }
}

// Private
impl Machine {
    fn shutdown(&mut self) {
        self.is_running = false;
    }

    fn get_value(&mut self) -> Result<Value> {
        let value_opcode = self.get_u8()?;
        let Some(value_kind) = ValueKind::from_u8(value_opcode) else {
            panic!("Unknown value `{}`", value_opcode);
        };
        match value_kind {
            ValueKind::Nil => Ok(Value::Nil),
            ValueKind::U8 => {
                let value = self.get_u8()?;
                Ok(Value::U8(value))
            }
            ValueKind::I32 => {
                let value = self.get_i32()?;
                Ok(Value::I32(value))
            }
            ValueKind::U32 => {
                let value = self.get_u32()?;
                Ok(Value::U32(value))
            }
            ValueKind::F32 => {
                let value = self.get_f32()?;
                Ok(Value::F32(value))
            }
            ValueKind::F64 => {
                let value = self.get_f64()?;
                Ok(Value::F64(value))
            }
            ValueKind::String => {
                let value = self.get_string()?;
                Ok(Value::String(Box::new(value)))
            }
            ValueKind::Bool => {
                let value = self.get_u8()? != 0;
                Ok(Value::Bool(value))
            }
            ValueKind::List => {
                let length = self.get_u32()?;
                let mut values = Vec::new();
                for _ in 0..length {
                    values.push(self.get_value()?);
                }
                Ok(Value::List(Box::new(values)))
            }
            ValueKind::Callable => {
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
            ValueKind::Builtin => {
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
            ValueKind::Symbol => {
                let value = self.get_string()?;
                Ok(Value::Symbol(Box::new(value)))
            }
            ValueKind::Keyword => {
                let value = self.get_string()?;
                Ok(Value::Keyword(Box::new(value)))
            }
            ValueKind::Struct => {
                let value_struct = self.get_struct()?;
                Ok(Value::Struct(Rc::new(RefCell::new(value_struct))))
            }
        }
    }

    fn peek_u8(&mut self) -> Option<&u8> {
        self.program.get(self.ip)
    }

    fn get_u8(&mut self) -> Result<u8> {
        let Some(byte) = self.program.get(self.ip) else {
            return Err(Box::new(Error::Internal(
                "unexpected end of program get_u8".to_string(),
            )));
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
        match String::from_utf8(bytes) {
            Ok(string) => Ok(string),
            Err(err) => panic!("{}", err),
        }
    }

    fn get_metadata(&mut self) -> Result<RuntimeMetadata> {
        let id = self.get_u32()? as usize;
        let len = self.get_u8()? as usize;
        let bytes = self.program[self.ip..self.ip + len].to_vec();
        self.ip += len;
        let name = match String::from_utf8(bytes) {
            Ok(string) => string,
            Err(err) => panic!("{}", err),
        };
        let span = self.get_span()?;
        Ok(RuntimeMetadata::new(id, &name, span))
    }

    fn get_span(&mut self) -> Result<Span> {
        let start = self.get_u32()? as usize;
        let end = self.get_u32()? as usize;
        Ok(start..end)
    }

    fn get_call_ffi(&mut self) -> Result<CallFfi> {
        let lib = self.get_string()?;
        let ffi_function_name = self.get_string()?;
        let args_len = self.get_u8()?;
        let mut args = Vec::new();
        for _ in 0..args_len {
            let byte = self.get_u8()?;
            args.push(Type::try_from(byte).unwrap());
        }
        let ret = Type::try_from(self.get_u8()?).unwrap();

        let span = self.get_span()?;
        Ok(CallFfi::new(lib, ffi_function_name, args, ret, span))
    }

    fn get_struct(&mut self) -> Result<Struct> {
        let name = self.get_string()?;
        let span = self.get_span()?;
        let fields_len = self.get_u32()? as usize;
        let mut field_names = Vec::new();
        for _ in 0..fields_len {
            let field_name = self.get_string()?;
            field_names.push(field_name);
        }
        let mut field_values = Vec::new();
        for _ in 0..fields_len {
            let value = self.get_value()?;
            field_values.push(value);
        }
        Ok(Struct {
            name,
            span,
            field_names,
            field_values,
        })
    }

    pub(crate) fn get_current_frame(&self) -> Result<&Frame> {
        self.stack.last().map_or_else(
            || Err(Box::new(Error::Internal("Stack is empty".to_string()))),
            Ok,
        )
    }

    pub(crate) fn get_current_frame_mut(&mut self) -> Result<&mut Frame> {
        self.stack.last_mut().map_or_else(
            || Err(Box::new(Error::Internal("Stack is empty".to_string()))),
            Ok,
        )
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
        INTRISICS[callable.address](self)?;

        let value = {
            let frame = self.get_current_frame_mut()?;
            let value = match frame.stack.pop() {
                Some(value) => value,
                None => Value::Nil,
            };
            self.ip = frame.return_address;
            value
        };
        self.stack.pop();
        if let Value::Nil = value {
            return Ok(());
        }
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
            let value = match frame.stack.pop() {
                Some(value) => value,
                None => Value::Nil,
            };
            self.ip = frame.return_address;
            value
        };
        self.stack.pop();
        let frame = self.get_current_frame_mut()?;
        if let Value::Nil = value {
            return Ok(());
        }
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_return_from_test(&mut self) -> Result<()> {
        self.symbol_table.exit_scope();
        let (value, test_name) = {
            let frame = self.get_current_frame_mut()?;
            // TODO: For errors I think we should some how return a list from each test.
            // Maybe at compile time we create a function that puts a Struct or List to keep track
            // of failed tests.
            // (list (list :name :pass-or-failed))
            //
            // '((test-name true) (test2-name false))
            //
            // sum up the list and at end report
            //
            // :OUTPUT:
            //
            // 2 test ran, 1 passed, 1 failed
            //
            let Some(value) = frame.stack.pop() else {
                // TODO: ERROR REPORTING
                panic!("missing return value on stack");
            };
            let test_name = frame.scope_name.clone();
            self.ip = frame.return_address;
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
                (l, r) => {
                    let message =
                        format!("Can not add {} and {} together", l.type_of(), r.type_of());
                    let help = if l.kind() == r.kind() && l.kind() == ValueKind::String {
                        Some("to concatenate strings use (append ...)")
                    } else {
                        None
                    };
                    return Err(self.create_run_time_error(
                        "Type Cannot be added",
                        message,
                        "E012",
                        help,
                    )?);
                }
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
                (l, r) => {
                    return Err(self.create_run_time_error(
                        "Type Cannot be subtracted",
                        format!(
                            "Can not subtract {} and {} together",
                            l.type_of(),
                            r.type_of()
                        ),
                        "E012",
                        None::<&str>,
                    )?);
                }
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
                (l, r) => {
                    return Err(self.create_run_time_error(
                        "Type Cannot be multiplied",
                        format!(
                            "Can not multiply {} and {} together",
                            l.type_of(),
                            r.type_of()
                        ),
                        "E012",
                        None::<&str>,
                    )?);
                }
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
                (l, r) => {
                    return Err(self.create_run_time_error(
                        "Type Cannot be divided",
                        format!(
                            "Can not divide {} and {} together",
                            l.type_of(),
                            r.type_of()
                        ),
                        "E012",
                        None::<&str>,
                    )?);
                }
            }
        }
        frame.stack.push(left);
        Ok(())
    }

    generate_instruction_operator!(instruction_eq, ==);
    generate_instruction_operator!(instruction_greater_than, >);
    generate_instruction_operator!(instruction_less_than, <);
    generate_instruction_operator!(instruction_greater_than_or_equal, >=);
    generate_instruction_operator!(instruction_less_than_or_equal, <=);

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
                (l, r) => {
                    return Err(self.create_run_time_error(
                        "Type Cannot be And",
                        format!("Can not And {} and {} together", l.type_of(), r.type_of()),
                        "E012",
                        None::<&str>,
                    )?);
                }
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
                (l, r) => {
                    return Err(self.create_run_time_error(
                        "Type Cannot be Or",
                        format!("Can not Or {} and {} together", l.type_of(), r.type_of()),
                        "E012",
                        None::<&str>,
                    )?);
                }
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
            value => {
                return Err(self.create_run_time_error(
                    "Type Cannot be Not",
                    format!("Can not Not {}", value.type_of()),
                    "E012",
                    None::<&str>,
                )?);
            }
        }
        Ok(())
    }

    fn instruction_mod(&mut self) -> Result<()> {
        let right = self.pop()?;
        let frame = self.get_current_frame_mut()?;
        let last_index = frame.stack.len() - 1;
        let mut left = &mut frame.stack[last_index];
        match (&mut left, right) {
            (Value::I32(l), Value::I32(r)) => {
                *left = Value::I32(*l % r);
            }
            (Value::F64(l), Value::F64(r)) => {
                *left = Value::F64(*l % r);
            }
            (l, r) => {
                let lhs_ty = l.type_of();
                return Err(self.create_run_time_error(
                    "Type Cannot be Mod",
                    format!("Can not Mod {} and {} together", lhs_ty, r.type_of()),
                    "E012",
                    None::<&str>,
                )?);
            }
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
            return Err(Box::new(Error::Internal(format!(
                "stack frame overflow at {}",
                self.ip
            ))));
        }
        let count = self.get_u8()? as usize;
        let value = self.pop()?;

        if let Value::Builtin(callable) = value {
            self.buildin_function_call(*callable, count)?;
            return Ok(());
        }

        let Value::Callable(callable) = value else {
            return Err(Box::new(Error::Internal(format!(
                "can not call '{}' type",
                value.kind()
            ))));
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
            panic!("stack frame overflow at {}", self.ip);
        }
        let value = self.pop()?;

        let Value::Callable(callable) = value else {
            return Err(Box::new(Error::Internal(format!(
                "can not call '{}' type",
                value.kind()
            ))));
        };
        self.symbol_table.enter_scope(&callable.name);
        let new_frame = Frame::new(self.ip, callable.name, callable.span, Vec::new());
        self.stack.push(new_frame);
        self.ip = callable.address;
        Ok(())
    }

    fn instruction_tail_call_function(&mut self) -> Result<()> {
        if self.stack.len() >= Self::MAX_STACK_FRAME_SIZE {
            panic!("stack frame overflow at {}", self.ip);
        }
        let count = self.get_u8()? as usize;

        let value = self.pop()?;

        let Value::Callable(callable) = value else {
            return Err(Box::new(Error::Internal(format!(
                "can not call '{}' type",
                value.kind()
            ))));
        };

        let frame = self.get_current_frame_mut()?;
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
        let value = self.pop()?;
        let frame = self.get_current_frame_mut()?;
        if let Some(stack_value) = frame.args.get_mut(index) {
            *stack_value = value;
            return Ok(());
        }
        if index == frame.args.len() {
            frame.args.push(value);
            return Ok(());
        }
        Err(Box::new(Error::Internal(format!(
            "{} invalid set-local address at {} ip: {}",
            file!(),
            index,
            self.ip
        ))))
    }

    fn instruction_get_local(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.args.get(index) else {
            return Err(Box::new(Error::Internal(format!(
                "{} invalid get-local address at {} ip: {}",
                file!(),
                index,
                self.ip
            ))));
        };
        frame.stack.push(value.clone());
        Ok(())
    }

    fn instruction_set_global(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let value = self.pop()?;

        if let Some(stack_value) = self.global.get_mut(index) {
            *stack_value = value;
            return Ok(());
        }

        if index == self.global.len() {
            self.global.push(value);
            return Ok(());
        }

        Err(Box::new(Error::Internal(format!(
            "{} invalid set-global address at {} ip: {} for {} and len is {}",
            file!(),
            index,
            self.ip,
            metadata.name,
            self.global.len(),
        ))))
    }

    fn instruction_get_global(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let Some(value) = self.global.get(index).cloned() else {
            return Err(Box::new(Error::Internal(format!(
                "{} invalid get-global address at {} ip: {}",
                file!(),
                index,
                self.ip
            ))));
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_set_free(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let value = self.pop()?;

        if let Some(stack_value) = self.free.get_mut(index) {
            *stack_value = value;
            return Ok(());
        }
        if index == self.free.len() {
            self.free.push(value);
            return Ok(());
        }
        Err(Box::new(Error::Internal(format!(
            "{} invalid set-free address at {} ip: {}",
            file!(),
            index,
            self.ip
        ))))
    }

    fn instruction_get_free(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let index = metadata.data;
        let Some(value) = self.free.get(index).cloned() else {
            return Err(Box::new(Error::Internal(format!(
                "{} invalid get-free address at {} ip: {}",
                file!(),
                index,
                self.ip
            ))));
        };
        let frame = self.get_current_frame_mut()?;
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_jump_if(&mut self) -> Result<()> {
        let address = self.get_u32()? as usize;
        let (Value::Bool(false) | Value::Nil) = self.pop()? else {
            return Ok(());
        };
        self.ip += address;
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

    fn instruction_load_library(&mut self) -> Result<()> {
        let library_name = self.get_string()?;
        let span = self.get_span()?;
        if self.ffi_libs.contains_key(&library_name) {
            return Ok(());
        }
        let Ok(lib) = (unsafe { Library::new(&library_name) }) else {
            panic!("failed to load library {}:{:?}", library_name, span);
        };
        self.ffi_libs.insert(library_name, lib);
        Ok(())
    }

    fn instruction_call_ffi(&mut self) -> Result<()> {
        let call_ffi = self.get_call_ffi()?;
        if !self.ffi_libs.contains_key(&call_ffi.lib) {
            panic!("library {} not loaded", call_ffi.name);
        }

        let lib = self.ffi_libs.get(&call_ffi.lib).unwrap();
        let func_ptr = unsafe {
            let sym: libloading::Symbol<*const ()> = lib
                .get(call_ffi.name.as_bytes())
                .expect("Failed to load symbol");
            libffi::middle::CodePtr::from_ptr(*sym as *const _)
        };

        let cif = libffi::middle::Cif::new(call_ffi.get_arg_types(), call_ffi.get_ret_type());
        let frame = self.get_current_frame_mut()?;
        frame.args.reverse();
        let mut args = Vec::new();
        let mut i32_args = Vec::new();
        for _ in call_ffi.args.iter() {
            let frame = self.get_current_frame_mut()?;
            let Some(item) = frame.args.pop() else {
                // TODO: ERROR REPORTING
                panic!("no value on the stack for CallFfi");
            };
            match item {
                Value::F64(value) => {
                    let value: i32 = value as i32;
                    let idx = i32_args.len();
                    i32_args.push(value);
                    let arg = libffi::middle::Arg::new(&i32_args[idx]);
                    args.push(arg);
                }
                Value::Bool(value) => {
                    let value: i32 = value as i32;
                    let idx = i32_args.len();
                    i32_args.push(value);
                    let arg = libffi::middle::Arg::new(&i32_args[idx]);
                    args.push(arg);
                }
                Value::String(value) => {
                    let c_string = CString::new(value.as_bytes()).expect("CString::from failed");
                    let idx = self.c_strings.len();
                    self.c_strings.push(c_string.clone());
                    let string: *const c_char = self.c_strings[idx].as_ptr();
                    let arg = libffi::middle::Arg::new(&string);
                    args.push(arg);
                }
                v => panic!("unsupported argument type for ffi call: {v}"),
            }
        }

        let ret_value = match call_ffi.ret {
            Type::Nil => {
                let _: () = unsafe { cif.call(func_ptr, &args) };
                Value::Nil
            }
            Type::Bool => {
                let result: i32 = unsafe { cif.call(func_ptr, &args) };
                Value::Bool(result != 0)
            }
            Type::Int => {
                let result: i32 = unsafe { cif.call(func_ptr, &args) };
                Value::F64(result as f64)
            }
            Type::String => {
                let result: *const c_char = unsafe { cif.call(func_ptr, &args) };
                let result = unsafe { CStr::from_ptr(result) };
                Value::String(Box::new(result.to_string_lossy().to_string()))
            }
            _ => unreachable!("unsupported return type for ffi call"),
        };

        let frame = self.get_current_frame_mut()?;
        frame.stack.push(ret_value);
        Ok(())
    }

    fn instruction_struct_init(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let frame = self.get_current_frame_mut()?;
        let mut field_names = Vec::new();
        let mut field_values = Vec::new();
        for _ in 0..frame.args.len() / 2 {
            let Some(value) = frame.args.pop() else {
                // TODO: ERROR REPORTING
                panic!("no value on the stack for StructInit");
            };
            let Some(stack_value_name) = frame.args.pop() else {
                // TODO: ERROR REPORTING
                panic!("no value on the stack for StructInit");
            };
            let Value::Keyword(value_name) = stack_value_name else {
                // TODO: ERROR REPORTING
                panic!("expected keyword on the stack for StructInit");
            };
            field_names.push(value_name.to_string());
            field_values.push(value);
        }
        let value_struct = Struct {
            name: metadata.name.to_string(),
            span: metadata.span,
            field_names,
            field_values,
        };
        let value = Value::Struct(Rc::new(RefCell::new(value_struct)));
        frame.stack.push(value);
        Ok(())
    }

    fn instruction_struct_setter(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let frame = self.get_current_frame_mut()?;
        let Some(value) = frame.args.pop() else {
            // TODO: ERROR REPORTING
            panic!("no value on the stack for StructSetter");
        };
        let Some(Value::Keyword(name)) = frame.args.pop() else {
            // TODO: ERROR REPORTING
            panic!("no value on the stack for StructSetter");
        };
        let Some(Value::Struct(struct_value)) = frame.args.pop() else {
            // TODO: ERROR REPORTING
            panic!("no value on the stack for StructSetter");
        };
        if struct_value.borrow().name != *metadata.name {
            // TODO: ERROR REPORTING
            panic!(
                "miss match struct setter function called on struct '{}' with '{}'",
                metadata.name,
                struct_value.borrow().name,
            );
        }
        let Some(index) = struct_value
            .borrow()
            .field_names
            .iter()
            .position(|x| x == &*name)
        else {
            // TODO: ERROR REPORTING
            panic!(
                "no value named '{name}' in struct '{}' found",
                struct_value.borrow().name
            );
        };
        struct_value.borrow_mut().field_values[index] = value;
        frame.stack.push(Value::Nil);
        Ok(())
    }
    fn instruction_struct_getter(&mut self) -> Result<()> {
        let metadata = self.get_metadata()?;
        let frame = self.get_current_frame_mut()?;
        let Some(Value::Keyword(name)) = frame.args.pop() else {
            // TODO: ERROR REPORTING
            panic!("no value on the stack for StructSetter");
        };
        let Some(Value::Struct(struct_value)) = frame.args.pop() else {
            // TODO: ERROR REPORTING
            panic!("no value on the stack for StructSetter",);
        };
        if struct_value.borrow().name != *metadata.name {
            // TODO: ERROR REPORTING
            panic!(
                "miss match struct getter function called on struct '{}' with '{}'",
                metadata.name,
                struct_value.borrow().name,
            );
        }
        let Some(index) = struct_value
            .borrow()
            .field_names
            .iter()
            .position(|x| x == &*name)
        else {
            // TODO: ERROR REPORTING
            panic!(
                "no value named '{name}' in struct '{}' found",
                struct_value.borrow().name
            );
        };
        let value = struct_value.borrow().field_values[index].clone();
        frame.stack.push(value);
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
            let opcode =
                OpCode::from_u8(byte).ok_or(Error::Internal("Unknown opcode".to_string()))?;
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
                OpCode::LoadLibrary => {
                    let name = self.get_string()?;
                    let span = self.get_span()?;
                    instructions.push(Instruction::LoadLibrary(LoadLibrary::new(name, span)))
                }
                OpCode::CallFfi => {
                    let call_ffi = self.get_call_ffi()?;
                    instructions.push(Instruction::CallFfi(call_ffi))
                }
                OpCode::StructInit => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::StructInit(metadata))
                }
                OpCode::StructSet => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::StructSet(metadata))
                }
                OpCode::StructGet => {
                    let metadata = self.get_metadata()?;
                    instructions.push(Instruction::StructGet(metadata))
                }
            }
        }
        self.ip = ip_address;
        Ok(instructions)
    }
}

#[cfg(test)]
mod tests {
    use super::{Compiler, Machine, SymbolTable, Value};
    use anyhow::Result;
    use pretty_assertions::assert_eq;

    fn run_program(src: &str) -> Result<Machine> {
        let mut symbol_table = SymbolTable::default();
        let compiler = Compiler::default()
            .no_main(true)
            .compile(&src, &mut symbol_table);

        let instructions = match compiler {
            Ok(Some(instructions)) => instructions,
            Ok(None) => return Err(anyhow::anyhow!("Failed to compile")),
            Err(errors) => {
                for error in errors {
                    error.report(&"test", &src, true);
                }
                return Err(anyhow::anyhow!("Failed to compile"));
            }
        };

        let program: Vec<u8> = instructions.iter().flat_map(|i| i.to_bytecode()).collect();
        let mut machine = Machine::new(program, symbol_table);
        match machine.run() {
            Ok(()) => return Ok(machine),
            Err(e) => Err(anyhow::anyhow!(e)),
        }
    }

    #[test]
    fn test_single_value() {
        let machine = run_program(r#"12345"#).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(12345.0));
        assert_eq!(machine.cycle_count, 2);

        let machine = run_program(r#"321.123"#).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(321.123));

        let machine = run_program(r#""string :)""#).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(
            frame.stack[0],
            Value::String(Box::new(String::from("string :)")))
        );
        assert_eq!(machine.cycle_count, 2);
        assert_eq!(machine.cycle_count, 2);
    }

    #[test]
    fn test_add_function() {
        let machine = run_program(r#"(+ 1 2)"#).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(3.0));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_sub_function() {
        let machine = run_program(r#"(- 2 1)"#).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(1.0));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_mul_function() {
        let machine = run_program(r#"(* 2 1)"#).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(2.0));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_div_function() {
        let machine = run_program(r#"(/ 2 1)"#).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(2.0));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_eq_function() {
        let src = r#"(= 2 2 2)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);

        let src = r#"(= "north" 'north)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_gt_function() {
        let src = r#"(> 3 2 1)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
    }

    #[test]
    fn test_lt_function() {
        let src = r#"(< 2 3 4)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 5);
    }

    #[test]
    fn test_gte_function() {
        let src = r#"(>= 3 3 2 1)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 6);
    }

    #[test]
    fn test_lte_function() {
        let src = r#"(<= 1 3 2 1)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 6);
    }

    #[test]
    fn test_and_function() {
        let src = r#"(and true true)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_or_function() {
        let src = r#"(or false true)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_not_function() {
        let src = r#"(not false)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::Bool(true));
        assert_eq!(machine.cycle_count, 3);
    }

    #[test]
    fn test_mod_function() {
        let src = r#"(mod 4 2)"#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(0.0));
        assert_eq!(machine.cycle_count, 4);
    }

    #[test]
    fn test_calling_a_function() {
        let src = r#"
            (fn add (x y) (+ x y))
            (add 123 321)
            "#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(444.0));
        assert_eq!(machine.cycle_count, 12);
    }

    #[test]
    fn test_calling_a_lambda() {
        let src = r#"
            (var add (lambda (x y) (+ x y)))
            (add 123 321)
            "#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(444.0));
        assert_eq!(machine.cycle_count, 12);
    }

    #[test]
    fn test_var_function() {
        let src = r#"
            (var X 100)
            (var Y 300)
            (fn add (x y) (+ x y))
            (add X Y)
            "#;
        let machine = run_program(src).unwrap();
        let frame = machine.get_current_frame().unwrap();
        assert_eq!(frame.stack[0], Value::F64(400.0));
        assert_eq!(machine.cycle_count, 16);
    }

    #[test]
    fn test_quote() {
        // TODO: use this test for the run time error when using a quote on the list.
        let src = r#"
(struct Person
    name :string
    age :int)
(var people (list
    (Person:new :name "Bob" :age 20)
    (Person:new :name "Alice" :age 21)))

    (var person (nth people 0))
    (var name (Person:get person :name))
    (var age (Person:get person :age))
            "#;
        let machine = run_program(src).unwrap();
        // let frame = machine.get_current_frame().unwrap();
        assert_eq!(
            machine.global.get(5),
            Some(&Value::String(Box::new(String::from("Bob"))))
        );
        assert_eq!(machine.global.get(6), Some(&Value::F64(20.0)));
        assert_eq!(machine.cycle_count, 48);
    }

    // #[test]
    // fn test_if_else_function() -> Result<()> {
    //     let mut machine = Machine::default();
    //     machine.run_from_string("(if true 1 3)")?;
    //     machine.run()?;
    //
    //     let frame = machine.get_current_frame()?;
    //     assert_eq!(frame.stack[0], Value::F64(1.0));
    //     assert_eq!(machine.cycle_count, 5);
    //
    //     let mut machine = Machine::default();
    //     machine.run_from_string("(if false 1 3)")?;
    //     machine.run()?;
    //
    //     let frame = machine.get_current_frame()?;
    //     assert_eq!(frame.stack[0], Value::F64(3.0));
    //     assert_eq!(machine.cycle_count, 4);
    //     Ok(())
    // }
    //
    // #[test]
    // fn test_let_function() -> Result<()> {
    //     let mut machine = Machine::default();
    //     machine.run_from_string("(let (x 10) x)")?;
    //     machine.run()?;
    //
    //     let frame = machine.get_current_frame()?;
    //     assert_eq!(frame.stack[0], Value::F64(10.0));
    //     assert_eq!(machine.cycle_count, 4);
    //     Ok(())
    // }
}
