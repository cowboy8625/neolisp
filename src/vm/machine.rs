use super::builtin;
#[cfg(any(debug_assertions, test))]
use super::decompile::decompile;
use super::decompile::get_instruction;
use super::instruction::{Callee, Direction, Instruction};
use super::value::Value;
use core::panic;

use anyhow::{anyhow, Result};

#[cfg(debug_assertions)]
const RED: &str = "\x1b[31m";
#[cfg(any(debug_assertions, test))]
const GREEN: &str = "\x1b[32m";
#[cfg(any(debug_assertions, test))]
const UNDERLINE: &str = "\x1b[4m";
#[cfg(any(debug_assertions, test))]
const RESET: &str = "\x1b[0m";

#[cfg(debug_assertions)]
#[derive(Debug, Default)]
pub enum DebugMode {
    #[default]
    Off,
    Pause,
    PauseAndDisplay,
    Step,
    StepAndDisplay,
    Continue,
    ContinueStart,
}

const INSTRUCTION_CALL: [fn(&mut Machine) -> Result<()>; 27] = [
    instruction_start_at,
    instruction_halt,
    instruction_return,
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
    instruction_call,
    instruction_load_local,
    instruction_get_local,
    instruction_load_global,
    instruction_get_global,
    instruction_load_free,
    instruction_get_free,
    instruction_jump_if,
    instruction_jump,
];

pub struct Machine {
    pub program: Vec<u8>,
    pub ip: usize,
    pub is_running: bool,
    pub call_stack: Vec<usize>,
    pub stack: Vec<Value>,
    pub local_stack: Vec<Value>,
    pub global_stack: Vec<Value>,
    pub free_stack: Vec<Value>,
    pub breakpoints: Vec<usize>,
    #[cfg(debug_assertions)]
    pub debug_mode: DebugMode,
    #[cfg(test)]
    pub cycle_count: usize,
}

impl Machine {
    pub fn new(program: Vec<u8>) -> Self {
        Self {
            program,
            ip: 0,
            is_running: true,
            call_stack: Vec::with_capacity(2024),
            stack: Vec::with_capacity(2024),
            local_stack: Vec::with_capacity(2024),
            global_stack: Vec::with_capacity(2024),
            free_stack: Vec::with_capacity(2024),
            breakpoints: Vec::with_capacity(2024),
            #[cfg(debug_assertions)]
            debug_mode: DebugMode::Off,
            #[cfg(test)]
            cycle_count: 0,
        }
    }

    #[cfg(debug_assertions)]
    pub fn add_breakpoint(&mut self, ip: usize) {
        self.breakpoints.push(ip);
    }

    pub fn run_once(&mut self) -> Result<()> {
        #[cfg(test)]
        {
            self.cycle_count += 1;
        }

        #[cfg(debug_assertions)]
        match self.debug_mode {
            DebugMode::Off if self.breakpoints.contains(&self.ip) => {
                self.debug_mode = DebugMode::PauseAndDisplay;
                return Ok(());
            }
            DebugMode::Off => {}
            DebugMode::Pause => {
                self.debugger();
                return Ok(());
            }
            DebugMode::PauseAndDisplay => {
                self.debug_mode = DebugMode::Pause;
                self.debug();
                self.debugger();
                return Ok(());
            }
            DebugMode::Step => self.debug_mode = DebugMode::Pause,
            DebugMode::StepAndDisplay => self.debug_mode = DebugMode::PauseAndDisplay,
            DebugMode::Continue if self.breakpoints.contains(&self.ip) => {
                self.debug_mode = DebugMode::PauseAndDisplay;
                return Ok(());
            }
            DebugMode::Continue => {}
            DebugMode::ContinueStart => self.debug_mode = DebugMode::Continue,
        }

        let opcode = self.get_u8()? as usize;

        INSTRUCTION_CALL[opcode](self)?;
        Ok(())
    }

    pub fn run(&mut self) -> Result<()> {
        while self.is_running {
            self.run_once()?;
        }
        Ok(())
    }

    pub fn call(&mut self, address: usize, arg_count: usize) -> Result<()> {
        self.new_local_stack(arg_count);
        self.stack.push(Value::U32(self.ip as u32));
        self.ip = address;
        // self.bring_to_top_of_stack(arg_count);
        loop {
            let mut ip = self.ip;
            let Some(Instruction::Return) = get_instruction(&self.program, &mut ip).ok() else {
                self.run_once()?;
                continue;
            };
            self.run_once()?;
            break;
        }
        Ok(())
    }
}

impl Machine {
    fn get_callee(&mut self) -> Result<Callee> {
        let callee_opcode = self.program[self.ip];
        self.ip += 1;
        match callee_opcode {
            // Function
            // TODO: FUNCTION opcode
            0x00 => Ok(Callee::Function),
            // Builtin(String),
            // TODO: BUILTIN opcode
            0x01 => {
                let length = self.get_u8()? as usize;
                let start = self.ip;
                let end = start + length;
                let bytes = &self.program[start..end];
                let name = String::from_utf8_lossy(bytes).to_string();
                self.ip += length;
                Ok(Callee::Builtin(name))
            }
            _ => Err(anyhow!("Unknown callee type: {}", self.program[self.ip])),
        }
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
                Ok(Value::String(value))
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
                Ok(Value::List(values))
            }
            Value::CODE_CALLABLE => {
                let address = self.get_u32()? as usize;
                Ok(Value::Callable(address))
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

    fn new_local_stack(&mut self, count: usize) {
        self.call_stack.push(count);
    }

    fn leaving_local_stack(&mut self) {
        let count = self.call_stack.pop().unwrap();
        let len = self.local_stack.len();
        self.local_stack.truncate(len - count);
    }

    fn push_local(&mut self, value: Value) {
        self.local_stack.push(value);
    }

    fn get_local(&self, index: usize) -> Option<&Value> {
        let count = self.call_stack.last().unwrap();
        let start = self.local_stack.len() - count;
        let local_stack = &self.local_stack[start..];
        local_stack.get(index)
    }

    fn bring_to_top_of_stack(&mut self, count: usize) {
        let length = self.stack.len();
        self.stack[length - count..].rotate_left(count.saturating_sub(1));
    }

    fn builtins(&mut self, name: String, arg_count: u8) {
        match name.as_str() {
            "sleep" => builtin::nlvm_sleep(self, arg_count).unwrap(),
            "atom?" => builtin::nlvm_is_atom(self, arg_count).unwrap(),
            "number?" => builtin::nlvm_is_number(self, arg_count).unwrap(),
            "slice" => builtin::nlvm_slice(self, arg_count).unwrap(),
            "join" => builtin::nlvm_join(self, arg_count).unwrap(),
            "split" => builtin::nlvm_split(self, arg_count).unwrap(),
            "to-string" => builtin::nlvm_to_string(self, arg_count).unwrap(),
            "filter" => builtin::nlvm_filter(self, arg_count).unwrap(),
            "fold-right" => builtin::nlvm_fold_right(self, arg_count).unwrap(),
            "fold" => builtin::nlvm_fold(self, arg_count).unwrap(),
            "map" => builtin::nlvm_map(self, arg_count).unwrap(),
            "nth" => builtin::nlvm_nth(self, arg_count).unwrap(),
            "reverse" => builtin::nlvm_reverse(self, arg_count).unwrap(),
            "append" => builtin::nlvm_append(self, arg_count).unwrap(),
            "last" => builtin::nlvm_last(self, arg_count).unwrap(),
            "cdr" => builtin::nlvm_cdr(self, arg_count).unwrap(),
            "typeof" => builtin::nlvm_typeof(self, arg_count).unwrap(),
            "print" => builtin::nlvm_print(self, arg_count).unwrap(),
            "length" => builtin::length(self, arg_count).unwrap(),
            "assert-eq" => builtin::nlvm_assert_eq(self, arg_count).unwrap(),
            "assert" => builtin::nlvm_assert(self, arg_count).unwrap(),
            "list" => builtin::list(self, arg_count).unwrap(),
            "cons" => builtin::cons(self, arg_count).unwrap(),
            "car" => builtin::car(self, arg_count).unwrap(),
            _ => panic!("unknown builtin: {}", name),
        }
    }

    #[cfg(debug_assertions)]
    fn debugger(&mut self) {
        use std::io::Write;
        let mut input = String::new();
        let mut result = Vec::new();
        while input.is_empty() {
            print!("{RED}debugger> {RESET}");
            std::io::stdout().flush().unwrap();
            std::io::stdin().read_line(&mut input).unwrap();
            result = input.trim().split(' ').collect::<Vec<_>>();
        }
        match result[0] {
            "d" | "display" => self.debug(),
            "h" | "help" => {
                println!("h  help");
                println!("pfs  print-free-stack");
                println!("pls  print-local-stack");
                println!("pgs  print-global-stack");
                println!("ps   print-stack");
                println!("p    print <index>");
                println!("b    breakpoint <ip>");
                println!("c    continue");
                println!("n    next");
                println!("rot  rotate <count>");
                println!("ip   shows ip address");
                println!("nd   run <next> and <display>");
                println!("q    quit");
            }
            "pfs" | "print-free-stack" => {
                println!("FREE STACK:");
                for (i, item) in self.free_stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            // HACK: the local stack should be shared with the main stack.
            "pls" | "print-local-stack" => {
                let Some(count) = self.call_stack.last() else {
                    println!("LOCAL STACK EMPTY");
                    return;
                };
                println!("LOCAL STACK {count}:");
                let start = self.local_stack.len().saturating_sub(*count);
                for (i, item) in self.local_stack[start..].iter().rev().enumerate() {
                    println!("0x{i:02X}: {item:?}");
                }
            }
            "pals" | "print-all-local-stack" => {
                for (i, item) in self.local_stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item:?}");
                }
            }
            "pgs" | "print-global-stack" => {
                println!("GLOBAL STACK:");
                for (i, item) in self.global_stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            "ps" | "print-stack" => {
                println!("STACK:");
                for (i, item) in self.stack.iter().rev().enumerate() {
                    println!("0x{i:02X}: {item}");
                }
            }
            "p" | "print" if result.len() == 2 => {
                let Ok(stack_index) = result[1].parse::<usize>() else {
                    println!("print <index> not {RED}{}{RESET}", result[1]);
                    return;
                };

                if stack_index >= self.stack.len() {
                    println!(
                        "The stack size is {} but your index is out of range {}",
                        self.stack.len(),
                        stack_index
                    );
                    return;
                }

                let item = &self.stack[self.stack.len() - 1 - stack_index];
                println!("{item}");
            }
            "b" | "breakpoint" if result.len() == 2 => {
                let Ok(ip) = result[1].parse::<usize>() else {
                    println!("breakpoint <ip> not {RED}{}{RESET}", result[1]);
                    return;
                };
                self.add_breakpoint(ip);
            }
            "p" | "print" => eprintln!("print <index>"),
            "c" | "continue" => self.debug_mode = DebugMode::ContinueStart,
            "n" | "next" => self.debug_mode = DebugMode::Step,
            "rot" if result.len() == 2 => {
                let Ok(count) = result[1].parse::<usize>() else {
                    println!("rotate <count> not {RED}{}{RESET}", result[1]);
                    return;
                };
                println!("ROT BY: {count}");
                println!("{:?}", self.stack);
                let mut temp = Vec::new();
                for _ in 0..count {
                    temp.push(self.stack.pop().unwrap());
                }
                for item in temp.into_iter() {
                    self.stack.push(item);
                }
                println!("{:?}", self.stack);
            }
            "ip" => println!("ip: 0x{:02X}, {0}", self.ip),
            "nd" => self.debug_mode = DebugMode::StepAndDisplay,
            "q" | "quit" => self.shutdown(),
            _ => println!("{RED}unknown command{RESET} {input}"),
        }
    }

    #[cfg(any(debug_assertions, test))]
    pub fn debug(&self) {
        let instructions = decompile(&self.program);
        let mut offset = 0;
        for int in instructions.iter() {
            let selected = if self.ip == offset {
                format!("{GREEN}{UNDERLINE}0x{offset:02X} {offset:>2} ")
            } else {
                format!("0x{offset:02X} {offset:>2} ")
            };
            let breakpoint = if self.breakpoints.contains(&offset) {
                "🔴".to_string()
            } else {
                "  ".to_string()
            };
            eprintln!("{breakpoint}{selected} {int:?}{RESET}");
            offset += int.size();
        }
    }

    #[cfg(debug_assertions)]
    fn shutdown(&mut self) {
        self.is_running = false;
    }
}

fn instruction_start_at(machine: &mut Machine) -> Result<()> {
    let address = machine.get_u32()? as usize;
    machine.ip = address;
    Ok(())
}

fn instruction_halt(machine: &mut Machine) -> Result<()> {
    machine.is_running = false;
    Ok(())
}

fn instruction_return(machine: &mut Machine) -> Result<()> {
    let Some(Value::U32(address)) = machine.stack.pop() else {
        // TODO: REPORT ERROR
        panic!("expected value on stack for return")
    };
    machine.ip = address as usize;
    machine.leaving_local_stack();
    Ok(())
}
fn instruction_push(machine: &mut Machine) -> Result<()> {
    let value = machine.get_value()?;
    machine.stack.push(value);
    Ok(())
}

fn instruction_add(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let mut left = args[0].clone();
    for right in args.iter().skip(1) {
        match (left, right) {
            (Value::I32(l), Value::I32(r)) => {
                left = Value::I32(l + r);
            }
            (Value::F64(l), Value::F64(r)) => {
                left = Value::F64(l + r);
            }
            (Value::String(l), Value::String(r)) => {
                left = Value::String(format!("{l}{r}"));
            }
            (lhs, rhs) => panic!(
                "invalid types for Add: {lhs:?} and {rhs:?}, {:#?}",
                machine.stack
            ),
        }
    }
    machine.stack.push(left);
    Ok(())
}

fn instruction_sub(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
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
    machine.stack.push(left);
    Ok(())
}

fn instruction_mul(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
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
    machine.stack.push(left);
    Ok(())
}

fn instruction_div(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
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
    machine.stack.push(left);
    Ok(())
}

fn instruction_eq(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let left = &args[0];
    let value = args.iter().skip(1).all(|right| match (left, right) {
        (Value::I32(l), Value::I32(r)) => l == r,
        (Value::F64(l), Value::F64(r)) => l == r,
        _ => panic!("invalid types for Less Than"),
    });
    machine.stack.push(Value::Bool(value));
    Ok(())
}

fn instruction_greater_than(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let left = &args[0];
    let value = args.iter().skip(1).all(|right| match (left, right) {
        (Value::I32(l), Value::I32(r)) => l > r,
        (Value::F64(l), Value::F64(r)) => l > r,
        _ => panic!("invalid types for Less Than"),
    });
    machine.stack.push(Value::Bool(value));
    Ok(())
}

fn instruction_less_than(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let left = &args[0];
    let value = args.iter().skip(1).all(|right| match (left, right) {
        (Value::I32(l), Value::I32(r)) => l < r,
        (Value::F64(l), Value::F64(r)) => l < r,
        _ => panic!("invalid types for Less Than"),
    });
    machine.stack.push(Value::Bool(value));
    Ok(())
}

fn instruction_greater_than_or_equal(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let left = &args[0];
    let value = args.iter().skip(1).all(|right| match (left, right) {
        (Value::I32(l), Value::I32(r)) => l >= r,
        (Value::F64(l), Value::F64(r)) => l >= r,
        _ => panic!("invalid types for Greater Than Or Equal"),
    });
    machine.stack.push(Value::Bool(value));
    Ok(())
}

fn instruction_less_than_or_equal(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let left = &args[0];
    let value = args.iter().skip(1).all(|right| match (left, right) {
        (Value::I32(l), Value::I32(r)) => l <= r,
        (Value::F64(l), Value::F64(r)) => l <= r,
        _ => panic!("invalid types for Less Than Or Equal"),
    });
    machine.stack.push(Value::Bool(value));
    Ok(())
}

fn instruction_and(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let left = &args[0];
    let value = args.iter().skip(1).all(|right| match (left, right) {
        (Value::Bool(l), Value::Bool(r)) => *l && *r,
        _ => panic!("invalid types for And"),
    });
    machine.stack.push(Value::Bool(value));
    Ok(())
}

fn instruction_or(machine: &mut Machine) -> Result<()> {
    let count = machine.get_u8()?;
    let args = machine
        .stack
        .split_off(machine.stack.len() - count as usize);
    let left = &args[0];
    let value = args
        .iter()
        .skip(1)
        .fold(false, |acc, right| match (left, right) {
            (Value::Bool(l), Value::Bool(r)) => acc || *l || *r,
            _ => panic!("invalid types for Or"),
        });
    machine.stack.push(Value::Bool(value));
    Ok(())
}

fn instruction_not(machine: &mut Machine) -> Result<()> {
    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for Not")
    };
    match value {
        Value::Bool(b) => {
            machine.stack.push(Value::Bool(!b));
        }
        _ => panic!("invalid types for Not"),
    }
    Ok(())
}

fn instruction_mod(machine: &mut Machine) -> Result<()> {
    let Some(right) = machine.stack.pop() else {
        panic!("expected value on stack for Mod")
    };
    let Some(left) = machine.stack.last() else {
        panic!("expected value on stack for Mod")
    };
    let last_index = machine.stack.len() - 1;
    match (left, right) {
        (Value::I32(left), Value::I32(right)) => {
            machine.stack[last_index] = Value::I32(left % right);
        }
        (Value::F64(left), Value::F64(right)) => {
            machine.stack[last_index] = Value::F64(left % right);
        }
        _ => panic!("invalid types for Mod"),
    }
    Ok(())
}

fn instruction_rot(machine: &mut Machine) -> Result<()> {
    machine.bring_to_top_of_stack(2);
    Ok(())
}

fn instruction_call(machine: &mut Machine) -> Result<()> {
    let callee = machine.get_callee()?;
    let arg_count = machine.get_u8()?;
    match callee.clone() {
        Callee::Function => {
            let address = match machine.stack.pop() {
                Some(Value::Callable(address)) => address,
                None => {
                    panic!("expected value on stack for function call but stack is empty")
                }
                value => {
                    panic!(
                        "expected value on stack for function call but got {:?} {:#?}",
                        value, machine.stack
                    )
                }
            };

            machine.new_local_stack(arg_count as usize);
            let location = machine.stack.len() - arg_count as usize;
            machine
                .stack
                .insert(location, Value::U32(machine.ip as u32));
            machine.ip = address;
        }
        Callee::Builtin(name) => machine.builtins(name.clone(), arg_count),
    }
    Ok(())
}

fn instruction_load_local(machine: &mut Machine) -> Result<()> {
    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for LoadLocal")
    };

    machine.push_local(value);
    Ok(())
}

fn instruction_get_local(machine: &mut Machine) -> Result<()> {
    let index = machine.get_u8()? as usize;
    let Some(value) = machine.get_local(index) else {
        panic!("expected value on stack for GetLocal")
    };
    machine.stack.push(value.clone());
    Ok(())
}

fn instruction_load_global(machine: &mut Machine) -> Result<()> {
    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for LoadGlobal")
    };

    machine.global_stack.push(value);
    Ok(())
}

fn instruction_get_global(machine: &mut Machine) -> Result<()> {
    let index = machine.get_u8()? as usize;
    let Some(value) = machine.global_stack.get(index) else {
        panic!("expected value on stack for GetGlobal")
    };
    machine.stack.push(value.clone());
    Ok(())
}

fn instruction_load_free(machine: &mut Machine) -> Result<()> {
    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for LoadFree")
    };

    machine.free_stack.push(value);
    Ok(())
}

fn instruction_get_free(machine: &mut Machine) -> Result<()> {
    let index = machine.get_u8()? as usize;
    let Some(value) = machine.free_stack.get(index) else {
        panic!("expected value on stack for GetFree")
    };
    machine.stack.push(value.clone());
    Ok(())
}

fn instruction_jump_if(machine: &mut Machine) -> Result<()> {
    let address = machine.get_u32()? as usize;
    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for JumpIf")
    };
    if value == Value::Bool(false) {
        machine.ip += address;
    }
    Ok(())
}

fn instruction_jump(machine: &mut Machine) -> Result<()> {
    let direction = machine.get_u8()?;
    let address = machine.get_u32()? as usize;
    match direction {
        Direction::OPCODE_FORWARD => {
            machine.ip += address;
        }
        Direction::OPCODE_BACKWARD => {
            machine.ip -= address;
        }
        _ => panic!("invalid direction for Jump"),
    }
    Ok(())
}
