use chumsky::prelude;

use super::{
    IState, Stage1Callee, Stage1Data, Stage1Function, Stage1Instruction, Stage1Lambda, Stage1Value,
};
use crate::symbol_table::SymbolTable;
use crate::vm::{Callee, Instruction, Value};

fn set_location_of_functions_in_symbol_table(
    symbol_table: &mut SymbolTable,
    functions: &Vec<Stage1Function>,
    location: &mut usize,
) {
    for function in functions {
        let name = function.name.as_str();
        symbol_table.set_location(Some(name), name, *location as u32);
        *location += function.params.len();
        *location += function.prelude.len();
        *location += function.body.len();
    }
}

fn set_location_of_lambdas_in_symbol_table(
    symbol_table: &mut SymbolTable,
    lambdas: &Vec<Stage1Lambda>,
    location: &mut usize,
) {
    for lambda in lambdas {
        let name = lambda.name.as_str();
        symbol_table.set_location(Some(name), name, *location as u32);
        *location += lambda.params.len();
        *location += lambda.body.len();
    }
}

fn convert_section_to_instructions(
    symbol_table: &SymbolTable,
    data: &[Stage1Instruction],
) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();
    for instruction in data {
        into_instructions(symbol_table, instruction, &mut instructions);
    }
    instructions
}

fn into_instructions(
    symbol_table: &SymbolTable,
    instruction: &Stage1Instruction,
    out: &mut Vec<Instruction>,
) {
    let i = match instruction {
        Stage1Instruction::StartAt(_) => unreachable!(),
        Stage1Instruction::Noop => Instruction::Noop,
        Stage1Instruction::Halt => Instruction::Halt,
        Stage1Instruction::Return => Instruction::Return,
        Stage1Instruction::Push(stage1_value) => {
            Instruction::Push(stage1_value_into_value(symbol_table, stage1_value))
        }
        Stage1Instruction::Add => Instruction::Add,
        Stage1Instruction::Rot => Instruction::Rot,
        Stage1Instruction::Call(Stage1Callee::Function, count) => {
            Instruction::Call(Callee::Function, *count)
        }
        Stage1Instruction::Call(Stage1Callee::Builtin(name), count) => {
            Instruction::Call(Callee::Builtin(name.clone()), *count)
        }
        Stage1Instruction::LoadLocal => Instruction::LoadLocal,
        Stage1Instruction::GetLocal(IState::Set(v)) => Instruction::GetLocal(*v),
        Stage1Instruction::GetLocal(IState::Unset(v)) => todo!("GetLocal({})", v),
        Stage1Instruction::LoadGlobal => Instruction::LoadGlobal,
        Stage1Instruction::GetGlobal(IState::Set(v)) => Instruction::GetGlobal(*v),
        Stage1Instruction::GetGlobal(IState::Unset(v)) => todo!("GetGlobal({})", v),
        Stage1Instruction::LoadFree => Instruction::LoadFree,
        Stage1Instruction::GetFree(IState::Set(v)) => Instruction::GetFree(*v),
        Stage1Instruction::GetFree(IState::Unset(v)) => todo!("GetFree({})", v),
    };
    out.push(i);
}

fn stage1_value_into_value(symbol_table: &SymbolTable, stage1_value: &Stage1Value) -> Value {
    match stage1_value {
        Stage1Value::U8(v) => Value::U8(*v),
        Stage1Value::I32(v) => Value::I32(*v),
        Stage1Value::U32(v) => Value::U32(*v),
        Stage1Value::F32(v) => Value::F32(*v),
        Stage1Value::F64(v) => Value::F64(*v),
        Stage1Value::String(v) => Value::String(v.clone()),
        Stage1Value::Bool(v) => Value::Bool(*v),
        Stage1Value::List(v) => Value::List(
            v.iter()
                .map(|v| stage1_value_into_value(symbol_table, v))
                .collect(),
        ),
        Stage1Value::Callable(IState::Set(v)) => Value::Callable(*v),
        Stage1Value::Callable(IState::Unset(v)) => {
            let Some(symbol) = symbol_table.lookup(v) else {
                panic!("Symbol not found: {v}");
            };

            let Some(location) = symbol.location else {
                panic!("Symbol location not set: {v}");
            };

            Value::Callable(location as usize)
        }
    }
}

pub fn compile_to_instructions(
    symbol_table: &mut SymbolTable,
    data: &Stage1Data,
) -> Vec<Instruction> {
    let mut location = 1;
    set_location_of_functions_in_symbol_table(symbol_table, &data.functions, &mut location);
    set_location_of_lambdas_in_symbol_table(symbol_table, &data.lambdas, &mut location);

    let mut instructions: Vec<Instruction> = Vec::new();
    let Some(start_symbol) = symbol_table.lookup("main") else {
        panic!("Symbol not found: main");
    };
    let Some(start_location) = start_symbol.location else {
        panic!("Symbol location not set: main");
    };

    instructions.push(Instruction::StartAt(start_location as usize));
    for function in &data.functions {
        let params = convert_section_to_instructions(&symbol_table, &function.params);
        instructions.extend(params);
        let prelude = convert_section_to_instructions(&symbol_table, &function.prelude);
        instructions.extend(prelude);
        let body = convert_section_to_instructions(&symbol_table, &function.body);
        instructions.extend(body);
    }

    for lambda in &data.lambdas {
        let params = convert_section_to_instructions(&symbol_table, &lambda.params);
        instructions.extend(params);
        let body = convert_section_to_instructions(&symbol_table, &lambda.body);
        instructions.extend(body);
    }

    instructions
}
