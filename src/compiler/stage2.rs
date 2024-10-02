use super::{
    Chunk, IState, Stage1Callee, Stage1Data, Stage1Function, Stage1Instruction, Stage1Lambda,
    Stage1Value,
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
        *location += function.params.size();
        *location += function.prelude.size();
        *location += function.body.size();
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
        *location += lambda.params.size();
        *location += lambda.body.size();
    }
}

fn convert_section_to_instructions(symbol_table: &SymbolTable, data: &Chunk) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = Vec::new();
    for instruction in data.items.iter() {
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
        Stage1Instruction::Halt => Instruction::Halt,
        Stage1Instruction::Return => Instruction::Return,
        Stage1Instruction::Push(stage1_value) => {
            Instruction::Push(stage1_value_into_value(symbol_table, stage1_value))
        }
        Stage1Instruction::Add(c) => Instruction::Add(*c),
        Stage1Instruction::Sub(c) => Instruction::Sub(*c),
        Stage1Instruction::Mul(c) => Instruction::Mul(*c),
        Stage1Instruction::Div => Instruction::Div,
        Stage1Instruction::Eq => Instruction::Eq,
        Stage1Instruction::GreaterThan => Instruction::GreaterThan,
        Stage1Instruction::LessThan => Instruction::LessThan,
        Stage1Instruction::GreaterThanOrEqual => Instruction::GreaterThanOrEqual,
        Stage1Instruction::LessThanOrEqual => Instruction::LessThanOrEqual,
        Stage1Instruction::And => Instruction::And,
        Stage1Instruction::Or => Instruction::Or,
        Stage1Instruction::Not => Instruction::Not,
        Stage1Instruction::Mod => Instruction::Mod,
        Stage1Instruction::Rot => Instruction::Rot,
        Stage1Instruction::Call(Stage1Callee::Function, count) => {
            Instruction::Call(Callee::Function, *count)
        }
        Stage1Instruction::Call(Stage1Callee::Builtin(name), count) => {
            Instruction::Call(Callee::Builtin(name.clone()), *count)
        }
        Stage1Instruction::LoadLocal => Instruction::LoadLocal,
        Stage1Instruction::GetLocal(IState::Set(v)) => Instruction::GetLocal(*v),
        Stage1Instruction::GetLocal(IState::Unset(v)) => {
            let Some(symbol) = symbol_table.lookup(v) else {
                panic!("Variable `{}` is not defined", v);
            };

            Instruction::GetLocal(symbol.id)
        }
        Stage1Instruction::LoadGlobal => Instruction::LoadGlobal,
        Stage1Instruction::GetGlobal(IState::Set(v)) => Instruction::GetGlobal(*v),
        Stage1Instruction::GetGlobal(IState::Unset(v)) => todo!("GetGlobal({})", v),
        Stage1Instruction::LoadFree => Instruction::LoadFree,
        Stage1Instruction::GetFree(IState::Set(v)) => Instruction::GetFree(*v),
        Stage1Instruction::GetFree(IState::Unset(v)) => todo!("GetFree({})", v),
        Stage1Instruction::JumpIf(usize) => Instruction::JumpIf(*usize),
        Stage1Instruction::Jump(usize) => Instruction::Jump(*usize),
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
    let mut location = 5;
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
    // TODO: MAYBE we need to insert a main function if no_main is true
    for function in &data.functions {
        symbol_table.enter_scope(&function.name);
        let params = convert_section_to_instructions(symbol_table, &function.params);
        instructions.extend(params);
        let prelude = convert_section_to_instructions(symbol_table, &function.prelude);
        instructions.extend(prelude);
        let body = convert_section_to_instructions(symbol_table, &function.body);
        instructions.extend(body);
        symbol_table.exit_scope();
    }

    for lambda in &data.lambdas {
        symbol_table.enter_scope(&lambda.name);
        let params = convert_section_to_instructions(symbol_table, &lambda.params);
        instructions.extend(params);
        let body = convert_section_to_instructions(symbol_table, &lambda.body);
        instructions.extend(body);
        symbol_table.exit_scope();
    }

    instructions
}
