use criterion::{black_box, criterion_group, criterion_main, Criterion};
use neolisp::instruction::{Callable, Instruction, RuntimeMetadata, Value};
use neolisp::machine::Machine;
use neolisp::symbol_table::SymbolTable;

pub fn benchmark_call(c: &mut Criterion) {
    let program = vec![
        Instruction::Push(Box::new(Value::F64(1.0))), // 0x00   0   02 04 00 00 00 00 00 00 F0 3F
        Instruction::Push(Box::new(Value::F64(1.0))), // 0x0A  10   02 04 00 00 00 00 00 00 F0 3F
        Instruction::Jump(38),                        // 0x14  20   1C 26 00 00 00
        Instruction::GetLocal(RuntimeMetadata::new(0, "x")), // 0x19  25   16 00 00 00 00
        Instruction::GetLocal(RuntimeMetadata::new(1, "y")), // 0x1E  30   16 01 00 00 00
        Instruction::Add(2),                          // 0x23  35   03 02
        Instruction::Return,                          // 0x25  37   01
        Instruction::Push(Box::new(Value::Callable(Box::new(Callable::new(
            25, "apply",
        ))))), // 0x26  38   02 08 19 00 00 00
        Instruction::Call(2),                         // 0x2C  44   11 02
    ]
    .iter()
    .flat_map(|i: &Instruction| i.to_bytecode())
    .collect::<Vec<u8>>();

    c.bench_function("((lambda (x y) (+ x y)) 1 1)", |b| {
        b.iter(|| {
            let mut machine = Machine::new(
                black_box(program.clone()),
                black_box(SymbolTable::default()),
            );
            machine.run().unwrap();
        })
    });
}

criterion_group!(benches, benchmark_call,);
criterion_main!(benches);
