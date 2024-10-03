use criterion::{black_box, criterion_group, criterion_main, Criterion};
use neolisp::vm::{Callee, Instruction, Machine, Value};

pub fn benchmark_call(c: &mut Criterion) {
    let program = vec![
        // 0x00
        Instruction::Push(Value::F64(1.)), // 0x0A
        // 0x0A
        Instruction::Push(Value::F64(1.)), // 0x0A
        // 0x14
        Instruction::Push(Value::Callable(0x1E)), // 0x06
        // 0x1A
        Instruction::Call(Callee::Function, 2), // 0x03
        // 0x1D
        Instruction::Halt, // 0x01
        // 0x1E
        Instruction::Rot,         // 0x01
        Instruction::LoadLocal,   // 0x01
        Instruction::Rot,         // 0x01
        Instruction::LoadLocal,   // 0x01
        Instruction::GetLocal(0), // 0x05?
        Instruction::GetLocal(1), // 0x05?
        Instruction::Add(2),      // 0x02
        Instruction::Rot,         // 0x01
        Instruction::Return,      // 0x01
    ]
    .iter()
    .flat_map(|i| i.to_bytecode())
    .collect::<Vec<u8>>();

    c.bench_function("((lambda (x y) (+ x y)) 1 1)", |b| {
        b.iter(|| {
            let mut machine = Machine::new(black_box(program.clone()));
            machine.run().unwrap();
        })
    });
}

criterion_group!(benches, benchmark_call,);
criterion_main!(benches);
