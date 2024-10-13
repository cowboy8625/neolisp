use criterion::{black_box, criterion_group, criterion_main, Criterion};
use neolisp::instruction::{Instruction, Value};
use neolisp::machine::Machine;

macro_rules! operator_benchmark {
    ($func_name:ident, $test_name:literal, $instructions:expr) => {
        pub fn $func_name(c: &mut Criterion) {
            let program = $instructions
                .iter()
                .flat_map(|i| i.to_bytecode())
                .collect::<Vec<u8>>();

            c.bench_function($test_name, |b| {
                b.iter(|| {
                    Machine::new(black_box(program.clone())).run().unwrap();
                })
            });
        }
    };
}

operator_benchmark!(
    benchmark_add,
    "(+ 1 1 1 1 1 1 1 1 1 1)",
    vec![
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Push(Box::new(Value::F64(1.))),
        Instruction::Add(10),
    ]
);

operator_benchmark!(
    benchmark_sub,
    "(- 1000 500 250 125 75 35 7 8)",
    vec![
        Instruction::Push(Box::new(Value::F64(1000.))),
        Instruction::Push(Box::new(Value::F64(500.))),
        Instruction::Push(Box::new(Value::F64(250.))),
        Instruction::Push(Box::new(Value::F64(125.))),
        Instruction::Push(Box::new(Value::F64(75.))),
        Instruction::Push(Box::new(Value::F64(35.))),
        Instruction::Push(Box::new(Value::F64(7.))),
        Instruction::Push(Box::new(Value::F64(8.))),
        Instruction::Sub(8),
    ]
);

operator_benchmark!(
    benchmark_mul,
    "(* 2 2 2 2 2 2 2 2 2 2)",
    vec![
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Mul(10),
    ]
);

operator_benchmark!(
    benchmark_div,
    "(/ 1_000 2 2 2 2 2 2 2 2 2)",
    vec![
        Instruction::Push(Box::new(Value::F64(1_000.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Div(10),
    ]
);

operator_benchmark!(
    benchmark_gt,
    "(> 1_000 2 2 2 2 2 2 2 2 2)",
    vec![
        Instruction::Push(Box::new(Value::F64(1_000.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::GreaterThan(10),
    ]
);

operator_benchmark!(
    benchmark_lt,
    "(< 1_000 2 2 2 2 2 2 2 2 2)",
    vec![
        Instruction::Push(Box::new(Value::F64(1_000.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::LessThan(10),
    ]
);

operator_benchmark!(
    benchmark_gte,
    "(>= 1_000 2 2 2 2 2 2 2 2 2)",
    vec![
        Instruction::Push(Box::new(Value::F64(1_000.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::GreaterThanOrEqual(10),
    ]
);

operator_benchmark!(
    benchmark_lte,
    "(<= 1_000 2 2 2 2 2 2 2 2 2)",
    vec![
        Instruction::Push(Box::new(Value::F64(1_000.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::Push(Box::new(Value::F64(2.))),
        Instruction::LessThanOrEqual(10),
    ]
);

criterion_group!(
    benches,
    benchmark_add,
    benchmark_sub,
    benchmark_mul,
    benchmark_div,
    benchmark_gt,
    benchmark_lt,
    benchmark_gte,
    benchmark_lte,
);
criterion_main!(benches);
