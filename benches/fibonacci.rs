use criterion::{black_box, criterion_group, criterion_main, Criterion};
use neolisp::{
    compiler::{compile, CompilerOptions},
    vm::Machine,
};

// TODO: Break this down
pub fn criterion_benchmark(c: &mut Criterion) {
    let src = r#"
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fn main () (fib 30))
"#;

    let options = CompilerOptions::default();
    c.bench_function("Machine::new", |b| {
        b.iter(|| {
            let program = compile(&src, &options).unwrap();
            let mut machine = Machine::new(black_box(program));
            machine.run().unwrap();
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
