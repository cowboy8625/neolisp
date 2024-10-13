use criterion::{black_box, criterion_group, criterion_main, Criterion};
use neolisp::machine::Machine;

// TODO: Break this down
pub fn criterion_benchmark(c: &mut Criterion) {
    let src = r#"
(fn fib (n)
  (if (or (= n 0) (= n 1))
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(fn main () (fib 30))
"#;

    c.bench_function("Machine::new", |b| {
        b.iter(|| {
            let mut machine = Machine::default();
            machine.run_from_string(black_box(src)).unwrap();
        })
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
