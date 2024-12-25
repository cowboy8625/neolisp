use criterion::{black_box, criterion_group, criterion_main, Criterion};
use neolisp::machine::Machine;

pub fn fibbonacci_recursive(c: &mut Criterion) {
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

pub fn fibbonacci_tail_call(c: &mut Criterion) {
    let src = r#"
(fn fib2 (n a b)
  (if (= n 0)
    a
    (fib2 (- n 1) b (+ a b))))
(fn main () (print (fib2 30 0 1) "\n"))
"#;

    c.bench_function("Machine::new", |b| {
        b.iter(|| {
            let mut machine = Machine::default();
            machine.run_from_string(black_box(src)).unwrap();
        })
    });
}

pub fn range_function(c: &mut Criterion) {
    let src = r#"
(fn range-helper (n acc)
  (if (<= n 0)
      acc
      (range-helper (- n 1) (cons (- n 1) acc))))

(fn range (n)
  (range-helper n '()))

(fn main () (range 30_000))
"#;

    c.bench_function("Machine::new", |b| {
        b.iter(|| {
            let mut machine = Machine::default();
            machine.run_from_string(black_box(src)).unwrap();
        })
    });
}

criterion_group!(
    benches,
    fibbonacci_recursive,
    fibbonacci_tail_call,
    range_function
);
criterion_main!(benches);
