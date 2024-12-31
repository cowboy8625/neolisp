use crate::parser;
use chumsky::Parser;

macro_rules! snapshot_parse {
    ($name:tt, $contents:tt) => {
        #[test]
        fn $name() {
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                let ast = format!("{:#?}", parser::parser().parse($contents));
                insta::assert_snapshot!(ast);
            });
        }
    };
}
snapshot_parse!(parse_test_ident1, "(hello-world)");
snapshot_parse!(parse_test_ident2, "(*ident*)");
snapshot_parse!(parse_test_ident3, "(<*ident/1234567890*>!?)");
snapshot_parse!(
    parse_test_bool_comment,
    "
; comment
(true)"
);
snapshot_parse!(parse_test_bool_true, "(true)");
snapshot_parse!(parse_test_bool_false, "(false)");
snapshot_parse!(parse_test_number, "(1)");
snapshot_parse!(parse_test_string, r#"("hello")"#);
snapshot_parse!(parse_test_string_empty, r#"("")"#);
snapshot_parse!(parse_test_string_ecape_char, r#"("\"")"#);
snapshot_parse!(parse_test_string_newline, r#"("hello\n")"#);
snapshot_parse!(parse_test_string_tab, r#"("hello\t")"#);
snapshot_parse!(parse_test_symbol_plus, "(+ 1 2)");
snapshot_parse!(parse_test_symbol_minus, "(+ 1 2)");
snapshot_parse!(parse_test_symbol_gt, "(> 1 2)");
snapshot_parse!(parse_test_symbol_lt, "(< 1 2)");
snapshot_parse!(parse_test_symbol_lte, "(<= 1 2)");
snapshot_parse!(parse_test_symbol_gte, "(>= 1 2)");
snapshot_parse!(parse_test_list, "((+ 1 2) (+ 1 2))");
snapshot_parse!(parse_test_lambda, "(lambda (x y) (+ x y))");
snapshot_parse!(parse_test_def_var, "(var x 1)");
snapshot_parse!(parse_test_def_func, "(fn add (x y) (+ x y))");
snapshot_parse!(parse_test_quote, "(var x '(1 2 3))");
snapshot_parse!(
    parse_test_program_with_many_single_list,
    " (x y) (+ x y) (x y z) "
);

macro_rules! snapshot_emit {
    ($name:tt, $contents:tt) => {
        #[test]
        fn $name() {
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                let mut symbol_table = crate::symbol_table::SymbolTable::default();

                let Ok(Some(instructions)) = crate::compiler::Compiler::default()
                    .no_main(true)
                    .compile($contents, &mut symbol_table)
                else {
                    let msg = format!("Failed to compile: '{}'\n{}", stringify!($name), $contents);
                    return insta::assert_snapshot!(msg);
                };
                let string = crate::compiler::format_instructions(&instructions);

                insta::assert_snapshot!(string);
            });
        }
    };
    (enable_test $name:tt, $contents:tt) => {
        #[test]
        fn $name() {
            let mut settings = insta::Settings::clone_current();
            settings.set_snapshot_path("testdata/output/");
            settings.bind(|| {
                let mut symbol_table = crate::symbol_table::SymbolTable::default();

                let Ok(Some(instructions)) = crate::compiler::Compiler::default()
                    .no_main(true)
                    .with_test(true)
                    .compile($contents, &mut symbol_table)
                else {
                    let msg = format!("Failed to compile: '{}'\n{}", stringify!($name), $contents);
                    return insta::assert_snapshot!(msg);
                };
                let string = crate::compiler::format_instructions(&instructions);

                insta::assert_snapshot!(string);
            });
        }
    };
}

snapshot_emit!(emit_nil, "nil");
snapshot_emit!(emit_float, "1");
snapshot_emit!(emit_string, r#""hello world\n""#);
snapshot_emit!(emit_quote, "'(1 2 3)");
snapshot_emit!(emit_add_function, "(fn add (x y) (+ x y))");
snapshot_emit!(emit_simple_let, "(let (x 0) (+ 1 x))");
snapshot_emit!(
    emit_let,
    r#"
    (let (x 1)
    (let (y 2)
    (let (z 3)
    (print x y z "\n"))))"#
);
snapshot_emit!(emit_if_else, "(if true 'then 'else)");
snapshot_emit!(
    emit_apply_function,
    "(fn apply (f x) (f x))
(apply (lambda (x) (+ x 321)) 123)"
);
snapshot_emit!(
    emit_fib_function,
    "(fn fib (n a b)
  (if (= n 0)
      a
      (fib (- n 1) b (+ a b))))"
);
snapshot_emit!(emit_run_code, "(test test-testing (assert (= (+ 1 2) 3)))");
snapshot_emit!(enable_test emit_test_code, "(test test-testing (assert (= (+ 1 2) 3)))");
