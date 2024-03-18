use crate::environment::Env;
use crate::eval::eval;
use crate::parser::{parse_expr, parser, Expr};
use chumsky::prelude::*;
use pretty_assertions::assert_eq;

macro_rules! test_parser {
    (parser, $name:ident, $src:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let ast = parser().parse($src).unwrap();
            assert_eq!(ast, $expected);
        }
    };
    ($name:ident, $src:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let ast = parse_expr().parse($src).unwrap();
            assert_eq!(ast, $expected);
        }
    };
}

test_parser!(
    parse_test_ident1,
    "
(hello-world)
",
    Expr::List(vec![Expr::Symbol("hello-world".to_string())])
);
test_parser!(
    parse_test_ident2,
    "
(*ident*)
",
    Expr::List(vec![Expr::Symbol("*ident*".to_string())])
);
test_parser!(
    parse_test_ident3,
    "
(<*ident/1234567890*>!?)
",
    Expr::List(vec![Expr::Symbol("<*ident/1234567890*>!?".to_string())])
);
test_parser!(
    parse_test_bool_comment,
    "
; comment
(true)
",
    Expr::List(vec![Expr::Bool(true)])
);
test_parser!(
    parse_test_bool_true,
    "(true)",
    Expr::List(vec![Expr::Bool(true)])
);
test_parser!(
    parse_test_bool_false,
    "(false)",
    Expr::List(vec![Expr::Bool(false)])
);
test_parser!(parse_test_number, "(1)", Expr::List(vec![Expr::Number(1.)]));
test_parser!(
    parse_test_string,
    r#"("hello")"#,
    Expr::List(vec![Expr::String("hello".to_string())])
);
test_parser!(
    parse_test_string_newline,
    r#"("hello\n")"#,
    Expr::List(vec![Expr::String("hello\n".to_string())])
);
test_parser!(
    parse_test_string_tab,
    r#"("hello\t")"#,
    Expr::List(vec![Expr::String("hello\t".to_string())])
);
test_parser!(
    parse_test_symbol_plus,
    "(+ 1 2)",
    Expr::List(vec![
        Expr::Symbol("+".to_string()),
        Expr::Number(1.),
        Expr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_minus,
    "(+ 1 2)",
    Expr::List(vec![
        Expr::Symbol("+".to_string()),
        Expr::Number(1.),
        Expr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_gt,
    "(> 1 2)",
    Expr::List(vec![
        Expr::Symbol(">".to_string()),
        Expr::Number(1.),
        Expr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_lt,
    "(< 1 2)",
    Expr::List(vec![
        Expr::Symbol("<".to_string()),
        Expr::Number(1.),
        Expr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_lte,
    "(<= 1 2)",
    Expr::List(vec![
        Expr::Symbol("<=".to_string()),
        Expr::Number(1.),
        Expr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_gte,
    "(>= 1 2)",
    Expr::List(vec![
        Expr::Symbol(">=".to_string()),
        Expr::Number(1.),
        Expr::Number(2.)
    ])
);
test_parser!(
    parse_test_list,
    "((+ 1 2) (+ 1 2))",
    Expr::List(vec![
        Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::Number(1.),
            Expr::Number(2.)
        ]),
        Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::Number(1.),
            Expr::Number(2.)
        ])
    ])
);
test_parser!(
    parse_test_lambda,
    "(lambda (x y) (+ x y))",
    Expr::List(vec![
        Expr::Symbol("lambda".to_string()),
        Expr::List(vec![
            Expr::Symbol("x".to_string()),
            Expr::Symbol("y".to_string())
        ]),
        Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Symbol("y".to_string())
        ]),
    ])
);
test_parser!(
    parse_test_def_var,
    "(var x 1)",
    Expr::List(vec![
        Expr::Symbol("var".to_string()),
        Expr::Symbol("x".to_string()),
        Expr::Number(1.)
    ])
);
test_parser!(
    parse_test_def_func,
    "(fn add (x y) (+ x y))",
    Expr::List(vec![
        Expr::Symbol("fn".to_string()),
        Expr::Symbol("add".to_string()),
        Expr::List(vec![
            Expr::Symbol("x".to_string()),
            Expr::Symbol("y".to_string())
        ]),
        Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Symbol("y".to_string())
        ])
    ])
);
test_parser!(
    parse_test_quote,
    "(var x '(1 2 3))",
    Expr::List(vec![
        Expr::Symbol("var".to_string()),
        Expr::Symbol("x".to_string()),
        Expr::List(vec![
            Expr::Symbol("quote".to_string()),
            Expr::List(vec![Expr::Number(1.), Expr::Number(2.), Expr::Number(3.)])
        ]),
    ])
);
test_parser!(
    parser,
    parse_test_program_with_many_single_list,
    " (x y) (+ x y) (x y z) ",
    vec![
        Expr::List(vec![
            Expr::Symbol("x".to_string()),
            Expr::Symbol("y".to_string()),
        ]),
        Expr::List(vec![
            Expr::Symbol("+".to_string()),
            Expr::Symbol("x".to_string()),
            Expr::Symbol("y".to_string()),
        ]),
        Expr::List(vec![
            Expr::Symbol("x".to_string()),
            Expr::Symbol("y".to_string()),
            Expr::Symbol("z".to_string()),
        ])
    ]
);

// -------------------------
// |       Test Eval       |
// ------------------------

macro_rules! test_eval {
    (fail, $name:ident, $src:expr) => {
        #[test]
        fn $name() -> Result<(), String> {
            let ast = parse_expr().parse($src).map_err(|e| format!("{:?}", e))?;
            let mut env = Env::new();
            assert!(eval(&ast, &mut env).is_err());
            Ok(())
        }
    };
    (parser, $name:ident, $src:expr, $expected:expr) => {
        #[test]
        fn $name() -> Result<(), String> {
            let ast = parser().parse($src).unwrap();
            let mut env = Env::new();
            let mut result = Expr::Number(0.);
            for r in ast {
                eprintln!("{:?}", r);
                result = eval(&r, &mut env)?;
            }
            assert_eq!(result, $expected);
            Ok(())
        }
    };
    ($name:ident, $src:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let ast = parse_expr().parse($src).unwrap();
            let mut env = Env::new();
            let r = eval(&ast, &mut env);
            assert_eq!(r, Ok($expected));
        }
    };
}
test_eval!(eval_test_symbol_add, "(+ 1 2)", Expr::Number(3.));
test_eval!(eval_test_symbol_sub, "(- 1 2)", Expr::Number(-1.));
test_eval!(eval_test_symbol_eq_false, "(= 1 2)", Expr::Bool(false));
test_eval!(
    eval_test_symbol_eq_false_multi,
    "(= 1 2 3 4)",
    Expr::Bool(false)
);
test_eval!(eval_test_symbol_eq_true, "(= 2 2)", Expr::Bool(true));
test_eval!(
    eval_test_symbol_eq_true_multi,
    "(= 2 2 2 2)",
    Expr::Bool(true)
);
test_eval!(eval_test_symbol_gt_false, "(> 1 2)", Expr::Bool(false));
test_eval!(
    eval_test_symbol_gt_false_multi,
    "(> 1 2 3)",
    Expr::Bool(false)
);
test_eval!(eval_test_symbol_gt_true, "(> 3 2)", Expr::Bool(true));
test_eval!(
    eval_test_symbol_gt_true_multi,
    "(> 3 2 1)",
    Expr::Bool(true)
);
test_eval!(eval_test_symbol_lt_false, "(< 2 1)", Expr::Bool(false));
test_eval!(eval_test_symbol_lt_true, "(< 1 2)", Expr::Bool(true));
test_eval!(
    eval_test_symbol_lt_true_multi,
    "(< 1 2 3)",
    Expr::Bool(true)
);
test_eval!(
    eval_test_symbol_lt_false_multi,
    "(< 1 2 3)",
    Expr::Bool(true)
);
test_eval!(eval_test_symbol_lte_true, "(<= 1 1)", Expr::Bool(true));
test_eval!(eval_test_symbol_lte_false, "(<= 2 1)", Expr::Bool(false));
test_eval!(
    eval_test_symbol_lte_false_multi,
    "(<= 2 3 1)",
    Expr::Bool(false)
);
test_eval!(eval_test_symbol_gte_true, "(>= 1 1)", Expr::Bool(true));
test_eval!(eval_test_symbol_gte_false, "(>= 1 2)", Expr::Bool(false));
test_eval!(
    eval_test_symbol_and_false,
    "(and (= 1 1) (= 1 2))",
    Expr::Bool(false)
);
test_eval!(
    eval_test_symbol_and_true,
    "(and (= 1 1) (<= 1 2))",
    Expr::Bool(true)
);
test_eval!(
    parser,
    eval_test_fn_def_and_call,
    r#"
(fn add (x y) (+ x y))
(add 1 2)
    "#,
    Expr::Number(3.)
);
test_eval!(
    parser,
    eval_test_let,
    r#"
; returns -> 3
(let
    (x 1)
    (y 2)
    (+ x y))
    "#,
    Expr::Number(3.)
);
test_eval!(
    parser,
    eval_test_fn_def_var,
    r#"
(var a (+ 1 2))
    "#,
    Expr::Symbol("a".to_string())
);
test_eval!(
    parser,
    eval_test_fn_def_var_with_quote,
    r#"
(var a '(+ 1 2))
(fn add (x y) a)
(add 1 2)
    "#,
    Expr::List(vec![
        Expr::Symbol("+".to_string(),),
        Expr::Number(1.0,),
        Expr::Number(2.0,),
    ])
);

// ------------------------------------------
// |       Test For Builtin Functions       |
// ------------------------------------------
test_eval!(
    eval_test_builtin_plus,
    r#"
(+ 123 321) ; -> 444
    "#,
    Expr::Number(444.0)
);
test_eval!(
    eval_test_builtin_minus,
    r#"
(- 123 321) ; -> -123
    "#,
    Expr::Number(-198.0)
);
test_eval!(
    eval_test_builtin_mult,
    r#"
(* 2 2) ; -> 4
    "#,
    Expr::Number(4.0)
);
test_eval!(
    eval_test_builtin_div,
    r#"
(/ 100 5) ; -> 20
    "#,
    Expr::Number(20.0)
);
test_eval!(
    eval_test_builtin_mod,
    r#"
(/ 100 5) ; -> 20
    "#,
    Expr::Number(20.0)
);
test_eval!(
    eval_test_builtin_or,
    r#"
(or (= 1 1) (= 1 2))
    "#,
    Expr::Bool(true)
);
test_eval!(
    eval_test_builtin_and,
    r#"
(and (= 1 1) (= 1 2))
    "#,
    Expr::Bool(false)
);
test_eval!(
    eval_test_builtin_typeof_number,
    r#"
(typeof 1)
    "#,
    Expr::Symbol("Number".to_string())
);
test_eval!(
    eval_test_builtin_typeof_string,
    r#"
(typeof "number")
    "#,
    Expr::Symbol("String".to_string())
);
test_eval!(
    eval_test_builtin_typeof_list,
    r#"
(typeof (1 2 3))
    "#,
    Expr::Symbol("List".to_string())
);
test_eval!(
    eval_test_builtin_list,
    r#"
(list 1 2 3)
    "#,
    Expr::List(vec![
        Expr::Number(1.0),
        Expr::Number(2.0),
        Expr::Number(3.0),
    ])
);
test_eval!(
    eval_test_builtin_cons,
    r#"
(cons 1 (list 2 3))
    "#,
    Expr::List(vec![
        Expr::Number(1.0),
        Expr::Number(2.0),
        Expr::Number(3.0),
    ])
);
test_eval!(
    eval_test_builtin_car,
    r#"
(car (list 321 123))
    "#,
    Expr::Number(321.0)
);
test_eval!(
    eval_test_builtin_cdr,
    r#"
(cdr (list 321 123))
    "#,
    Expr::List(vec![Expr::Number(123.0)])
);
test_eval!(
    eval_test_builtin_append,
    r#"
(append (list 1 2) (list 3 4)) ; -> (1 2 3 4)
    "#,
    Expr::List(vec![
        Expr::Number(1.0),
        Expr::Number(2.0),
        Expr::Number(3.0),
        Expr::Number(4.0),
    ])
);
test_eval!(
    eval_test_builtin_reverse,
    r#"
(reverse (list 1 2 3)) ; -> (3 2 1)
    "#,
    Expr::List(vec![
        Expr::Number(3.0),
        Expr::Number(2.0),
        Expr::Number(1.0),
    ])
);
test_eval!(
    eval_test_builtin_nth,
    r#"
(nth (list 1 2 3) 2) ; -> 3
    "#,
    Expr::Number(3.0)
);
test_eval!(
    eval_test_builtin_length,
    r#"
(length (list 1 2 3)) ; -> 3
    "#,
    Expr::Number(3.0)
);
test_eval!(
    eval_test_builtin_map_with_lambda,
    r#"
(map (lambda (x) (+ x 1)) (list 1 2 3)) ; -> (2 3 4)
    "#,
    Expr::List(vec![
        Expr::Number(2.0),
        Expr::Number(3.0),
        Expr::Number(4.0),
    ])
);
test_eval!(
    parser,
    eval_test_builtin_map_with_function,
    r#"
(fn addone (x) (+ x 1))
(map addone (list 1 2 3)) ; -> (2 3 4)
    "#,
    Expr::List(vec![
        Expr::Number(2.0),
        Expr::Number(3.0),
        Expr::Number(4.0),
    ])
);
test_eval!(
    eval_test_builtin_fold_with_plus_op,
    r#"
(fold 0 + (list 1 2 3)) ; -> 6
    "#,
    Expr::Number(6.0)
);
test_eval!(
    parser,
    eval_test_builtin_fold_with_lambda,
    r#"
; returns -> 6
(fold 0
    (lambda (x y) (+ x y))
    (list 1 2 3))
    "#,
    Expr::Number(6.0)
);
test_eval!(
    parser,
    eval_test_builtin_fold_with_fn,
    r#"
(fn add (x y) (+ x y))
(fold 0 add (list 1 2 3)) ; -> 6
    "#,
    Expr::Number(6.0)
);
test_eval!(
    parser,
    eval_test_builtin_filter_with_lambda,
    r#"
; returns -> (2)
(filter
    (lambda (x) (= (mod x 2) 0))
    (list 1 2 3))
    "#,
    Expr::List(vec![Expr::Number(2.0)])
);
test_eval!(
    parser,
    eval_test_builtin_filter_with_fn,
    r#"

(fn is-even (x) (= (mod x 2) 0))

; returns -> (2)
(filter
    is-even
    (list 1 2 3))
    "#,
    Expr::List(vec![Expr::Number(2.0)])
);
test_eval!(
    fail,
    eval_test_builtin_assert,
    r#"
(assert (> 1 2) "1 is not greater than 2")
    "#
);
test_eval!(
    fail,
    eval_test_builtin_assert_eq,
    r#"
(assert 1 2 "1 is not 1 equal than 2")
    "#
);
// ----------------------------------
// |       Test For Debugging       |
// ----------------------------------

// #[test]
// fn test_debug() {
//     let src = include_str!("../test.nlisp");
//     let ast = parser().parse(src).unwrap();
//     let mut env = Env::new();
//     for node in ast {
//         eval(&node, &mut env).unwrap();
//     }
// }
