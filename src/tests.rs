use crate::ast::{Expr, Spanned};
use crate::parser::{parse_expr, parser};
use chumsky::prelude::*;
use pretty_assertions::assert_eq;

#[derive(Debug, Clone, PartialEq)]
pub enum TestingExpr {
    Bool(bool),
    String(String),
    Symbol(String),
    Number(f64),
    List(Vec<TestingExpr>),
}

impl std::fmt::Display for TestingExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::String(s) => write!(f, "{s}"),
            Self::Symbol(s) => write!(f, "{s}"),
            Self::Number(n) => write!(f, "{n}"),
            Self::List(list) => {
                write!(f, "(")?;
                for (i, item) in list.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub name: Box<TestingExpr>,
    pub params: Box<TestingExpr>,
    pub body: Box<TestingExpr>,
    pub help_doc: Option<Box<TestingExpr>>,
}

impl std::fmt::Display for Func {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(fn {} {} {})", self.name, self.params, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub params: Box<TestingExpr>,
    pub body: Box<TestingExpr>,
}

impl std::fmt::Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(lambda {} {})", self.params, self.body)
    }
}

macro_rules! test_parser {
    (parser, $name:ident, $src:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let ast = parser().parse($src).unwrap();
            let stripped = ast
                .iter()
                .cloned()
                .map(strip_out_spanned)
                .collect::<Vec<_>>();
            assert_eq!(stripped, $expected);
        }
    };
    ($name:ident, $src:expr, $expected:expr) => {
        #[test]
        fn $name() {
            let ast = parse_expr().parse($src).unwrap();
            let ast = strip_out_spanned(ast);
            assert_eq!(ast, $expected);
        }
    };
}

#[test]
fn test_span_simple() {
    let ast = parse_expr().parse("(+ 1 2)").unwrap();
    assert_eq!(
        ast,
        Spanned::from((
            Expr::List(vec![
                Spanned::from((Expr::Symbol("+".to_string()), 0..1)),
                Spanned::from((Expr::Number(1.0), 2..3)),
                Spanned::from((Expr::Number(2.0), 4..5)),
            ]),
            0..6
        ))
    );
}

fn strip_out_spanned(expr: Spanned<Expr>) -> TestingExpr {
    match expr.expr {
        Expr::Bool(bool) => TestingExpr::Bool(bool),
        Expr::String(string) => TestingExpr::String(string.to_string()),
        Expr::Symbol(symbol) => TestingExpr::Symbol(symbol.to_string()),
        Expr::Number(number) => TestingExpr::Number(number),
        Expr::List(exprs) => {
            TestingExpr::List(exprs.into_iter().map(strip_out_spanned).collect::<Vec<_>>())
        }
    }
}

test_parser!(
    parse_test_ident1,
    "
(hello-world)
",
    TestingExpr::List(vec![TestingExpr::Symbol("hello-world".to_string())])
);
test_parser!(
    parse_test_ident2,
    "
(*ident*)
",
    TestingExpr::List(vec![TestingExpr::Symbol("*ident*".to_string())])
);
test_parser!(
    parse_test_ident3,
    "
(<*ident/1234567890*>!?)
",
    TestingExpr::List(vec![TestingExpr::Symbol(
        "<*ident/1234567890*>!?".to_string()
    )])
);
test_parser!(
    parse_test_bool_comment,
    "
; comment
(true)
",
    TestingExpr::List(vec![TestingExpr::Bool(true)])
);
test_parser!(
    parse_test_bool_true,
    "(true)",
    TestingExpr::List(vec![TestingExpr::Bool(true)])
);
test_parser!(
    parse_test_bool_false,
    "(false)",
    TestingExpr::List(vec![TestingExpr::Bool(false)])
);
test_parser!(
    parse_test_number,
    "(1)",
    TestingExpr::List(vec![TestingExpr::Number(1.)])
);
test_parser!(
    parse_test_string,
    r#"("hello")"#,
    TestingExpr::List(vec![TestingExpr::String("hello".to_string())])
);
test_parser!(
    parse_test_string_empty,
    r#"("")"#,
    TestingExpr::List(vec![TestingExpr::String("".to_string())])
);
test_parser!(
    parse_test_string_ecape_char,
    r#"("\"")"#,
    TestingExpr::List(vec![TestingExpr::String("\"".to_string())])
);
test_parser!(
    parse_test_string_newline,
    r#"("hello\n")"#,
    TestingExpr::List(vec![TestingExpr::String("hello\n".to_string())])
);
test_parser!(
    parse_test_string_tab,
    r#"("hello\t")"#,
    TestingExpr::List(vec![TestingExpr::String("hello\t".to_string())])
);
test_parser!(
    parse_test_symbol_plus,
    "(+ 1 2)",
    TestingExpr::List(vec![
        TestingExpr::Symbol("+".to_string()),
        TestingExpr::Number(1.),
        TestingExpr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_minus,
    "(+ 1 2)",
    TestingExpr::List(vec![
        TestingExpr::Symbol("+".to_string()),
        TestingExpr::Number(1.),
        TestingExpr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_gt,
    "(> 1 2)",
    TestingExpr::List(vec![
        TestingExpr::Symbol(">".to_string()),
        TestingExpr::Number(1.),
        TestingExpr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_lt,
    "(< 1 2)",
    TestingExpr::List(vec![
        TestingExpr::Symbol("<".to_string()),
        TestingExpr::Number(1.),
        TestingExpr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_lte,
    "(<= 1 2)",
    TestingExpr::List(vec![
        TestingExpr::Symbol("<=".to_string()),
        TestingExpr::Number(1.),
        TestingExpr::Number(2.)
    ])
);
test_parser!(
    parse_test_symbol_gte,
    "(>= 1 2)",
    TestingExpr::List(vec![
        TestingExpr::Symbol(">=".to_string()),
        TestingExpr::Number(1.),
        TestingExpr::Number(2.)
    ])
);
test_parser!(
    parse_test_list,
    "((+ 1 2) (+ 1 2))",
    TestingExpr::List(vec![
        TestingExpr::List(vec![
            TestingExpr::Symbol("+".to_string()),
            TestingExpr::Number(1.),
            TestingExpr::Number(2.)
        ]),
        TestingExpr::List(vec![
            TestingExpr::Symbol("+".to_string()),
            TestingExpr::Number(1.),
            TestingExpr::Number(2.)
        ])
    ])
);
test_parser!(
    parse_test_lambda,
    "(lambda (x y) (+ x y))",
    TestingExpr::List(vec![
        TestingExpr::Symbol("lambda".to_string()),
        TestingExpr::List(vec![
            TestingExpr::Symbol("x".to_string()),
            TestingExpr::Symbol("y".to_string())
        ]),
        TestingExpr::List(vec![
            TestingExpr::Symbol("+".to_string()),
            TestingExpr::Symbol("x".to_string()),
            TestingExpr::Symbol("y".to_string())
        ]),
    ])
);
test_parser!(
    parse_test_def_var,
    "(var x 1)",
    TestingExpr::List(vec![
        TestingExpr::Symbol("var".to_string()),
        TestingExpr::Symbol("x".to_string()),
        TestingExpr::Number(1.)
    ])
);
test_parser!(
    parse_test_def_func,
    "(fn add (x y) (+ x y))",
    TestingExpr::List(vec![
        TestingExpr::Symbol("fn".to_string()),
        TestingExpr::Symbol("add".to_string()),
        TestingExpr::List(vec![
            TestingExpr::Symbol("x".to_string()),
            TestingExpr::Symbol("y".to_string())
        ]),
        TestingExpr::List(vec![
            TestingExpr::Symbol("+".to_string()),
            TestingExpr::Symbol("x".to_string()),
            TestingExpr::Symbol("y".to_string())
        ])
    ])
);
test_parser!(
    parse_test_quote,
    "(var x '(1 2 3))",
    TestingExpr::List(vec![
        TestingExpr::Symbol("var".to_string()),
        TestingExpr::Symbol("x".to_string()),
        TestingExpr::List(vec![
            TestingExpr::Symbol("quote".to_string()),
            TestingExpr::List(vec![
                TestingExpr::Number(1.),
                TestingExpr::Number(2.),
                TestingExpr::Number(3.)
            ])
        ]),
    ])
);
test_parser!(
    parser,
    parse_test_program_with_many_single_list,
    " (x y) (+ x y) (x y z) ",
    vec![
        TestingExpr::List(vec![
            TestingExpr::Symbol("x".to_string()),
            TestingExpr::Symbol("y".to_string()),
        ]),
        TestingExpr::List(vec![
            TestingExpr::Symbol("+".to_string()),
            TestingExpr::Symbol("x".to_string()),
            TestingExpr::Symbol("y".to_string()),
        ]),
        TestingExpr::List(vec![
            TestingExpr::Symbol("x".to_string()),
            TestingExpr::Symbol("y".to_string()),
            TestingExpr::Symbol("z".to_string()),
        ])
    ]
);
//
// // -------------------------
// // |       Test Eval       |
// // ------------------------
//
// macro_rules! test_eval {
//     (fail, $name:ident, $src:expr) => {
//         #[test]
//         fn $name() -> Result<(), String> {
//             let ast = parse_expr().parse($src).map_err(|e| format!("{:?}", e))?;
//             let mut env = Env::new();
//             assert!(eval(&ast, &mut env).is_err());
//             Ok(())
//         }
//     };
//     (parser, $name:ident, $src:expr, $expected:expr) => {
//         #[test]
//         fn $name() -> Result<(), String> {
//             let ast = parser().parse($src).unwrap();
//             let mut env = Env::new();
//             let mut result = TestingExpr::Number(0.);
//             for r in ast {
//                 let ast = eval(&r, &mut env)?;
//                 result = strip_out_spanned(ast);
//             }
//             assert_eq!(result, $expected);
//             Ok(())
//         }
//     };
//     ($name:ident, $src:expr, $expected:expr) => {
//         #[test]
//         fn $name() {
//             let ast = parse_expr().parse($src).unwrap();
//             let mut env = Env::new();
//             let r = match eval(&ast, &mut env) {
//                 Ok(r) => r,
//                 Err(e) => panic!("{:?}", e),
//             };
//             let result = strip_out_spanned(r);
//             assert_eq!(result, $expected);
//         }
//     };
// }
// test_eval!(eval_test_symbol_add, "(+ 1 2)", TestingExpr::Number(3.));
// test_eval!(eval_test_symbol_sub, "(- 1 2)", TestingExpr::Number(-1.));
// test_eval!(
//     eval_test_symbol_eq_false,
//     "(= 1 2)",
//     TestingExpr::Bool(false)
// );
// test_eval!(
//     eval_test_symbol_eq_false_multi,
//     "(= 1 2 3 4)",
//     TestingExpr::Bool(false)
// );
// test_eval!(eval_test_symbol_eq_true, "(= 2 2)", TestingExpr::Bool(true));
// test_eval!(
//     eval_test_symbol_eq_true_multi,
//     "(= 2 2 2 2)",
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     eval_test_symbol_gt_false,
//     "(> 1 2)",
//     TestingExpr::Bool(false)
// );
// test_eval!(
//     eval_test_symbol_gt_false_multi,
//     "(> 1 2 3)",
//     TestingExpr::Bool(false)
// );
// test_eval!(eval_test_symbol_gt_true, "(> 3 2)", TestingExpr::Bool(true));
// test_eval!(
//     eval_test_symbol_gt_true_multi,
//     "(> 3 2 1)",
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     eval_test_symbol_lt_false,
//     "(< 2 1)",
//     TestingExpr::Bool(false)
// );
// test_eval!(eval_test_symbol_lt_true, "(< 1 2)", TestingExpr::Bool(true));
// test_eval!(
//     eval_test_symbol_lt_true_multi,
//     "(< 1 2 3)",
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     eval_test_symbol_lt_false_multi,
//     "(< 1 2 3)",
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     eval_test_symbol_lte_true,
//     "(<= 1 1)",
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     eval_test_symbol_lte_false,
//     "(<= 2 1)",
//     TestingExpr::Bool(false)
// );
// test_eval!(
//     eval_test_symbol_lte_false_multi,
//     "(<= 2 3 1)",
//     TestingExpr::Bool(false)
// );
// test_eval!(
//     eval_test_symbol_gte_true,
//     "(>= 1 1)",
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     eval_test_symbol_gte_false,
//     "(>= 1 2)",
//     TestingExpr::Bool(false)
// );
// test_eval!(
//     eval_test_symbol_and_false,
//     "(and (= 1 1) (= 1 2))",
//     TestingExpr::Bool(false)
// );
// test_eval!(
//     eval_test_symbol_and_true,
//     "(and (= 1 1) (<= 1 2))",
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     parser,
//     eval_test_fn_def_and_call,
//     r#"
// (fn add (x y) (+ x y))
// (add 1 2)
//     "#,
//     TestingExpr::Number(3.)
// );
// test_eval!(
//     parser,
//     eval_test_let,
//     r#"
// ; returns -> 3
// (let
//     ((x 1)
//     (y 2))
//     (+ x y))
//     "#,
//     TestingExpr::Number(3.)
// );
// test_eval!(
//     parser,
//     eval_test_fn_def_var,
//     r#"
// (var a (+ 1 2))
//     "#,
//     TestingExpr::Symbol("a".to_string())
// );
// test_eval!(
//     parser,
//     eval_test_fn_def_var_with_quote,
//     r#"
// (var a '(+ 1 2))
// (fn add (x y) a)
// (add 1 2)
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Symbol("+".to_string(),),
//         TestingExpr::Number(1.0,),
//         TestingExpr::Number(2.0,),
//     ])
// );
//
// // ------------------------------------------
// // |       Test For Builtin Functions       |
// // ------------------------------------------
// test_eval!(
//     eval_test_builtin_plus,
//     r#"
// (+ 123 321) ; -> 444
//     "#,
//     TestingExpr::Number(444.0)
// );
// test_eval!(
//     eval_test_builtin_minus,
//     r#"
// (- 123 321) ; -> -123
//     "#,
//     TestingExpr::Number(-198.0)
// );
// test_eval!(
//     eval_test_builtin_mult,
//     r#"
// (* 2 2) ; -> 4
//     "#,
//     TestingExpr::Number(4.0)
// );
// test_eval!(
//     eval_test_builtin_div,
//     r#"
// (/ 100 5) ; -> 20
//     "#,
//     TestingExpr::Number(20.0)
// );
// test_eval!(
//     eval_test_builtin_mod,
//     r#"
// (/ 100 5) ; -> 20
//     "#,
//     TestingExpr::Number(20.0)
// );
// test_eval!(
//     eval_test_builtin_or,
//     r#"
// (or (= 1 1) (= 1 2))
//     "#,
//     TestingExpr::Bool(true)
// );
// test_eval!(
//     eval_test_builtin_and,
//     r#"
// (and (= 1 1) (= 1 2))
//     "#,
//     TestingExpr::Bool(false)
// );
// test_eval!(
//     eval_test_builtin_typeof_number,
//     r#"
// (typeof 1)
//     "#,
//     TestingExpr::Symbol("Number".to_string())
// );
// test_eval!(
//     eval_test_builtin_typeof_string,
//     r#"
// (typeof "number")
//     "#,
//     TestingExpr::Symbol("String".to_string())
// );
// test_eval!(
//     eval_test_builtin_typeof_list,
//     r#"
// (typeof (1 2 3))
//     "#,
//     TestingExpr::Symbol("List".to_string())
// );
// test_eval!(
//     eval_test_builtin_list,
//     r#"
// (list 1 2 3)
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Number(1.0),
//         TestingExpr::Number(2.0),
//         TestingExpr::Number(3.0),
//     ])
// );
// test_eval!(
//     eval_test_builtin_cons,
//     r#"
// (cons (+ 0 1) (list 2 3))
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Number(1.0),
//         TestingExpr::Number(2.0),
//         TestingExpr::Number(3.0),
//     ])
// );
// test_eval!(
//     eval_test_builtin_car_list,
//     r#"
// (car (list 321 123))
//     "#,
//     TestingExpr::Number(321.0)
// );
// test_eval!(
//     eval_test_builtin_car_string,
//     r#"
// (car "Hello World") ; -> "H"
//     "#,
//     TestingExpr::String("H".to_string())
// );
// test_eval!(
//     eval_test_builtin_cdr_list,
//     r#"
// (cdr (list 321 123))
//     "#,
//     TestingExpr::List(vec![TestingExpr::Number(123.0)])
// );
// test_eval!(
//     eval_test_builtin_cdr_string,
//     r#"
// (cdr "Hello World") ; -> "ello World"
//     "#,
//     TestingExpr::String("ello World".to_string())
// );
// test_eval!(
//     eval_test_builtin_last_list,
//     r#"
// (last (list 321 123))
//     "#,
//     TestingExpr::Number(123.0)
// );
// test_eval!(
//     eval_test_builtin_last_string,
//     r#"
// (last "Hello")
//     "#,
//     TestingExpr::String("o".to_string())
// );
// test_eval!(
//     eval_test_builtin_append,
//     r#"
// (append (list 1 2) (list 3 4)) ; -> (1 2 3 4)
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Number(1.0),
//         TestingExpr::Number(2.0),
//         TestingExpr::Number(3.0),
//         TestingExpr::Number(4.0),
//     ])
// );
// test_eval!(
//     eval_test_builtin_reverse,
//     r#"
// (reverse (list 1 2 3)) ; -> (3 2 1)
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Number(3.0),
//         TestingExpr::Number(2.0),
//         TestingExpr::Number(1.0),
//     ])
// );
// test_eval!(
//     eval_test_builtin_nth,
//     r#"
// (nth (list 1 2 3) 2) ; -> 3
//     "#,
//     TestingExpr::Number(3.0)
// );
// test_eval!(
//     eval_test_builtin_length,
//     r#"
// (length (list 1 2 3)) ; -> 3
//     "#,
//     TestingExpr::Number(3.0)
// );
// test_eval!(
//     eval_test_builtin_map_with_lambda,
//     r#"
// (map (lambda (x) (+ x 1)) (list 1 2 3)) ; -> (2 3 4)
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Number(2.0),
//         TestingExpr::Number(3.0),
//         TestingExpr::Number(4.0),
//     ])
// );
// test_eval!(
//     parser,
//     eval_test_builtin_map_with_function,
//     r#"
// (fn addone (x) (+ x 1))
// (map addone (list 1 2 3)) ; -> (2 3 4)
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Number(2.0),
//         TestingExpr::Number(3.0),
//         TestingExpr::Number(4.0),
//     ])
// );
// test_eval!(
//     eval_test_builtin_fold_with_plus_op,
//     r#"
// (fold 0 + (list 1 2 3)) ; -> 6
//     "#,
//     TestingExpr::Number(6.0)
// );
// test_eval!(
//     parser,
//     eval_test_builtin_fold_with_lambda,
//     r#"
// ; returns -> 6
// (fold 0
//     (lambda (x y) (+ x y))
//     (list 1 2 3))
//     "#,
//     TestingExpr::Number(6.0)
// );
// test_eval!(
//     parser,
//     eval_test_builtin_fold_with_fn,
//     r#"
// (fn add (x y) (+ x y))
// (fold 0 add (list 1 2 3)) ; -> 6
//     "#,
//     TestingExpr::Number(6.0)
// );
// test_eval!(
//     parser,
//     eval_test_builtin_filter_with_lambda,
//     r#"
// ; returns -> (2)
// (filter
//     (lambda (x) (= (mod x 2) 0))
//     (list 1 2 3))
//     "#,
//     TestingExpr::List(vec![TestingExpr::Number(2.0)])
// );
// test_eval!(
//     parser,
//     eval_test_builtin_filter_with_fn,
//     r#"
//
// (fn is-even (x) (= (mod x 2) 0))
//
// ; returns -> (2)
// (filter
//     is-even
//     (list 1 2 3))
//     "#,
//     TestingExpr::List(vec![TestingExpr::Number(2.0)])
// );
// test_eval!(
//     eval_test_builtin_loop,
//     r#"(let ((x 0)) (loop (< x 3) (var x (+ x 1))))
// ; returns -> (2)
//     "#,
//     TestingExpr::Symbol("x".to_string())
// );
// test_eval!(
//     eval_test_builtin_to_string,
//     r#"
// (to-string 1) ; returns -> "1"
//     "#,
//     TestingExpr::String("1".to_string())
// );
//
// test_eval!(
//     eval_test_builtin_split_string,
//     r#"
// (split " " "(+ 1 1)") ; ->  ("(+" "1" "1)")
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::String("(+".to_string()),
//         TestingExpr::String("1".to_string()),
//         TestingExpr::String("1)".to_string()),
//     ])
// );
//
// test_eval!(
//     eval_test_builtin_is_number,
//     r#"
//
// (list
//     (number? 10) ; -> true
//     (number? "10") ; -> true
//     (number? "abc") ; -> false
//     (number? '(1 2 3)) ; -> false
// )
//     "#,
//     TestingExpr::List(vec![
//         TestingExpr::Bool(true),
//         TestingExpr::Bool(true),
//         TestingExpr::Bool(false),
//         TestingExpr::Bool(false),
//     ])
// );
//
// test_eval!(
//     eval_test_builtin_do_block,
//     r#"
// (do (print "hello")(+ 321 123)) ; -> 444
//     "#,
//     TestingExpr::Number(444.0)
// );
//
// test_eval!(
//     eval_test_builtin_slice_string,
//     r#"
// (slice "abc" 1 2) ; -> "b"
//     "#,
//     TestingExpr::String("b".to_string())
// );
//
// test_eval!(
//     eval_test_builtin_slice_list,
//     r#"
// (slice (list 1 2 3) 1 2) ; -> (2)
//     "#,
//     TestingExpr::List(vec![TestingExpr::Number(2.0)])
// );
// // ----------------------------------
// // |       Test ENVIRONMENTS        |
// // ----------------------------------
// test_eval!(
//     parser,
//     lambda_environment,
//     r#"
// (fn create-function (outer)
//     (lambda (x) (+ x outer)))
// ((create-function 10) 1) ; -> 11
//     "#,
//     TestingExpr::Number(11.0)
// );
// // ----------------------------------
// // |       Test For Debugging       |
// // ----------------------------------
//
// test_eval!(
//     fail,
//     eval_test_builtin_assert,
//     r#"
// (assert (> 1 2) "1 is not greater than 2")
//     "#
// );
// test_eval!(
//     fail,
//     eval_test_builtin_assert_eq,
//     r#"
// (assert 1 2 "1 is not 1 equal than 2")
//     "#
// );
