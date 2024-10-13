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
