use crate::parser::{Expr, Func, Lambda};
use std::collections::HashMap;

pub fn eval(expr: &Expr, env: &mut Env) -> Result<Expr, String> {
    match expr {
        Expr::Bool(_) => Ok(expr.clone()),
        Expr::Number(_) => Ok(expr.clone()),
        Expr::String(_) => Ok(expr.clone()),
        Expr::Symbol(symbol) => eval_symbol(&symbol, env),
        Expr::List(list) => eval_list(list, env),
        _ => unreachable!("invalid expr: {expr:?}"),
    }
}

fn eval_symbol(symbol: &str, env: &mut Env) -> Result<Expr, String> {
    if let Some(outer) = &mut env.outer {
        outer
            .as_mut()
            .data
            .get(symbol)
            .cloned()
            .ok_or(format!("undefined symbol: {symbol}"))
    } else if env.data.contains_key(symbol) {
        env.data
            .get(symbol)
            .cloned()
            .ok_or(format!("undefined symbol: {symbol}"))
    } else {
        Err(format!("undefined symbol: {symbol}"))
    }
}

fn eval_list(expr: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let Some(head) = expr.first() else {
        return Err("empty list are not allowed".to_string());
    };
    let args = &expr[1..];
    match &head {
        // TODO: you was added the quotes
        Expr::Symbol(symbol) if symbol == "var" => eval_define_variable(args, env),
        Expr::Symbol(symbol) if symbol == "if" => eval_if(args, env),
        Expr::Symbol(symbol) if symbol == "lambda" => eval_define_lambda(args),
        Expr::Symbol(symbol) if symbol == "fn" => eval_define_fn(args, env),
        Expr::Symbol(symbol) if symbol == "quote" => eval_quote(args),
        _ => match eval(head, env)? {
            Expr::Builtin(builtin) => builtin(args, env),
            Expr::Lambda(func) => eval_lambda(&func, args, env),
            Expr::Func(func) => eval_func(&func, args, env),
            expr => Err(format!(
                "list head expected to be a function, but got {expr:?}"
            )),
        },
    }
}

fn eval_define_fn(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let Some(name_symbol) = args.first() else {
        return Err("missing name, params and body in fn definition".to_string());
    };

    let Some(params) = args.get(1) else {
        return Err("missing params and body in fn definition".to_string());
    };

    let Some(body) = args.get(2) else {
        return Err("missing body in fn definition".to_string());
    };
    let Expr::Symbol(name) = name_symbol.clone() else {
        return Err("fn name must be a symbol".to_string());
    };
    env.data.insert(
        name.clone(),
        Func {
            name: Box::new(name_symbol.clone()),
            params: Box::new(params.clone()),
            body: Box::new(body.clone()),
        }
        .into(),
    );
    Ok(name_symbol.clone())
}

fn eval_func(func: &Func, args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let mut new_env = env.clone();
    let Expr::List(params) = &*func.params else {
        unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list");
    };
    for (param, arg) in params.iter().zip(args) {
        let Expr::Symbol(param) = param else {
            unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list of symbol");
        };
        let i = eval(arg, env)?;
        new_env.data.insert(param.to_string(), i);
    }
    eval(&func.body, &mut new_env)
}

fn eval_define_variable(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let Some(name) = args.first() else {
        return Err("empty list are not allowed in var definition".to_string());
    };
    if args.len() != 2 {
        return Err(format!(
            "invalid number of arguments, expected 2, got {}",
            args.len()
        ));
    }
    let value = eval(&args[1], env)?;
    env.data.insert(name.to_string(), value);
    Ok(name.clone())
}

fn eval_define_lambda(args: &[Expr]) -> Result<Expr, String> {
    let Some(Expr::List(params)) = args.get(0) else {
        return Err("lambda params must be a list".to_string());
    };
    let Some(Expr::List(body)) = args.get(1) else {
        return Err("lambda body must be a list".to_string());
    };
    let lambda = Lambda {
        params: Box::new(Expr::List(params.clone())),
        body: Box::new(Expr::List(body.clone())),
    };
    Ok(Expr::Lambda(lambda))
}

fn eval_lambda(lambda: &Lambda, args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let mut new_env = env.clone();
    let Expr::List(params) = &*lambda.params else {
        unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list");
    };
    for (param, arg) in params.iter().zip(args) {
        let Expr::Symbol(param) = param else {
            unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list of symbol");
        };
        new_env.data.insert(param.to_string(), eval(arg, env)?);
    }
    eval(&lambda.body, &mut new_env)
}

fn eval_if(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 3 {
        return Err("invalid number of arguments for if expression expected 3".to_string());
    }
    let branch = if let Expr::Bool(true) = eval(&args[0], env)? {
        args[1].clone()
    } else {
        args[2].clone()
    };
    eval(&branch, env)
}

fn eval_quote(args: &[Expr]) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("invalid number of arguments for quote expression expected 1".to_string());
    }
    Ok(args[0].clone())
}

#[derive(Debug, Clone)]
pub struct Env {
    data: HashMap<String, Expr>,
    outer: Option<Box<Env>>,
}

impl Env {
    pub fn new() -> Self {
        let data = HashMap::from([
            ("+".to_string(), Expr::Builtin(add)),
            ("-".to_string(), Expr::Builtin(sub)),
            ("=".to_string(), Expr::Builtin(eq)),
            (">".to_string(), Expr::Builtin(gt)),
            ("<".to_string(), Expr::Builtin(lt)),
            (">=".to_string(), Expr::Builtin(gte)),
            ("<=".to_string(), Expr::Builtin(lte)),
            ("and".to_string(), Expr::Builtin(and)),
            ("or".to_string(), Expr::Builtin(or)),
            // ("not".to_string(), Expr::Builtin(not)),
            ("print".to_string(), Expr::Builtin(print)),
            ("typeof".to_string(), Expr::Builtin(type_of)),
        ]);
        Self { data, outer: None }
    }
}

fn get_number(expr: &Expr) -> Result<f64, String> {
    match expr {
        Expr::Number(n) => Ok(*n),
        e => Err(format!("{e:?} expected a number")),
    }
}

fn add(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let mut sum = 0.0;
    for arg in args {
        let i = eval(arg, env)?;
        sum += get_number(&i)?;
    }
    Ok(Expr::Number(sum))
}

fn sub(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let h = args
        .first()
        .ok_or("sub requires at least one argument".to_string())?;
    let eval_h = eval(h, env)?;
    let head = get_number(&eval_h)?;
    let mut sum = 0.0;
    for arg in &args[1..] {
        let i = eval(arg, env)?;
        sum += get_number(&i)?;
    }
    Ok(Expr::Number(head - sum))
}

fn print(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    for arg in args {
        print!("{}", eval(arg, env)?);
    }
    Ok(Expr::Bool(true))
}

fn type_of(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let expr = args
        .first()
        .ok_or("typeof requires at least one argument".to_string())?;
    let Expr::Symbol(_) = expr else {
        return Ok(Expr::Symbol(expr.type_of()));
    };
    let evaluated = eval(expr, env)?;
    Ok(Expr::Symbol(evaluated.type_of()))
}

#[macro_export]
macro_rules! unwrap {
    ($expr:expr, $name:ident) => {{
        match $expr {
            Expr::$name(n) => Ok(*n),
            e => Err(format!("{e:?} expected a {}", stringify!($name))),
        }
    }};
}

macro_rules! builtin {
    ($name:ident, $func:expr, $type:ident) => {
        fn $name(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
            let maybe_head = args
                .first()
                .ok_or("sub requires at least one argument".to_string())?;

            let head = crate::unwrap!(&eval(maybe_head, env)?, $type)?;

            for arg in &args[1..] {
                let value = crate::unwrap!(&eval(arg, env)?, $type)?;
                if $func(head, value) {
                    continue;
                }
                return Ok(Expr::Bool(false));
            }
            Ok(Expr::Bool(true))
        }
    };
}

builtin!(eq, |a: f64, b: f64| a == b, Number);
builtin!(gt, |a: f64, b: f64| a > b, Number);
builtin!(lt, |a: f64, b: f64| a < b, Number);
builtin!(gte, |a: f64, b: f64| a >= b, Number);
builtin!(lte, |a: f64, b: f64| a <= b, Number);
builtin!(and, |a: bool, b: bool| a && b, Bool);
builtin!(or, |a: bool, b: bool| a || b, Bool);
