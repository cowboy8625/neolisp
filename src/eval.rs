use crate::{
    builtins,
    parser::{Expr, Func, Lambda},
};
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
            Expr::Builtin(builtin, _) => builtin(args, env),
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

    let mut body_index = 2;
    let help_doc = match args.get(2) {
        Some(Expr::String(doc)) => {
            body_index += 1;
            Some(Box::new(Expr::String(doc.clone())))
        }
        _ => None,
    };

    let Some(body) = args.get(body_index) else {
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
            help_doc,
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

macro_rules! init_builtin {
    ($doc:ident, $symbol:expr, $name:ident) => {
        (
            $symbol.to_string(),
            crate::parser::Expr::Builtin($name, $doc.get($symbol).unwrap().to_string()),
        )
    };
    ($doc:ident, $name:ident) => {
        (
            stringify!($name).to_string(),
            Expr::Builtin($name, $doc.get(stringify!($name)).unwrap().to_string()),
        )
    };
}

impl Env {
    pub fn new() -> Self {
        let docs = builtins::load_doc();
        let data = HashMap::from([
            init_builtin!(docs, "+", add),
            init_builtin!(docs, "-", sub),
            init_builtin!(docs, "=", eq),
            init_builtin!(docs, ">", gt),
            init_builtin!(docs, "<", lt),
            init_builtin!(docs, ">=", gte),
            init_builtin!(docs, "<=", lte),
            init_builtin!(docs, "typeof", type_of),
            init_builtin!(docs, and),
            init_builtin!(docs, or),
            init_builtin!(docs, help),
            init_builtin!(docs, print),
            init_builtin!(docs, list),
            init_builtin!(docs, cons),
            init_builtin!(docs, car),
            init_builtin!(docs, cdr),
            init_builtin!(docs, append),
            init_builtin!(docs, reverse),
            init_builtin!(docs, nth),
            init_builtin!(docs, length),
            init_builtin!(docs, map),
            // ("not".to_string(), Expr::Builtin(not, docs.get("not").unwrap().clone())),
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

fn help(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("help takes one argument".to_string());
    }

    if let Expr::Symbol(name) = &args[0] {
        let Some(item) = env.data.get(name) else {
            return Ok(Expr::String(format!("{name}: has no help docs")));
        };
        match &item {
            Expr::Builtin(_, help_doc) => {
                return Ok(Expr::String(help_doc.to_string()));
            }
            Expr::Func(func) if func.help_doc.is_some() => {
                return Ok(*func.help_doc.clone().unwrap());
            }
            _ => {
                return Ok(Expr::String(format!("{item}: has no help docs")));
            }
        }
    }
    Ok(Expr::String(format!("{:?}: has no help docs", args[0])))
}

fn print(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("print requires at least one argument".to_string());
    }
    for arg in args {
        print!("{}", eval(arg, env)?);
    }
    Ok(args[0].clone())
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

fn list(args: &[Expr], _env: &mut Env) -> Result<Expr, String> {
    Ok(Expr::List(args.to_vec()))
}

fn cons(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("cons requires two arguments".to_string());
    }
    let Expr::List(tail) = eval(&args[1], env)? else {
        return Err("cons requires a list as the second argument".to_string());
    };

    let new = vec![args[0].clone()]
        .into_iter()
        .chain(tail.iter().cloned());
    Ok(Expr::List(new.collect()))
}

fn car(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("car requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("car requires a list to be the first and only argument".to_string());
    };
    let Some(head) = head.first() else {
        return Err("car requires a list with at least one element".to_string());
    };
    Ok(head.clone())
}

fn cdr(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("cdr requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("cdr requires a list to be the first and only argument".to_string());
    };
    let Some(tail) = head.get(1..) else {
        return Ok(Expr::List(vec![]));
    };
    Ok(Expr::List(tail.to_vec()))
}

fn append(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("append requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("append requires a list to be the first argument".to_string());
    };
    let mut result = head.clone();
    for arg in args[1..].iter() {
        let Expr::List(tail) = eval(arg, env)? else {
            return Err("append requires List to be the arguments type".to_string());
        };
        result.extend(tail);
    }
    Ok(Expr::List(result))
}

fn reverse(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("reverse requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("reverse requires a list to be the argument".to_string());
    };
    Ok(Expr::List(head.iter().rev().cloned().collect()))
}

fn nth(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("nth requires two arguments".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("nth requires a list to be the first argument".to_string());
    };
    let Expr::Number(index) = eval(&args[1], env)? else {
        return Err("nth requires a number to be the second argument".to_string());
    };
    let index = index as usize;
    if index >= head.len() {
        return Err("nth index out of bounds".to_string());
    }
    Ok(head[index].clone())
}

fn length(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("length requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("length requires a list to be the first argument".to_string());
    };
    Ok(Expr::Number(head.len() as f64))
}

fn map(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    println!("map args: {:?}", args);
    if args.len() != 2 {
        return Err("map requires two arguments".to_string());
    }
    let Expr::List(list) = eval(&args[1], env)? else {
        return Err("map requires a list as second argument".to_string());
    };
    let (params, body) = match eval(&args[0], env)? {
        Expr::Lambda(lambda) => (*lambda.params, lambda.body),
        Expr::Func(func) => (*func.params, func.body),
        Expr::Builtin(func, _) => {
            let mut new_list = vec![];
            for item in list {
                let new_item = func(&[item.clone()], env)?;
                new_list.push(new_item);
            }
            return Ok(Expr::List(new_list));
        }
        _ => return Err("map requires a function as first argument".to_string()),
    };

    let Expr::List(params) = params else {
        unreachable!("there is a bug in eval on lambda or func");
    };

    let mut new_list = vec![];
    for item in list {
        let mut inner_env = env.clone();
        let Some(Expr::Symbol(param)) = params.first() else {
            return Err("map requires a lambda function with one argument".to_string());
        };

        inner_env.data.insert(param.to_string(), item.clone());
        let new_item = eval(&body, &mut inner_env)?;
        new_list.push(new_item);
    }
    Ok(Expr::List(new_list))
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
