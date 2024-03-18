use crate::environment::Env;
use crate::parser::{Expr, Func, Lambda};

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
