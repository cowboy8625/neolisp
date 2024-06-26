use crate::ast::{Expr, Func, Lambda, Span, Spanned};
use crate::environment::Env;

pub type EvalResult = Result<Spanned<Expr>, String>;

pub fn eval(expr: &Spanned<Expr>, env: &mut Env) -> EvalResult {
    match &expr.expr {
        Expr::Bool(_) => Ok(expr.clone()),
        Expr::Number(_) => Ok(expr.clone()),
        Expr::String(_) => Ok(expr.clone()),
        Expr::Symbol(symbol) => eval_symbol(expr.span.clone(), symbol, env),
        Expr::List(list) => eval_list(expr.span.clone(), list, env),
        _ => unreachable!("invalid expr: {expr:?}"),
    }
}

fn eval_symbol(span: Span, symbol: &str, env: &mut Env) -> EvalResult {
    match env.data.get(symbol) {
        Some(expr) => Ok(expr.clone()),
        None => match &mut env.outer {
            Some(outer_env) => eval_symbol(span, symbol, outer_env),
            None => Err(format!("{span:?}: undefined symbol: {symbol}")),
        },
    }
}

fn eval_list(span: Span, expr: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let Some(head) = expr.first() else {
        return Err("empty list are not allowed".to_string());
    };
    let args = &expr[1..];
    match &head.expr {
        Expr::Symbol(symbol) if symbol == "var" => eval_define_variable(args, env),
        Expr::Symbol(symbol) if symbol == "let" => eval_define_let(span.clone(), args, env),
        Expr::Symbol(symbol) if symbol == "if" => eval_if(args, env),
        Expr::Symbol(symbol) if symbol == "lambda" => {
            eval_define_lambda(head.span.clone(), args, env)
        }
        Expr::Symbol(symbol) if symbol == "fn" => eval_define_fn(head.span.clone(), args, env),
        Expr::Symbol(symbol) if symbol == "quote" => eval_quote(args),
        _ => {
            let evaled_head = eval(head, env)?;
            match evaled_head.expr {
                Expr::Builtin(builtin, _) => builtin(evaled_head.span, args, env),
                Expr::Lambda(func) => eval_lambda(evaled_head.span, &func, args, env),
                Expr::Func(func) => eval_func(&func, args, env),
                expr => Err(format!(
                    "{:?}: list head expected to be a function, but got {expr:?}",
                    evaled_head.span
                )),
            }
        }
    }
}

fn eval_define_fn(starting_span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let Some(name_symbol) = args.first() else {
        return Err("missing name, params and body in fn definition".to_string());
    };

    let Some(params) = args.get(1) else {
        return Err("missing params and body in fn definition".to_string());
    };

    let mut body_index = 2;
    let help_doc = match args.get(2) {
        Some(maybe_doc) if matches!(maybe_doc.expr, Expr::String(_)) => {
            body_index += 1;
            Some(Box::new(maybe_doc.clone()))
        }
        _ => None,
    };

    let Some(body) = args.get(body_index) else {
        return Err("missing body in fn definition".to_string());
    };
    let Expr::Symbol(name) = name_symbol.expr.clone() else {
        return Err("fn name must be a symbol".to_string());
    };
    env.data.insert(
        name.clone(),
        (
            Expr::Func(Func {
                name: Box::new(name_symbol.clone()),
                params: Box::new(params.clone()),
                body: Box::new(body.clone()),
                help_doc,
            }),
            starting_span,
        )
            .into(),
    );
    Ok(name_symbol.clone())
}

fn eval_func(func: &Func, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let mut new_env = env.clone();
    let Spanned {
        expr: Expr::List(params),
        ..
    } = &*func.params
    else {
        unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list");
    };

    for (param, arg) in params.iter().zip(args) {
        let Expr::Symbol(param) = &param.expr else {
            unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list of symbol");
        };
        let i = eval(arg, env)?;
        new_env.data.insert(param.to_string(), i);
    }
    env.outer = Some(Box::new(new_env));
    eval(&func.body, env)
}

fn eval_define_variable(args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(format!(
            "invalid number of arguments, expected 2, got {}",
            args.len()
        ));
    }
    let Some(name) = args.first() else {
        return Err("empty list are not allowed in var definition".to_string());
    };
    let value = eval(&args[1], env)?;
    env.data.insert(name.to_string(), value);
    Ok(name.clone())
}

fn eval_define_let(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(format!(
            "{:?}: let requires two arguments but found {}",
            span,
            args.len()
        ));
    }

    let Some(spanned_first) = args.first() else {
        unreachable!("parser bug,  or eval bug for let blocks first");
    };

    let Some(body) = args.last() else {
        unreachable!("parser bug,  or eval bug for let blocks last");
    };

    let Expr::List(first) = &spanned_first.expr else {
        return Err(format!(
            "{:?}: let arguments must be a list but found {:?}",
            span, spanned_first.expr
        ));
    };

    let mut new_env = env.clone();

    for arg in first {
        let Spanned {
            expr: Expr::List(list),
            ..
        } = arg
        else {
            return Err("let arguments must be a list".to_string());
        };
        eval_define_variable(list, &mut new_env)?;
    }
    eval(body, &mut new_env)
}

fn eval_define_lambda(lambda_span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let Some(Spanned {
        expr: Expr::List(params),
        span: params_span,
    }) = args.first()
    else {
        return Err("lambda params must be a list".to_string());
    };

    let Some(body) = args.get(1) else {
        return Err("lambda body missing".to_string());
    };

    let lambda = Lambda {
        params: Box::new((Expr::List(params.clone()), params_span).into()),
        body: Box::new(body.clone()),
    };
    Ok((Expr::Lambda(lambda), lambda_span).into())
}

fn eval_lambda(_: Span, lambda: &Lambda, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let mut new_env = env.clone();
    let Expr::List(params) = &lambda.params.expr else {
        unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list");
    };
    for (param, arg) in params.iter().zip(args) {
        let Expr::Symbol(param) = &param.expr else {
            unreachable!("if this message is displayed, there is a bug in the in the eval function for params buts be a list of symbol");
        };
        let e = eval(arg, env)?;
        new_env.data.insert(param.to_string(), e);
    }
    eval(&lambda.body, &mut new_env)
}

fn eval_if(args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 3 {
        return Err(format!(
            "invalid number of arguments for if expression expected 3 found {}: {args:#?}",
            args.len()
        ));
    }
    let Spanned {
        expr: conditional,
        span: conditional_span,
    } = eval(&args[0], env)?;

    let condition = match conditional {
        Expr::Bool(b) => b,
        _ => {
            return Err(format!(
                "if condition must evaluate to a boolean value {conditional_span:?}"
            ));
        }
    };

    let branch = if condition {
        args[1].clone()
    } else {
        args[2].clone()
    };
    eval(&branch, env)
}

fn eval_quote(args: &[Spanned<Expr>]) -> EvalResult {
    if args.len() != 1 {
        return Err("invalid number of arguments for quote expression expected 1".to_string());
    }
    Ok(args[0].clone())
}
