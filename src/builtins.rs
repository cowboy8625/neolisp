use crate::environment::Env;
use crate::eval::{eval, EvalResult};
use crate::parser::{Expr, Span, Spanned};
use chumsky::prelude::*;
use std::collections::HashMap;

fn doc_parser() -> impl Parser<char, HashMap<String, String>, Error = Simple<char>> {
    let find_start = take_until(just("## Functions"));

    let punctuation = one_of(r#"!$,_-./:;?+<=>#%&*@[\]{|}`^~"#);
    let letters = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
    let digit = one_of("0123456789");

    let symbol = choice((letters.clone(), punctuation.clone()))
        .then(choice((letters, punctuation, digit)).repeated())
        .map(|(start, end)| format!("{start}{}", end.iter().collect::<String>()));

    let name_parser = just("###")
        .padded()
        .ignore_then(symbol)
        .padded()
        .map(|name| name.trim().to_string());

    let body_parser = just('-')
        .then(take_until(just("```\n")))
        .padded()
        .map(|(dash, (start, end))| format!("{dash}{}\n{end}", start.iter().collect::<String>()));

    let section_parser = name_parser
        .then(body_parser)
        .padded()
        .map(|(name, body)| (name, body));

    let doc = find_start
        .ignore_then(section_parser.padded().repeated())
        .map(HashMap::from_iter);

    doc.padded()
        .then_ignore(take_until(end()))
        .then_ignore(end())
}

pub fn load_doc() -> HashMap<String, String> {
    let mut path = std::env::current_dir().unwrap();
    path.push("Docs.md");

    let src = std::fs::read_to_string(path).unwrap();
    let ast = doc_parser().parse(src);
    match ast {
        Ok(ast) => ast,
        Err(err) => {
            println!("{err:?}");
            panic!("failed to parse docs");
        }
    }
}

#[test]
fn test_load_doc() {
    let doc = load_doc();
    assert!(doc.contains_key("fn"));
    assert!(doc.contains_key("lambda"));
    assert!(doc.contains_key("var"));
    assert!(doc.contains_key("+"));
    assert!(doc.contains_key("-"));
    assert!(doc.contains_key("="));
    assert!(doc.contains_key(">"));
    assert!(doc.contains_key("<"));
    assert!(doc.contains_key(">="));
    assert!(doc.contains_key("<="));
    assert!(doc.contains_key("and"));
    assert!(doc.contains_key("or"));
    assert!(doc.contains_key("not"));
    assert!(doc.contains_key("print"));
    assert!(doc.contains_key("typeof"));
    assert!(doc.contains_key("help"));
    assert!(doc.contains_key("list"));
    assert!(doc.contains_key("cons"));
    assert!(doc.contains_key("car"));
    assert!(doc.contains_key("cdr"));
    assert!(doc.contains_key("append"));
    assert!(doc.contains_key("reverse"));
    assert!(doc.contains_key("nth"));
    assert!(doc.contains_key("length"));
    assert!(doc.contains_key("map"));
    assert!(doc.contains_key("fold"));
    assert!(doc.contains_key("fold-right"));
    assert!(doc.contains_key("filter"));
    assert!(doc.contains_key("filter"));
    assert!(doc.contains_key("assert"));
}

fn get_number(expr: &Expr) -> Result<f64, String> {
    match expr {
        Expr::Number(n) => Ok(*n),
        e => Err(format!("{e:?} expected a number")),
    }
}

pub fn add(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let mut sum = 0.0;
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    for arg in args {
        let i = eval(arg, env)?;
        sum += get_number(&i.expr)?;
    }
    Ok((Expr::Number(sum), span.start..end_span.end).into())
}

pub fn sub(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let h = args
        .first()
        .ok_or("sub requires at least one argument".to_string())?;
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    let eval_h = eval(h, env)?;
    let head = get_number(&eval_h.expr)?;
    let mut sum = 0.0;
    for arg in &args[1..] {
        let i = eval(arg, env)?;
        sum += get_number(&i.expr)?;
    }
    Ok((Expr::Number(head - sum), span.start..end_span.end).into())
}

pub fn mul(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let mut product = 1.0;
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    for arg in args {
        let i = eval(arg, env)?;
        product *= get_number(&i.expr)?;
    }
    Ok((Expr::Number(product), span.start..end_span.end).into())
}

pub fn div(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let h = args
        .first()
        .ok_or("div requires at least one argument".to_string())?;
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    let eval_h = eval(h, env)?;
    let head = get_number(&eval_h.expr)?;
    let mut sum = 1.0;
    for arg in &args[1..] {
        let i = eval(arg, env)?;
        sum *= get_number(&i.expr)?;
    }
    Ok((Expr::Number(head / sum), span.start..end_span.end).into())
}

pub fn r#mod(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let h = args
        .first()
        .ok_or("mod requires at least one argument".to_string())?;
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    let eval_h = eval(h, env)?;
    let head = get_number(&eval_h.expr)?;
    let mut sum = 1.0;
    for arg in &args[1..] {
        let i = eval(arg, env)?;
        sum *= get_number(&i.expr)?;
    }
    Ok((Expr::Number(head % sum), span.start..end_span.end).into())
}

pub fn help(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.is_empty() {
        return Err("help takes one argument".to_string());
    }
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());

    if let Expr::Symbol(name) = &args[0].expr {
        let Some(item) = env.data.get(name) else {
            return Ok((
                Expr::String(format!("{name}: has no help docs")),
                span.start..end_span.end,
            )
                .into());
        };
        match &item.expr {
            Expr::Builtin(_, help_doc) => {
                return Ok((Expr::String(help_doc.to_string()), span.start..end_span.end).into());
            }
            Expr::Func(func) if func.help_doc.is_some() => {
                return Ok(*func.help_doc.clone().unwrap());
            }
            _ => {
                return Ok((
                    Expr::String(format!("{item}: has no help docs")),
                    span.start..end_span.end,
                )
                    .into());
            }
        }
    }
    Ok((
        Expr::String(format!("{:?}: has no help docs", args[0])),
        span.start..end_span.end,
    )
        .into())
}

pub fn print(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.is_empty() {
        return Err(format!("{span:?}: print requires at least one argument"));
    }
    for arg in args {
        print!("{}", eval(arg, env)?);
    }
    Ok(args[0].clone())
}

pub fn type_of(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let expr = args
        .first()
        .ok_or("typeof requires at least one argument".to_string())?;
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    let Expr::Symbol(_) = expr.expr else {
        return Ok((Expr::Symbol(expr.expr.type_of()), expr.span.clone()).into());
    };
    let evaluated = eval(expr, env)?;
    Ok((
        Expr::Symbol(evaluated.expr.type_of()),
        span.start..end_span.end,
    )
        .into())
}

pub fn list(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    let mut evaluated_args = Vec::new();
    for arg in args {
        evaluated_args.push(eval(arg, env)?);
    }
    Ok((Expr::List(evaluated_args), span.start..end_span.end).into())
}

pub fn cons(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err("cons requires two arguments".to_string());
    }
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    let Expr::List(tail) = eval(&args[1], env)?.expr else {
        return Err("cons requires a list as the second argument".to_string());
    };

    let item = eval(&args[0], env)?;
    let new = vec![item].into_iter().chain(tail.iter().cloned());
    Ok((Expr::List(new.collect()), span.start..end_span.end).into())
}

pub fn car(_: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 1 {
        return Err("car requires one argument".to_string());
    }
    let item = eval(&args[0], env)?;
    match item.expr {
        Expr::List(list) => {
            let Some(head) = list.first() else {
                return Ok((Expr::Bool(false), item.span).into());
            };
            Ok(head.clone())
        }
        Expr::String(head) => {
            let Some(head) = head.chars().next() else {
                return Ok((Expr::Bool(false), item.span).into());
            };
            Ok((Expr::String(head.to_string()), item.span.clone()).into())
        }
        _ => {
            return Err(format!("{:?}: car requires a list or string to be the first and only argument but found {:?}", item.span, item.expr));
        }
    }
}

pub fn cdr(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 1 {
        return Err("cdr requires one argument".to_string());
    }
    let item = eval(&args[0], env)?;
    match item.expr {
        Expr::List(list) => {
            let Some(tail) = list.get(1..) else {
                return Ok((Expr::Bool(false), item.span).into());
            };
            let start_span = tail
                .first()
                .map(|a| a.span.clone())
                .unwrap_or(item.span.clone());
            let end_span = tail.last().map(|a| a.span.clone()).unwrap_or(start_span);
            let span = span.start..end_span.end;
            Ok((Expr::List(tail.to_vec()), span).into())
        }
        Expr::String(head) => {
            let Some(tail) = head.get(1..) else {
                return Ok((Expr::Bool(false), item.span).into());
            };
            let start = item.span.start + 1;
            let end = item.span.end;
            Ok((Expr::String(tail.to_string()), start..end).into())
        }
        _ => Err(format!(
            "{:?}: cdr requires a list or string to be the first and only argument but found {:?}",
            item.span, item.expr
        )),
    }
}

pub fn append(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.is_empty() {
        return Err("append requires one argument".to_string());
    }
    let end_span = args.last().map(|a| a.span.clone()).unwrap_or(span.clone());
    let item = eval(&args[0], env)?;
    let Expr::List(head) = item.expr else {
        return Err("append requires a list to be the first argument".to_string());
    };
    let mut result = head.clone();
    for arg in args[1..].iter() {
        let Expr::List(tail) = eval(arg, env)?.expr else {
            return Err("append requires List to be the arguments type".to_string());
        };
        result.extend(tail);
    }
    Ok((Expr::List(result), span.start..end_span.end).into())
}

pub fn reverse(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.is_empty() {
        return Err("reverse requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)?.expr else {
        return Err("reverse requires a list to be the argument".to_string());
    };
    Ok((Expr::List(head.iter().rev().cloned().collect()), span).into())
}

pub fn nth(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(format!("{span:?}: nth requires two arguments"));
    }
    let evaled_list = eval(&args[0], env)?;
    let Expr::List(head) = evaled_list.expr else {
        return Err(format!(
            "{:?}: nth requires a list to be the first argument but found {:?}",
            evaled_list.span, evaled_list.expr
        ));
    };
    let evaled = eval(&args[1], env)?;
    let Expr::Number(index) = evaled.expr else {
        return Err(format!(
            "{:?}: nth requires a number to be the second argument but found {:?}",
            evaled.span, evaled.expr
        ));
    };
    let index = index as usize;
    if index >= head.len() {
        return Err(format!(
            "{:?}: nth index out of bounds, list length is {} but index is {}",
            evaled.span,
            head.len(),
            evaled.expr
        ));
    }
    Ok(head[index].clone())
}

pub fn length(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 1 {
        return Err(format!("{span:?}: length requires one argument"));
    }
    let evaled = eval(&args[0], env)?;
    let Expr::List(head) = evaled.expr else {
        return Err(format!(
            "{:?}: length requires a list to be the first argument but found {:?}",
            evaled.span, evaled.expr
        ));
    };
    Ok((Expr::Number(head.len() as f64), span).into())
}

pub fn map(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(format!("{span:?}: map requires two arguments"));
    }
    let evaled_list = eval(&args[1], env)?;
    let Expr::List(list) = evaled_list.expr else {
        return Err(format!(
            "{:?}: map requires a list as second argument but found {:?}",
            evaled_list.span, evaled_list
        ));
    };
    let evaled_func = eval(&args[0], env)?;
    let (params, body) = match evaled_func.expr {
        Expr::Lambda(lambda) => (*lambda.params, lambda.body),
        Expr::Func(func) => (*func.params, func.body),
        Expr::Builtin(func, _) => {
            let mut new_list = vec![];
            for item in list {
                let new_item = func(evaled_func.span.clone(), &[item.clone()], env)?;
                new_list.push(new_item);
            }
            return Ok((Expr::List(new_list), span).into());
        }
        _ => {
            return Err(format!(
                "{:?}: map requires a function as first argument but found {:?}",
                evaled_func.span, evaled_func.expr
            ))
        }
    };

    let Expr::List(params) = params.expr else {
        unreachable!("there is a bug in eval on lambda or func");
    };

    let mut new_list = vec![];
    let mut inner_env = env.clone();
    for item in list {
        let Some(Spanned {
            expr: Expr::Symbol(param),
            ..
        }) = params.first()
        else {
            return Err("map requires a lambda function with one argument".to_string());
        };

        inner_env.data.insert(param.to_string(), item.clone());
        let new_item = eval(&body, &mut inner_env)?;
        new_list.push(new_item);
    }
    Ok((Expr::List(new_list), span).into())
}

pub fn not(_: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let evaled_arg = eval(&args[0], env)?;
    let Expr::Bool(b) = evaled_arg.expr else {
        return Err(format!(
            "{:?}: 'not' function requires a boolean as arguments but found {:?}",
            evaled_arg.span, evaled_arg.expr
        ));
    };
    Ok((Expr::Bool(!b), evaled_arg.span).into())
}

pub fn fold(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    let acc_index = 0;
    let func_index = 1;
    let list_index = 2;

    if args.len() != 3 {
        return Err(format!(
            "{span:?}: 'fold' function requires three arguments but found {}",
            args.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    let evaled_list = eval(&args[list_index], env)?;
    let Expr::List(list) = evaled_list.expr else {
        // Start Back here with format
        return Err(format!(
            "{:?}: 'fold' function requires a list as second argument but found {:?}",
            evaled_list.span, evaled_list.expr
        ));
    };
    let evaled_func = eval(&args[func_index], env)?;
    let (spanned_params, body) = match evaled_func.expr {
        Expr::Lambda(lambda) => (*lambda.params, lambda.body),
        Expr::Func(func) => (*func.params, func.body),
        Expr::Builtin(func, _) => {
            let mut acc = eval(&args[acc_index], env)?;
            for item in list {
                acc = func(evaled_func.span.clone(), &[acc, item.clone()], env)?;
            }
            return Ok(acc);
        }
        _ => {
            return Err(format!(
                "{:?}: 'fold' requires a function as first argument but found {:?}",
                evaled_func.span, evaled_func.expr
            ))
        }
    };
    let Expr::List(params) = spanned_params.expr else {
        unreachable!("there is a bug in eval on lambda or func");
    };

    let Some(Spanned {
        expr: Expr::Symbol(acc),
        ..
    }) = params.first()
    else {
        return Err(format!(
            "{:?}: fold requires a function with two argument, missing first argument",
            spanned_params.span
        ));
    };
    let Some(Spanned {
        expr: Expr::Symbol(item),
        ..
    }) = params.get(1)
    else {
        return Err(format!(
            "{:?}: 'fold' function requires two argument but found only one",
            spanned_params.span
        ));
    };

    let mut inner_env = env.clone();
    let mut acc_value = eval(&args[acc_index], env)?;
    for item_value in list {
        inner_env.data.insert(acc.to_string(), acc_value.clone());
        inner_env.data.insert(item.to_string(), item_value.clone());
        acc_value = eval(&body, &mut inner_env)?;
    }
    Ok(acc_value)
}

pub fn filter(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(format!(
            "{span:?}: 'filter' function requires three arguments but found {}",
            args.iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(",")
        ));
    }
    let evaled_list = eval(&args[1], env)?;
    let Expr::List(list) = evaled_list.expr else {
        return Err(format!(
            "{:?}: 'filter' function requires a list as second argument but found {:?}",
            evaled_list.span, evaled_list.expr
        ));
    };
    let evaled_func = eval(&args[0], env)?;
    let (spanned_params, body) = match evaled_func.expr {
        Expr::Lambda(lambda) => (*lambda.params, lambda.body),
        Expr::Func(func) => (*func.params, func.body),
        Expr::Builtin(func, _) => {
            let mut new_list = vec![];
            for item in list {
                let new_item = func(evaled_func.span.clone(), &[item.clone()], env)?;
                if matches!(new_item.expr, Expr::Bool(true)) {
                    new_list.push(item.clone());
                }
            }
            return Ok((Expr::List(new_list), evaled_func.span.clone()).into());
        }
        _ => return Err("filter requires a function as first argument".to_string()),
    };
    let Expr::List(params) = spanned_params.expr else {
        unreachable!("there is a bug in eval on lambda or func");
    };
    let mut new_list = vec![];
    let mut inner_env = env.clone();
    for item in list {
        inner_env.data.insert(params[0].to_string(), item.clone());
        let new_item = eval(&body, &mut inner_env)?;
        if matches!(new_item.expr, Expr::Bool(true)) {
            new_list.push(item.clone());
        }
    }
    let start_span = new_list
        .first()
        .map(|a| a.span.clone())
        .unwrap_or(span.clone());
    let end_span = new_list
        .last()
        .map(|a| a.span.clone())
        .unwrap_or(span.clone());
    let new_span = start_span.start..end_span.end;
    Ok((Expr::List(new_list), new_span).into())
}

pub fn assertnl(_: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.is_empty() {
        return Err("assertnl requires one argument".to_string());
    }

    let msg = match &args.get(1) {
        Some(Spanned {
            expr: Expr::String(s),
            ..
        }) => s,
        _ => "",
    };

    let condition = eval(&args[0], env)?;
    let Expr::Bool(b) = condition.expr else {
        return Err("assertnl requires a boolean".to_string());
    };

    if b {
        Ok((Expr::Bool(true), condition.span).into())
    } else {
        eprintln!("{}", msg);
        Err("assertnl failed".to_string())
    }
}

pub fn assert_eqnl(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() < 2 {
        return Err(format!(
            "{:?}: 'assert-eq' requires at lest two arguments but found {}",
            span,
            args.len()
        ));
    }

    let msg = match &args.last() {
        Some(Spanned {
            expr: Expr::String(s),
            ..
        }) => s,
        _ => "",
    };

    if !msg.is_empty() && args.len() == 2 {
        return Err(format!(
            "{:?}: 'assert-eq' requires at lest two arguments",
            span
        ));
    }
    let value = eval(&args[0], env)?;
    let end = if msg.is_empty() {
        args.len()
    } else {
        args.len() - 1
    };
    for arg in &args[1..end] {
        let eval_arg = eval(arg, env)?;
        if value != eval_arg {
            eprintln!("{}", msg);
            return Err(format!("{} is not equal to {}", value, eval_arg));
        }
    }
    Ok((Expr::Bool(true), span).into())
}

pub fn r#loop(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() < 2 {
        return Err(format!(
            "{:?}: 'loop' requires at lest two arguments but found {:?}",
            span,
            args.len()
        ));
    }
    let mut result = Spanned {
        expr: Expr::List(vec![]),
        span,
    };
    let Some(condition) = args.first() else {
        unreachable!("loop function");
    };
    let mut evaled_condition = eval(condition, env)?;
    while evaled_condition.expr != Expr::Bool(false) {
        evaled_condition = eval(condition, env)?;
        for arg in &args[1..] {
            result = eval(arg, env)?;
        }
    }
    Ok(result)
}

pub fn sleep(span: Span, args: &[Spanned<Expr>], _: &mut Env) -> EvalResult {
    if args.len() != 1 {
        return Err(format!("{:?}: 'sleep' requires one argument", span,));
    }
    let duration = match &args[0] {
        Spanned {
            expr: Expr::Number(n),
            ..
        } => *n as u64,
        _ => {
            return Err(format!(
                "{:?}: 'sleep' requires one number argument but found {:?}",
                span, args[0]
            ))
        }
    };
    std::thread::sleep(std::time::Duration::from_millis(duration));
    Ok((Expr::Bool(true), span).into())
}

pub fn to_string(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 1 {
        return Err(format!(
            "{:?}: 'to-string' requires one argument but found {:?}",
            span,
            args.len()
        ));
    }
    let value = eval(&args[0], env)?;
    match value.expr {
        Expr::Number(n) => Ok((Expr::String(n.to_string()), value.span).into()),
        Expr::Bool(b) => Ok((Expr::String(b.to_string()), value.span).into()),
        Expr::List(l) => Ok((
            Expr::String(l.iter().map(|v| v.expr.to_string()).collect::<String>()),
            value.span,
        )
            .into()),
        _ => Err("to-string only works on numbers, booleans and lists".to_string()),
    }
}

pub fn split_string(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 2 {
        return Err(format!(
            "{:?}: 'split' requires two argument but found {:?}",
            span,
            args.len()
        ));
    }

    let spanned_seperator = eval(&args[0], env)?;
    let Expr::String(seperator) = &spanned_seperator.expr else {
        return Err(format!(
            "{:?}: split only works on strings but found {:?}",
            span, spanned_seperator
        ));
    };

    let spanned_string = eval(&args[1], env)?;
    let Expr::String(string) = &spanned_string.expr else {
        return Err(format!(
            "{:?}: split only works on strings but found {:?}",
            span, spanned_seperator
        ));
    };
    Ok((
        Expr::List(
            string
                .split(seperator)
                .map(|s| (Expr::String(s.to_string()), span.clone()).into())
                .collect::<Vec<_>>(),
        ),
        span,
    )
        .into())
}

pub fn is_number(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
    if args.len() != 1 {
        return Err(format!(
            "{:?}: 'number?' requires one argument but found {:?}",
            span,
            args.len()
        ));
    }

    let spanned_maybe_number = eval(&args[0], env)?;
    let value = match spanned_maybe_number.expr {
        Expr::String(value) if value.parse::<f64>().is_ok() => true,
        Expr::Symbol(value) if value.parse::<f64>().is_ok() => true,
        Expr::Number(_) => true,
        _ => false,
    };
    Ok((Expr::Bool(value), spanned_maybe_number.span).into())
}

#[macro_export]
macro_rules! unwrap {
    ($expr:expr, $name:ident) => {{
        match $expr {
            Spanned {
                expr: Expr::$name(n),
                ..
            } => Ok(*n),
            e => Err(format!("{e:?} expected a {}", stringify!($name))),
        }
    }};
}

macro_rules! builtin {
    ($name:ident, $symbol:tt, $type:ident) => {
        pub fn $name(span: Span, args: &[Spanned<Expr>], env: &mut Env) -> EvalResult {
            let maybe_head = args
                .first()
                .ok_or("sub requires at least one argument".to_string())?;

            let head = crate::unwrap!(&eval(maybe_head, env)?, $type)?;

            for arg in &args[1..] {
                let value = crate::unwrap!(&eval(arg, env)?, $type)?;
                if head $symbol value {
                    continue;
                }
                return Ok((Expr::Bool(false), span).into());
            }
            Ok((Expr::Bool(true), span).into())
        }
    };
}

builtin!(eq, ==,  Number);
builtin!(gt, >,  Number);
builtin!(lt, <,  Number);
builtin!(gte, >=,  Number);
builtin!(lte, <=,  Number);
builtin!(and, &&,  Bool);
builtin!(or, ||,  Bool);
