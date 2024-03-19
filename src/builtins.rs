use crate::environment::Env;
use crate::eval::eval;
use crate::parser::Expr;
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
        .map(|sections| HashMap::from_iter(sections.into_iter()));

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
        Ok(ast) => return ast,
        Err(err) => {
            println!("{err:?}");
            panic!("failed to parse docs");
        }
    }
}

#[test]
fn test_load_doc() {
    eprintln!("Starting test_load_doc");
    let doc = load_doc();
    let names = doc.keys().collect::<Vec<_>>();
    eprintln!("{names:#?}");
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

pub fn add(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let mut sum = 0.0;
    for arg in args {
        let i = eval(arg, env)?;
        sum += get_number(&i)?;
    }
    Ok(Expr::Number(sum))
}

pub fn sub(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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

pub fn mul(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let mut product = 1.0;
    for arg in args {
        let i = eval(arg, env)?;
        product *= get_number(&i)?;
    }
    Ok(Expr::Number(product))
}

pub fn div(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let h = args
        .first()
        .ok_or("div requires at least one argument".to_string())?;
    let eval_h = eval(h, env)?;
    let head = get_number(&eval_h)?;
    let mut sum = 1.0;
    for arg in &args[1..] {
        let i = eval(arg, env)?;
        sum *= get_number(&i)?;
    }
    Ok(Expr::Number(head / sum))
}

pub fn r#mod(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let h = args
        .first()
        .ok_or("mod requires at least one argument".to_string())?;
    let eval_h = eval(h, env)?;
    let head = get_number(&eval_h)?;
    let mut sum = 1.0;
    for arg in &args[1..] {
        let i = eval(arg, env)?;
        sum *= get_number(&i)?;
    }
    Ok(Expr::Number(head % sum))
}

pub fn help(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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

pub fn print(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("print requires at least one argument".to_string());
    }
    for arg in args {
        print!("{}", eval(arg, env)?);
    }
    Ok(args[0].clone())
}

pub fn type_of(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let expr = args
        .first()
        .ok_or("typeof requires at least one argument".to_string())?;
    let Expr::Symbol(_) = expr else {
        return Ok(Expr::Symbol(expr.type_of()));
    };
    let evaluated = eval(expr, env)?;
    Ok(Expr::Symbol(evaluated.type_of()))
}

pub fn list(args: &[Expr], _env: &mut Env) -> Result<Expr, String> {
    Ok(Expr::List(args.to_vec()))
}

pub fn cons(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("cons requires two arguments".to_string());
    }
    let Expr::List(tail) = eval(&args[1], env)? else {
        return Err("cons requires a list as the second argument".to_string());
    };

    let item = eval(&args[0], env)?;
    let new = vec![item].into_iter().chain(tail.iter().cloned());
    Ok(Expr::List(new.collect()))
}

pub fn car(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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

pub fn cdr(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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

pub fn append(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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

pub fn reverse(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("reverse requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("reverse requires a list to be the argument".to_string());
    };
    Ok(Expr::List(head.iter().rev().cloned().collect()))
}

pub fn nth(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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

pub fn length(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 1 {
        return Err("length requires one argument".to_string());
    }
    let Expr::List(head) = eval(&args[0], env)? else {
        return Err("length requires a list to be the first argument".to_string());
    };
    Ok(Expr::Number(head.len() as f64))
}

pub fn map(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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
    let mut inner_env = env.clone();
    for item in list {
        let Some(Expr::Symbol(param)) = params.first() else {
            return Err("map requires a lambda function with one argument".to_string());
        };

        inner_env.data.insert(param.to_string(), item.clone());
        let new_item = eval(&body, &mut inner_env)?;
        new_list.push(new_item);
    }
    Ok(Expr::List(new_list))
}

pub fn not(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let Expr::Bool(b) = eval(&args[0], env)? else {
        return Err("not requires a boolean".to_string());
    };
    Ok(Expr::Bool(!b))
}

pub fn fold(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    let acc_index = 0;
    let func_index = 1;
    let list_index = 2;

    if args.len() != 3 {
        return Err("fold requires three arguments".to_string());
    }
    let Expr::List(list) = eval(&args[list_index], env)? else {
        return Err("fold requires a list as second argument".to_string());
    };
    let (params, body) = match eval(&args[func_index], env)? {
        Expr::Lambda(lambda) => (*lambda.params, lambda.body),
        Expr::Func(func) => (*func.params, func.body),
        Expr::Builtin(func, _) => {
            let mut acc = eval(&args[acc_index], env)?;
            for item in list {
                acc = func(&[acc, item.clone()], env)?;
            }
            return Ok(acc);
        }
        _ => return Err("fold requires a function as first argument".to_string()),
    };
    let Expr::List(params) = params else {
        unreachable!("there is a bug in eval on lambda or func");
    };

    let Some(Expr::Symbol(acc)) = params.first() else {
        return Err(
            "fold requires a function with two argument, missing first argument".to_string(),
        );
    };
    let Some(Expr::Symbol(item)) = params.get(1) else {
        return Err(
            "fold requires a function with two argument, missing second argument".to_string(),
        );
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

pub fn filter(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("filter requires two arguments".to_string());
    }
    let Expr::List(list) = eval(&args[1], env)? else {
        return Err("filter requires a list as second argument".to_string());
    };
    let (params, body) = match eval(&args[0], env)? {
        Expr::Lambda(lambda) => (*lambda.params, lambda.body),
        Expr::Func(func) => (*func.params, func.body),
        Expr::Builtin(func, _) => {
            let mut new_list = vec![];
            for item in list {
                let new_item = func(&[item.clone()], env)?;
                if matches!(new_item, Expr::Bool(true)) {
                    new_list.push(item.clone());
                }
            }
            return Ok(Expr::List(new_list));
        }
        _ => return Err("filter requires a function as first argument".to_string()),
    };
    let Expr::List(params) = params else {
        unreachable!("there is a bug in eval on lambda or func");
    };
    let mut new_list = vec![];
    let mut inner_env = env.clone();
    for item in list {
        inner_env.data.insert(params[0].to_string(), item.clone());
        let new_item = eval(&body, &mut inner_env)?;
        if matches!(new_item, Expr::Bool(true)) {
            new_list.push(item.clone());
        }
    }
    Ok(Expr::List(new_list))
}

pub fn assertnl(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.is_empty() {
        return Err("assertnl requires one argument".to_string());
    }

    let msg = match &args.get(1) {
        Some(Expr::String(s)) => s,
        _ => "",
    };

    let Expr::Bool(b) = eval(&args[0], env)? else {
        return Err("assertnl requires a boolean".to_string());
    };

    if b {
        Ok(Expr::Bool(true))
    } else {
        eprintln!("{}", msg);
        Err("assertnl failed".to_string())
    }
}

pub fn assert_eqnl(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() < 2 {
        return Err("assert_eqnl requires at lest two arguments".to_string());
    }

    let msg = match &args.last() {
        Some(Expr::String(s)) => s,
        _ => "",
    };
    if msg.is_empty() && args.len() == 2 {
        return Err("assert_eqnl requires at lest two arguments".to_string());
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
    Ok(Expr::Bool(true))
}

pub fn r#loop(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
    if args.len() != 2 {
        return Err("loop requires two arguments".to_string());
    }
    let mut result = Expr::List(vec![]);
    let Some(condition) = args.first() else {
        return Err("loop requires two argument".to_string());
    };
    let Some(arg) = args.get(1) else {
        return Err("loop requires second argument for body".to_string());
    };
    while eval(condition, env)? != Expr::Bool(false) {
        result = eval(&arg, env)?;
    }
    Ok(result)
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
        pub fn $name(args: &[Expr], env: &mut Env) -> Result<Expr, String> {
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
