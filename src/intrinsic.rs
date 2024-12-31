use crate::error::Error;
use crate::instruction::Value;
use crate::machine::Machine;
use anyhow::Result;
use crossterm::style::Stylize;
use std::collections::HashMap;

pub(crate) struct Intrinsic;
impl Intrinsic {
    pub(crate) fn intrinsic_sleep(machine: &mut Machine, _: u8) -> Result<()> {
        // (sleep 1000) ; -> false
        let frame = machine.get_current_frame_mut()?;
        let Some(Value::F64(value)) = frame.stack.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected number on stack for sleep".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        std::thread::sleep(std::time::Duration::from_millis(value as u64));
        Ok(())
    }

    pub(crate) fn intrinsic_is_atom(machine: &mut Machine, count: u8) -> Result<()> {
        // (atom? 10) ; -> true
        // (atom? "10") ; -> true
        // (atom? "abc") ; -> true
        // (atom? '(1 2 3)) ; -> false
        if count != 1 {
            anyhow::bail!("atom? only support 1 arg");
        }
        let frame = machine.get_current_frame_mut()?;
        let Some(item) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected atom on stack for atom?".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let result = !matches!(item, Value::List(_) | Value::Callable(_));
        frame.stack.push(Value::Bool(result));
        Ok(())
    }

    pub(crate) fn intrinsic_is_number(machine: &mut Machine, count: u8) -> Result<()> {
        // (number? 10) ; -> true
        // (number? "10") ; -> true
        // (number? "abc") ; -> false
        // (number? '(1 2 3)) ; -> false
        if count != 1 {
            anyhow::bail!("number? only support 1 arg");
        }
        let frame = machine.get_current_frame_mut()?;
        let Some(item) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected number on stack for number?".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let result = matches!(
            item,
            Value::F64(_) | Value::U32(_) | Value::I32(_) | Value::F32(_) | Value::U8(_)
        );
        frame.stack.push(Value::Bool(result));
        Ok(())
    }

    pub(crate) fn intrinsic_slice(machine: &mut Machine, count: u8) -> Result<()> {
        // (slice "abc" 1 2) ; -> "b"
        // (slice (list 1 2 3) 1 2) ; -> (2)
        if count != 3 {
            anyhow::bail!("slice only support 3 args");
        }
        let frame = machine.get_current_frame_mut()?;

        let Some(Value::F64(end)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected int on stack for slice".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };

        let Some(Value::F64(start)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected int on stack for slice".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };

        let Some(item) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected string or list on stack for slice".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        match item {
            Value::String(string) => {
                let result = string
                    .chars()
                    .skip(start as usize)
                    .take((end - start) as usize)
                    .collect::<String>();
                frame.stack.push(Value::String(Box::new(result)));
            }
            Value::List(list) => {
                let result = list
                    .iter()
                    .skip(start as usize)
                    .take((end - start) as usize)
                    .cloned()
                    .collect::<Vec<_>>();
                frame.stack.push(Value::List(Box::new(result)));
            }
            _ => {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected string or list on stack for slice".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
        }
        Ok(())
    }

    pub(crate) fn intrinsic_join(machine: &mut Machine, count: u8) -> Result<()> {
        // (join " " "abc") ; -> "abc"
        // (join " " (list "1" "2" "3")) ; -> "1 2 3"
        if count != 2 {
            anyhow::bail!("join only support 2 args");
        }
        let frame = machine.get_current_frame_mut()?;
        let list = frame.args.pop();
        let Some(Value::List(list)) = list else {
            let ty = list.map(|i| i.type_of()).unwrap_or("NULL".to_string());
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: format!(
                    "expected list on stack for join as the second arg bug found {:?}",
                    ty
                ),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let Some(Value::String(string)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected string on stack for join".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let result = list
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(&string);
        frame.stack.push(Value::String(Box::new(result)));
        Ok(())
    }

    pub(crate) fn intrinsic_split(machine: &mut Machine, count: u8) -> Result<()> {
        // (split " " "abc") ; -> ("a" "b" "c")
        // (split " " "(+ 1 1)") ; ->  ("(+" "1" "1)")
        if count != 2 {
            anyhow::bail!("split only support 2 args");
        }
        let frame = machine.get_current_frame_mut()?;

        let Some(Value::String(string)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected string on stack for split".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };

        let Some(Value::String(item)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected string on stack for split".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };

        let result = string
            .split(&*item)
            .map(|i| Value::String(Box::new(i.to_string())))
            .collect::<Vec<_>>();
        frame.stack.push(Value::List(Box::new(result)));

        Ok(())
    }

    pub(crate) fn intrinsic_to_string(machine: &mut Machine, count: u8) -> Result<()> {
        // (to-string 1000) => "1000"
        if count != 1 {
            anyhow::bail!("to-string only support 1 args");
        }

        let frame = machine.get_current_frame_mut()?;

        let Some(value) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value on stack for to-string".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        frame.stack.push(Value::String(Box::new(value.to_string())));

        Ok(())
    }

    pub(crate) fn intrinsic_filter(machine: &mut Machine, count: u8) -> Result<()> {
        // (filter (lambda (x) (> x 1)) (list 1 2 3)) ; -> (2 3)

        if count != 2 {
            anyhow::bail!("filter only support 2 args");
        }

        let (callable, mut list) = {
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(list)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected list on stack for filter".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            let Some(Value::Callable(callable)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected lambda on stack for filter".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            (callable, list)
        };

        let mut result = Vec::new();
        for value in list.drain(..) {
            machine.push(value.clone())?;
            machine.call_from_address(
                callable.address,
                callable.span.clone(),
                1,
                &callable.name,
            )?;
            let Some(Value::Bool(true)) = machine.pop()? else {
                continue;
            };
            result.push(value);
        }
        machine.push(Value::List(Box::new(result)))?;
        Ok(())
    }

    pub(crate) fn intrinsic_fold_right(machine: &mut Machine, count: u8) -> Result<()> {
        // (fold-right 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
        if count != 3 {
            anyhow::bail!("fold only support 3 args");
        }
        let (mut list, callable, initial) = {
            let frame = machine.get_current_frame_mut()?;
            let Some(Value::List(list)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected list on stack for fold-right".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            let Some(Value::Callable(callable)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected lambda on stack for fold-right".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            let Some(initial) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected number on stack for fold-right".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            (list, callable, initial)
        };
        let mut result = initial;
        for value in list.drain(..).rev() {
            machine.push(result)?;
            machine.push(value)?;
            machine.call_from_address(
                callable.address,
                callable.span.clone(),
                2,
                &callable.name,
            )?;
            let Some(v) = machine.pop()? else {
                // TODO: ERROR REPORTING
                anyhow::bail!("expected value on stack for fold-right")
            };
            result = v;
        }
        machine.push(result)?;
        Ok(())
    }

    pub(crate) fn intrinsic_fold(machine: &mut Machine, count: u8) -> Result<()> {
        // (fold 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
        if count != 3 {
            anyhow::bail!("fold only support 3 args");
        }
        let (mut list, callable, initial, name, span) = {
            let frame = machine.get_current_frame_mut()?;
            let span = frame.span.clone();
            let name = frame.scope_name.to_string();
            let Some(Value::List(list)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span,
                    name,
                    message: "expected list on stack for fold".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            let Some(Value::Callable(callable)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span,
                    name,
                    message: "expected lambda on stack for fold".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            let Some(initial) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span,
                    name,
                    message: "expected number on stack for fold".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            (list, callable, initial, name, span)
        };
        let mut result = initial;

        for value in list.drain(..) {
            machine.push(result)?;
            machine.push(value)?;
            machine.call_from_address(
                callable.address,
                callable.span.clone(),
                2,
                &callable.name,
            )?;
            let Some(v) = machine.pop()? else {
                // TODO: ERROR REPORTING
                let error = Error::RunTimeError {
                    span,
                    name,
                    message: "expected value on stack for fold".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            result = v;
        }
        machine.push(result)?;
        Ok(())
    }

    pub(crate) fn intrinsic_map(machine: &mut Machine, count: u8) -> Result<()> {
        // (map (lambda (x) (+ x 1)) (list 1 2 3)) => (2 3 4)
        if count != 2 {
            anyhow::bail!("map only support 2 args");
        }

        let (mut list, callable, name, span) = {
            let frame = machine.get_current_frame_mut()?;
            let span = frame.span.clone();
            let name = frame.scope_name.to_string();

            let Some(Value::List(list)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span,
                    name,
                    message: "expected list on stack for map".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };

            let Some(Value::Callable(callable)) = frame.args.pop() else {
                let error = Error::RunTimeError {
                    span,
                    name,
                    message: "expected lambda on stack for map".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            (list, callable, name, span)
        };
        let mut result = Vec::new();
        for value in list.drain(..) {
            machine.push(value)?;
            machine.call_from_address(
                callable.address,
                callable.span.clone(),
                1,
                &callable.name,
            )?;
            let Some(v) = machine.pop()? else {
                // TODO: ERROR REPORTING
                let error = Error::RunTimeError {
                    span,
                    name,
                    message: "expected value on stack for map".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            };
            result.push(v);
        }

        machine.push(Value::List(Box::new(result)))?;
        Ok(())
    }

    pub(crate) fn intrinsic_nth(machine: &mut Machine, count: u8) -> Result<()> {
        // (nth (list 1 2 3) 1) => 2
        if count != 2 {
            anyhow::bail!("nth only support 2 args");
        }

        let frame = machine.get_current_frame_mut()?;
        let Some(number) = frame.args.pop() else {
            // I think this error shouldnt happen unless there is a bug in the compiler
            anyhow::bail!("expected number on stack for nth")
        };
        let Value::F64(index) = number else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value on stack for nth".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };

        match frame.args.pop() {
            Some(Value::List(mut list)) => {
                let index = index as usize;
                if index >= list.len() {
                    anyhow::bail!("nth index out of range");
                }
                let value = list.remove(index);
                frame.stack.push(value);
            }
            Some(Value::String(mut string)) => {
                let index = index as usize;
                if index >= string.len() {
                    let error = Error::RunTimeError {
                        span: frame.span.clone(),
                        name: frame.scope_name.to_string(),
                        message: "index out of range".to_string(),
                        code: "".to_string(),
                        note: None,
                        help: None,
                    };
                    anyhow::bail!(error)
                }
                let value = string.remove(index);
                frame.stack.push(Value::String(Box::new(value.to_string())));
            }
            _ => {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected list on stack for nth".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
        }
        Ok(())
    }

    pub(crate) fn intrinsic_reverse(machine: &mut Machine, count: u8) -> Result<()> {
        // (reverse (list 1 2 3)) => (3 2 1)
        if count != 1 {
            anyhow::bail!("reverse only support 1 args");
        }

        let frame = machine.get_current_frame_mut()?;

        match frame.args.pop() {
            Some(Value::List(mut list)) => {
                list.reverse();
                frame.stack.push(Value::List(list));
            }
            Some(Value::String(string)) => {
                let string = string.chars().rev().collect::<String>();
                frame.stack.push(Value::String(Box::new(string)));
            }
            _ => {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected list on stack for reverse".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
        }
        Ok(())
    }

    pub(crate) fn intrinsic_append(machine: &mut Machine, count: u8) -> Result<()> {
        // (append (list 1 2) (list 3 4)) => (1 2 3 4)
        if count != 2 {
            anyhow::bail!("append only support 2 args");
        }
        let frame = machine.get_current_frame_mut()?;
        let Some(mut rhs) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected list on stack for append".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let Some(mut lhs) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected list on stack for append".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };

        match (&mut lhs, &mut rhs) {
            (Value::List(lhs), Value::List(rhs)) => {
                lhs.append(rhs);
                frame.stack.push(Value::List(Box::new(lhs.to_vec())));
            }
            (Value::String(lhs), Value::String(rhs)) => {
                frame
                    .stack
                    .push(Value::String(Box::new(format!("{}{}", lhs, rhs))));
            }
            _ => {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: format!(
                        "expected list on stack for append but found {} and {}",
                        lhs.type_of(),
                        rhs.type_of()
                    ),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
        }
        Ok(())
    }

    pub(crate) fn intrinsic_last(machine: &mut Machine, count: u8) -> Result<()> {
        // (last (list 1 2 3 4)) => 4
        if count != 1 {
            anyhow::bail!("last only support 1 args");
        }
        let frame = machine.get_current_frame_mut()?;
        let Some(value) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value on stack for last".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        match &value {
            Value::List(list) if list.is_empty() => {
                frame.stack.push(Value::List(Box::default()));
            }
            Value::List(list) => {
                if let Some(value) = list.last() {
                    frame.stack.push(value.clone());
                    return Ok(());
                }
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected list to have at least 1 for `last`".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
            Value::String(string) => {
                if let Some(value) = string.chars().last() {
                    frame.stack.push(Value::String(Box::new(value.to_string())));
                    return Ok(());
                }
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected string to have at least 1 for `last`".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
            _ => {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected list on stack for last".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
        }
        Ok(())
    }

    pub(crate) fn intrinsic_cdr(machine: &mut Machine, count: u8) -> Result<()> {
        // (cdr (list 1 2)) => (2)
        if count != 1 {
            anyhow::bail!("cdr only support 1 args");
        }

        let frame = machine.get_current_frame_mut()?;

        let Some(value) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value on stack for cdr".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        match &value {
            Value::List(list) if list.is_empty() => {
                frame.stack.push(Value::List(Box::default()));
            }
            Value::List(list) => {
                frame.stack.push(Value::List(Box::new(list[1..].to_vec())));
            }
            _ => {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "expected list on stack for cdr".to_string(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
        }

        if let Value::List(list) = value {
            if let Some(Value::List(list)) = list.last() {
                frame.stack.push(list[0].clone());
            }
        }
        Ok(())
    }

    pub(crate) fn intrinsic_typeof(machine: &mut Machine, count: u8) -> Result<()> {
        // (type? 1) => "Number"
        if count != 1 {
            anyhow::bail!("type? only support 1 args");
        }
        let frame = machine.get_current_frame_mut()?;
        let Some(value) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value on stack for type?".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        frame.stack.push(Value::String(Box::new(value.type_of())));
        Ok(())
    }

    pub(crate) fn intrinsic_print(machine: &mut Machine, _: u8) -> Result<()> {
        use std::fmt::Write as FmtWrite;
        use std::io::Write;
        let lock = std::io::stdout().lock();
        let mut writer = std::io::BufWriter::new(lock);
        let mut output = String::new();

        let frame = machine.get_current_frame_mut()?;
        for arg in frame.args.iter() {
            write!(&mut output, "{}", arg).expect("write failed");
        }
        if let Some(arg) = frame.args.first() {
            frame.stack.push(arg.clone());
        }
        writer.write_all(output.as_bytes())?;
        writer.flush()?;

        Ok(())
    }

    pub(crate) fn intrinsic_length(machine: &mut Machine, count: u8) -> Result<()> {
        // (length (list 1 2 3)) -> 3
        if count != 1 {
            anyhow::bail!("length only support 1 args");
        }

        let frame = machine.get_current_frame_mut()?;
        let top = frame.args.pop();
        match top {
            Some(Value::String(item)) => {
                frame.stack.push(Value::F64(item.len() as f64));
            }
            Some(Value::List(item)) => {
                frame.stack.push(Value::F64(item.len() as f64));
            }
            _ => {
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: format!("expected a List on stack for length but found {top:?}"),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                anyhow::bail!(error)
            }
        }
        Ok(())
    }

    pub(crate) fn intrinsic_assert_eq<'a>(machine: &'a mut Machine, count: u8) -> Result<()> {
        // (assert-eq :expected 1 :actual 2)
        if count < 2 {
            anyhow::bail!("assert-eq at least 2 args");
        }

        let frame = machine.get_current_frame_mut()?;

        let mut keys: HashMap<String, &'a Value> = HashMap::new();
        let mut iter = frame.args.iter();
        while let Some(key) = iter.next() {
            let Some(value) = iter.next() else {
                // TODO: RUNTIME ERROR
                anyhow::bail!("expected value to key: {key:?}")
            };
            keys.insert(key.to_string(), value);
        }

        let Some(expected) = keys.get(":expected") else {
            // TODO: RUNTIME ERROR
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value to key: :expected".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let Some(actual) = keys.get(":actual") else {
            // TODO: RUNTIME ERROR
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value to key: :actual".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };

        let failed = expected != actual;
        if failed {
            eprintln!("{}", format!("expected: {expected}").green());
            eprintln!("{}", format!("actual: {actual}").red());
        }

        if let (Some(message), true) = (keys.get(":description"), failed) {
            eprintln!("{}", format!("description: {message}").yellow());
        }

        frame.stack.push(Value::Bool(!failed));
        Ok(())
    }

    pub(crate) fn intrinsic_assert(machine: &mut Machine, count: u8) -> Result<()> {
        // (assert (> 1 2) "1 is not greater than 2")
        if count != 2 {
            anyhow::bail!("assert only support 2 args");
        }

        let frame = machine.get_current_frame_mut()?;

        let Some(Value::String(message)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected string on stack for assert".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let Some(Value::Bool(value)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected boolean on stack for assert".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        if !value {
            eprintln!("assertion failed: {message}");
        }

        frame.stack.push(Value::Bool(value));

        Ok(())
    }

    pub(crate) fn intrinsic_create_list(machine: &mut Machine, _: u8) -> Result<()> {
        // (create-list 1 2 3) => (1 2 3)
        let frame = machine.get_current_frame_mut()?;
        let value = frame.args.clone();
        frame.stack.push(Value::List(Box::new(value)));
        Ok(())
    }

    pub(crate) fn intrinsic_cons(machine: &mut Machine, count: u8) -> Result<()> {
        // (cons 1 (list 2 3)) => (1 2 3)
        if count != 2 {
            anyhow::bail!("cons only support 2 args");
        }
        let frame = machine.get_current_frame_mut()?;
        let Some(Value::List(mut list)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected a List on stack for cons".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let Some(item) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value on stack for cons".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        list.insert(0, item);
        frame.stack.push(Value::List(Box::new(*list)));
        Ok(())
    }

    pub(crate) fn intrinsic_car(machine: &mut Machine, count: u8) -> Result<()> {
        // (car (list 1 2)) => 1
        if count != 1 {
            anyhow::bail!("car only support 2 args");
        }

        let frame = machine.get_current_frame_mut()?;

        let Some(Value::List(list)) = frame.args.pop() else {
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected a List on stack for car".to_string(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            anyhow::bail!(error)
        };
        let item = list.first().cloned().unwrap_or(Value::Bool(false));

        frame.stack.push(item);
        Ok(())
    }
}
