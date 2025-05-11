use crate::error::Error;
use crate::instruction::{Value, ValueKind};
use crate::machine::Machine;
use crossterm::style::Stylize;
use std::collections::HashMap;
use std::io::Write;

type Result<T> = std::result::Result<T, Box<Error>>;

pub(crate) struct Function;
impl Function {
    pub(crate) fn fn_sleep(machine: &mut Machine) -> Result<()> {
        // (sleep 1000) ; -> false
        machine.check_arg_count(1)?;
        let Value::F64(value) = machine.pop_arg(1, Some(&[ValueKind::F64]))? else {
            unreachable!()
        };
        std::thread::sleep(std::time::Duration::from_millis(value as u64));
        Ok(())
    }

    pub(crate) fn fn_is_atom(machine: &mut Machine) -> Result<()> {
        // (atom? 10) ; -> true
        // (atom? "10") ; -> true
        // (atom? "abc") ; -> true
        // (atom? '(1 2 3)) ; -> false
        machine.check_arg_count(1)?;

        let frame = machine.get_current_frame_mut()?;
        let arg1 = frame.args.pop().unwrap();
        let result = !matches!(
            arg1,
            Value::List(_) | Value::Callable(_) | Value::Builtin(_) | Value::Struct(_)
        );
        frame.stack.push(Value::Bool(result));
        Ok(())
    }

    pub(crate) fn fn_is_number(machine: &mut Machine) -> Result<()> {
        // (number? 10) ; -> true
        // (number? "10") ; -> true
        // (number? "abc") ; -> false
        // (number? '(1 2 3)) ; -> false
        machine.check_arg_count(1)?;
        let arg1 = machine.pop_arg(1, None)?;
        let result = matches!(
            arg1,
            Value::F64(_) | Value::U32(_) | Value::I32(_) | Value::F32(_) | Value::U8(_)
        );
        machine.push(Value::Bool(result))?;
        Ok(())
    }

    pub(crate) fn fn_slice(machine: &mut Machine) -> Result<()> {
        // (slice "abc" 1 2) ; -> "b"
        // (slice (list 1 2 3) 1 2) ; -> (2)
        machine.check_arg_count(3)?;
        let Value::F64(end) = machine.pop_arg(1, Some(&[ValueKind::F64]))? else {
            unreachable!()
        };
        let Value::F64(start) = machine.pop_arg(2, Some(&[ValueKind::F64]))? else {
            unreachable!()
        };
        let arg3 = machine.pop_arg(3, Some(&[ValueKind::String, ValueKind::List]))?;
        if let Value::String(string) = arg3 {
            let result = string
                .chars()
                .skip(start as usize)
                .take((end - start) as usize)
                .collect::<String>();
            machine.push(Value::String(Box::new(result)))?;
        } else if let Value::List(list) = arg3 {
            let result = list
                .iter()
                .skip(start as usize)
                .take((end - start) as usize)
                .cloned()
                .collect::<Vec<_>>();
            machine.push(Value::List(Box::new(result)))?;
        }
        Ok(())
    }

    pub(crate) fn fn_join(machine: &mut Machine) -> Result<()> {
        // (join " " "abc") ; -> "abc"
        // (join " " (list "1" "2" "3")) ; -> "1 2 3"
        machine.check_arg_count(2)?;
        let Value::List(list) = machine.pop_arg(1, Some(&[ValueKind::List]))? else {
            unreachable!()
        };
        let Value::String(string) = machine.pop_arg(1, Some(&[ValueKind::String]))? else {
            unreachable!()
        };
        let result = list
            .iter()
            .map(|i| i.to_string())
            .collect::<Vec<_>>()
            .join(&string);
        let frame = machine.get_current_frame_mut()?;
        frame.stack.push(Value::String(Box::new(result)));
        Ok(())
    }

    pub(crate) fn fn_split(machine: &mut Machine) -> Result<()> {
        // (split " " "abc") ; -> ("a" "b" "c")
        // (split " " "(+ 1 1)") ; ->  ("(+" "1" "1)")
        machine.check_arg_count(2)?;

        let Value::String(string) = machine.pop_arg(1, Some(&[ValueKind::String]))? else {
            unreachable!();
        };
        let Value::String(item) = machine.pop_arg(2, Some(&[ValueKind::String]))? else {
            unreachable!();
        };

        let frame = machine.get_current_frame_mut()?;

        let result = string
            .split(&*item)
            .map(|i| Value::String(Box::new(i.to_string())))
            .collect::<Vec<_>>();
        frame.stack.push(Value::List(Box::new(result)));

        Ok(())
    }

    pub(crate) fn fn_to_string(machine: &mut Machine) -> Result<()> {
        // (to-string 1000) => "1000"
        machine.check_arg_count(1)?;
        let arg1 = machine.pop_arg(1, None)?;
        machine.push(Value::String(Box::new(arg1.to_string())))?;
        Ok(())
    }

    pub(crate) fn fn_filter(machine: &mut Machine) -> Result<()> {
        // (filter (lambda (x) (> x 1)) (list 1 2 3)) ; -> (2 3)
        machine.check_arg_count(2)?;
        let Value::List(mut list) = machine.pop_arg(1, Some(&[ValueKind::List]))? else {
            unreachable!();
        };
        let Value::Callable(callable) = machine.pop_arg(2, Some(&[ValueKind::Callable]))? else {
            unreachable!();
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
            let Value::Bool(true) = machine.pop()? else {
                continue;
            };
            result.push(value);
        }
        machine.push(Value::List(Box::new(result)))?;
        Ok(())
    }

    pub(crate) fn fn_fold_right(machine: &mut Machine) -> Result<()> {
        // (fold-right 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
        machine.check_arg_count(3)?;
        let Value::List(mut list) = machine.pop_arg(2, Some(&[ValueKind::List]))? else {
            unreachable!();
        };
        let Value::Callable(callable) = machine.pop_arg(2, Some(&[ValueKind::Callable]))? else {
            unreachable!();
        };
        let mut result = machine.pop_arg(3, None)?;
        // FIXME: all map like functions need to be rewritten
        for value in list.drain(..).rev() {
            machine.push(result)?;
            machine.push(value)?;
            machine.call_from_address(
                callable.address,
                callable.span.clone(),
                2,
                &callable.name,
            )?;
            result = machine.pop()?;
        }
        machine.push(result)?;
        Ok(())
    }

    pub(crate) fn fn_fold(machine: &mut Machine) -> Result<()> {
        // (fold 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
        machine.check_arg_count(3)?;
        let Value::List(mut list) = machine.pop_arg(2, Some(&[ValueKind::List]))? else {
            unreachable!();
        };
        let Value::Callable(callable) = machine.pop_arg(2, Some(&[ValueKind::Callable]))? else {
            unreachable!();
        };
        let mut result = machine.pop_arg(3, None)?;

        for value in list.drain(..) {
            machine.push(result)?;
            machine.push(value)?;
            machine.call_from_address(
                callable.address,
                callable.span.clone(),
                2,
                &callable.name,
            )?;
            result = machine.pop()?;
        }
        machine.push(result)?;
        Ok(())
    }

    pub(crate) fn fn_map(machine: &mut Machine) -> Result<()> {
        // (map (lambda (x) (+ x 1)) (list 1 2 3)) => (2 3 4)
        machine.check_arg_count(2)?;
        let Value::List(mut list) = machine.pop_arg(1, Some(&[ValueKind::List]))? else {
            unreachable!();
        };
        let Value::Callable(callable) = machine.pop_arg(2, Some(&[ValueKind::Callable]))? else {
            unreachable!();
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
            let v = machine.pop()?;
            result.push(v);
        }

        machine.push(Value::List(Box::new(result)))?;
        Ok(())
    }

    pub(crate) fn fn_nth(machine: &mut Machine) -> Result<()> {
        // (nth (list 1 2 3) 1) => 2
        machine.check_arg_count(2)?;
        let Value::F64(index) = machine.pop_arg(1, Some(&[ValueKind::F64]))? else {
            unreachable!();
        };
        let arg2 = machine.pop_arg(2, None)?;

        if let Value::List(mut list) = arg2 {
            let index = index as usize;
            if index >= list.len() {
                // TODO: ERROR REPORTING
                panic!("nth index out of range");
            }
            let value = list.remove(index);
            machine.push(value)?;
        } else if let Value::String(mut string) = arg2 {
            let index = index as usize;
            if index >= string.len() {
                // TODO: ERROR REPORTING
                let frame = machine.get_current_frame()?;
                let error = Error::RunTimeError {
                    span: frame.span.clone(),
                    name: frame.scope_name.to_string(),
                    message: "index out of range".to_string(),
                    stack_trace: machine.create_stack_trace(),
                    code: "".to_string(),
                    note: None,
                    help: None,
                };
                return Err(Box::new(error));
            }
            let value = string.remove(index);
            machine.push(Value::String(Box::new(value.to_string())))?;
        }

        Ok(())
    }

    pub(crate) fn fn_reverse(machine: &mut Machine) -> Result<()> {
        // (reverse (list 1 2 3)) => (3 2 1)
        machine.check_arg_count(1)?;
        let arg1 = machine.pop_arg(1, None)?;

        if let Value::List(mut list) = arg1 {
            list.reverse();
            machine.push(Value::List(list))?;
        } else if let Value::String(string) = arg1 {
            let string = string.chars().rev().collect::<String>();
            machine.push(Value::String(Box::new(string)))?;
        }

        Ok(())
    }

    pub(crate) fn fn_append(machine: &mut Machine) -> Result<()> {
        // (append (list 1 2) (list 3 4)) => (1 2 3 4)
        machine.check_arg_count(2)?;
        let mut rhs = machine.pop_arg(1, Some(&[ValueKind::List, ValueKind::String]))?;
        let mut lhs = machine.pop_arg(2, Some(&[rhs.kind()]))?;
        if let (Value::List(lhs), Value::List(rhs)) = (&mut lhs, &mut rhs) {
            lhs.append(rhs);
            machine.push(Value::List(Box::new(lhs.to_vec())))?;
        } else if let (Value::String(lhs), Value::String(rhs)) = (lhs, rhs) {
            machine.push(Value::String(Box::new(format!("{}{}", lhs, rhs))))?;
        }
        Ok(())
    }

    pub(crate) fn fn_last(machine: &mut Machine) -> Result<()> {
        // (last (list 1 2 3 4)) => 4
        machine.check_arg_count(1)?;
        let arg1 = machine.pop_arg(1, Some(&[ValueKind::List, ValueKind::String]))?;
        if let Value::List(list) = arg1 {
            if let Some(value) = list.last() {
                machine.push(value.clone())?;
                return Ok(());
            }
            machine.push(Value::Nil)?;
            return Ok(());
        } else if let Value::String(string) = arg1 {
            if let Some(value) = string.chars().last() {
                machine.push(Value::String(Box::new(value.to_string())))?;
                return Ok(());
            }
            machine.push(Value::Nil)?;
            return Ok(());
        }
        unreachable!("last function");
    }

    pub(crate) fn fn_cdr(machine: &mut Machine) -> Result<()> {
        // (cdr (list 1 2)) => (2)
        machine.check_arg_count(1)?;
        let Value::List(list) = machine.pop_arg(1, Some(&[ValueKind::List]))? else {
            unreachable!();
        };

        if list.is_empty() {
            machine.push(Value::Nil)?;
            return Ok(());
        }

        machine.push(Value::List(Box::new(list[1..].to_vec())))?;

        Ok(())
    }

    pub(crate) fn fn_typeof(machine: &mut Machine) -> Result<()> {
        // (type? 1) => :i32
        machine.check_arg_count(1)?;
        let value = machine.pop_arg(1, None)?;
        machine.push(Value::Keyword(Box::new(value.type_of())))?;
        Ok(())
    }

    pub(crate) fn fn_print(machine: &mut Machine) -> Result<()> {
        use std::fmt::Write as FmtWrite;
        use std::io::Write;
        let lock = std::io::stdout().lock();
        let mut writer = std::io::BufWriter::new(lock);
        let mut output = String::new();

        let frame = machine.get_current_frame_mut()?;
        for arg in frame.args.iter() {
            write!(&mut output, "{}", arg).expect("write failed");
        }

        writer.write_all(output.as_bytes()).expect("write failed");
        writer.flush().expect("flush failed");

        frame.stack.push(Value::Nil);
        Ok(())
    }

    pub(crate) fn fn_input(machine: &mut Machine) -> Result<()> {
        // (input)
        // (input "message")
        if let Ok(Value::String(message)) = machine.pop_arg(1, Some(&[ValueKind::String])) {
            print!("{}", message);
            std::io::stdout().flush().unwrap();
        }
        let mut input = String::new();
        let Ok(_) = std::io::stdin().read_line(&mut input) else {
            return Err(Box::new(Error::RunTimeError {
                span: machine.get_current_frame()?.span.clone(),
                name: machine.get_current_frame()?.scope_name.to_string(),
                message: "input failed".to_string(),
                stack_trace: machine.create_stack_trace(),
                code: "".to_string(),
                note: None,
                help: None,
            }));
        };
        machine.push(Value::String(Box::new(input)))?;
        Ok(())
    }

    pub(crate) fn fn_length(machine: &mut Machine) -> Result<()> {
        // (length (list 1 2 3)) -> 3
        machine.check_arg_count(1)?;
        let arg1 = machine.pop_arg(1, Some(&[ValueKind::List, ValueKind::String]))?;
        if let Value::String(item) = arg1 {
            machine.push(Value::F64(item.len() as f64))?;
            return Ok(());
        } else if let Value::List(item) = arg1 {
            machine.push(Value::F64(item.len() as f64))?;
            return Ok(());
        }
        panic!("length function");
    }

    pub(crate) fn fn_assert_eq(machine: &mut Machine) -> Result<()> {
        // (assert-eq :expected 1 :actual 2 :description "description")
        machine.check_arg_count(6)?;

        let mut keys: HashMap<String, Value> = HashMap::new();
        let mut iter = machine.get_current_frame_mut()?.args.iter();
        while let Some(key) = iter.next() {
            let Some(value) = iter.next() else {
                // TODO: RUNTIME ERROR
                panic!("expected value to key: {key:?}");
            };
            keys.insert(key.to_string(), value.clone());
        }

        // TODO: This could be greately cleaned up.
        // Would be nice if you used keywords infront of the values then the order shouldnt matter.
        // (assert-eq :expected 1 :actual 2)
        // also description should be optional
        let Some(expected) = keys.get(":expected") else {
            // TODO: RUNTIME ERROR
            let frame = machine.get_current_frame_mut()?;
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value to key: :expected".to_string(),
                stack_trace: machine.create_stack_trace(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            return Err(Box::new(error));
        };
        let Some(actual) = keys.get(":actual") else {
            // TODO: RUNTIME ERROR
            let frame = machine.get_current_frame_mut()?;
            let error = Error::RunTimeError {
                span: frame.span.clone(),
                name: frame.scope_name.to_string(),
                message: "expected value to key: :actual".to_string(),
                stack_trace: machine.create_stack_trace(),
                code: "".to_string(),
                note: None,
                help: None,
            };
            return Err(Box::new(error));
        };

        let failed = expected != actual;
        if failed {
            eprintln!("{}", format!("expected: {expected}").green());
            eprintln!("{}", format!("actual: {actual}").red());
        }

        if let (Some(message), true) = (keys.get(":description"), failed) {
            eprintln!("{}", format!("description: {message}").yellow());
        }

        machine.push(Value::Bool(!failed))?;
        Ok(())
    }

    pub(crate) fn fn_assert(machine: &mut Machine) -> Result<()> {
        // (assert (> 1 2) "1 is not greater than 2")
        machine.check_arg_count(2)?;

        let Value::String(message) = machine.pop_arg(1, Some(&[ValueKind::String]))? else {
            unreachable!()
        };
        let Value::Bool(value) = machine.pop_arg(2, Some(&[ValueKind::Bool]))? else {
            unreachable!()
        };
        if !value {
            eprintln!("assertion failed: {message}");
        }

        let frame = machine.get_current_frame_mut()?;
        frame.stack.push(Value::Bool(value));

        Ok(())
    }

    pub(crate) fn fn_create_list(machine: &mut Machine) -> Result<()> {
        // (create-list 1 2 3) => (1 2 3)
        let frame = machine.get_current_frame_mut()?;
        let value = frame.args.clone();
        frame.stack.push(Value::List(Box::new(value)));
        Ok(())
    }

    pub(crate) fn fn_cons(machine: &mut Machine) -> Result<()> {
        // (cons 1 (list 2 3)) => (1 2 3)
        machine.check_arg_count(2)?;

        let Value::List(mut list) = machine.pop_arg(1, Some(&[ValueKind::List]))? else {
            unreachable!()
        };
        let arg2 = machine.pop_arg(2, None)?;
        list.insert(0, arg2);

        machine.push(Value::List(Box::new(*list)))?;
        Ok(())
    }

    pub(crate) fn fn_car(machine: &mut Machine) -> Result<()> {
        // (car (list 1 2)) => 1
        machine.check_arg_count(1)?;

        let Value::List(list) = machine.pop_arg(1, Some(&[ValueKind::List]))? else {
            unreachable!();
        };

        let Some(item) = list.first() else {
            machine.push(Value::Nil)?;
            return Ok(());
        };

        machine.push(item.clone())?;
        Ok(())
    }

    pub(crate) fn fn_string_trim_left(machine: &mut Machine) -> Result<()> {
        // (string-trim-left "   hello") => "hello"
        machine.check_arg_count(1)?;

        let Value::String(string) = machine.pop_arg(1, Some(&[ValueKind::String]))? else {
            unreachable!();
        };

        let trimmed = string.trim_start();
        machine.push(Value::String(Box::new(trimmed.to_string())))?;
        Ok(())
    }

    pub(crate) fn fn_string_trim_right(machine: &mut Machine) -> Result<()> {
        // (string-trim-right "hello   \n") => "hello"
        machine.check_arg_count(1)?;

        let Value::String(string) = machine.pop_arg(1, Some(&[ValueKind::String]))? else {
            unreachable!();
        };

        let trimmed = string.trim_end();
        machine.push(Value::String(Box::new(trimmed.to_string())))?;
        Ok(())
    }

    pub(crate) fn fn_string_trim(machine: &mut Machine) -> Result<()> {
        // (string-trim-right "   hello   \n") => "hello"
        machine.check_arg_count(1)?;

        let Value::String(string) = machine.pop_arg(1, Some(&[ValueKind::String]))? else {
            unreachable!();
        };

        let trimmed = string.trim();
        machine.push(Value::String(Box::new(trimmed.to_string())))?;
        Ok(())
    }

    pub(crate) fn fn_print_fmt(machine: &mut Machine) -> Result<()> {
        // (print-fmt "hello {}" "world") => "hello world"
        use std::io::Write;
        let frame = machine.get_current_frame_mut()?;
        let args = frame.args.iter().skip(1).collect::<Vec<_>>();
        let Some(Value::String(format_arg)) = frame.args.first() else {
            let span = frame.span.clone();
            let stack_trace = machine.create_stack_trace();
            let error = Error::RunTimeError {
                name: "Expected String".to_string(),
                span,
                stack_trace,
                message: "First argument to print-fmt must be a format string".to_string(),
                code: "E012".to_string(),
                note: None,
                help: None,
            };
            return Err(Box::new(error));
        };
        let output = format_from_vec(format_arg, &args);
        let lock = std::io::stdout().lock();
        let mut writer = std::io::BufWriter::new(lock);
        writer.write_all(output.as_bytes()).expect("write failed");
        writer.flush().expect("flush failed");
        frame.stack.push(Value::Nil);
        Ok(())
    }
}

fn format_from_vec(fmt: &str, args: &[&Value]) -> String {
    let mut result = String::new();
    let mut parts = fmt.split("{}");
    let mut iter = args.iter();

    if let Some(first) = parts.next() {
        result.push_str(first);
    }

    for part in parts {
        match iter.next() {
            Some(value) => result.push_str(&value.to_string()),
            None => result.push_str("{}"),
        }
        result.push_str(part);
    }

    result
}
