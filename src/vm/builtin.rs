use super::Machine;
use super::Value;
use anyhow::Result;
use std::io::Write;

pub fn nlvm_filter(machine: &mut Machine, count: u8) -> Result<()> {
    // (filter (lambda (x) (> x 1)) (list 1 2 3)) ; -> (2 3)
    if count != 2 {
        anyhow::bail!("filter only support 2 args");
    }
    let Some(Value::List(mut list)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for filter")
    };
    let Some(Value::Callable(address)) = machine.stack.pop() else {
        anyhow::bail!("expected lambda on stack for filter")
    };
    let mut result = Vec::new();
    for value in list.drain(..) {
        machine.stack.push(value.clone());
        machine.call(address, 1);
        let Some(Value::Bool(true)) = machine.stack.pop() else {
            continue;
        };
        result.push(value);
    }
    machine.stack.push(Value::List(result));
    Ok(())
}
pub fn nlvm_fold_right(machine: &mut Machine, count: u8) -> Result<()> {
    // (fold-right 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
    if count != 3 {
        anyhow::bail!("fold only support 3 args");
    }
    let Some(Value::List(mut list)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for fold")
    };
    let Some(Value::Callable(address)) = machine.stack.pop() else {
        anyhow::bail!("expected lambda on stack for fold")
    };
    let Some(initial) = machine.stack.pop() else {
        anyhow::bail!("expected number on stack for fold")
    };
    let mut result = initial;
    for value in list.drain(..).rev() {
        machine.stack.push(result);
        machine.stack.push(value);
        machine.call(address, 2);
        result = machine.stack.pop().unwrap();
    }
    machine.stack.push(result);
    Ok(())
}

pub fn nlvm_fold(machine: &mut Machine, count: u8) -> Result<()> {
    // (fold 0 (lambda (x y) (+ x y)) (list 1 2 3)) => 6
    if count != 3 {
        anyhow::bail!("fold only support 3 args");
    }
    let Some(Value::List(mut list)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for fold")
    };
    let Some(Value::Callable(address)) = machine.stack.pop() else {
        anyhow::bail!("expected lambda on stack for fold")
    };
    let Some(initial) = machine.stack.pop() else {
        anyhow::bail!("expected number on stack for fold")
    };
    let mut result = initial;
    for value in list.drain(..) {
        machine.stack.push(result);
        machine.stack.push(value);
        machine.call(address, 2);
        result = machine.stack.pop().unwrap();
    }
    machine.stack.push(result);
    Ok(())
}

pub fn nlvm_map(machine: &mut Machine, count: u8) -> Result<()> {
    // (map (lambda (x) (+ x 1)) (list 1 2 3)) => (2 3 4)
    if count != 2 {
        anyhow::bail!("map only support 2 args");
    }

    let Some(Value::List(mut list)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for map")
    };

    let Some(Value::Callable(address)) = machine.stack.pop() else {
        anyhow::bail!("expected lambda on stack for map")
    };
    let mut result = Vec::new();
    for value in list.drain(..) {
        machine.stack.push(value);
        machine.call(address, 1);
        result.push(machine.stack.pop().unwrap());
    }

    machine.stack.push(Value::List(result));
    Ok(())
}

pub fn nlvm_nth(machine: &mut Machine, count: u8) -> Result<()> {
    // (nth (list 1 2 3) 1) => 2
    if count != 2 {
        anyhow::bail!("nth only support 2 args");
    }
    let Some(Value::F64(index)) = machine.stack.pop() else {
        anyhow::bail!("expected number on stack for nth")
    };
    let Some(Value::List(mut list)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for nth")
    };
    let index = index as usize;
    if index >= list.len() {
        anyhow::bail!("nth index out of range");
    }
    let value = list.remove(index);
    machine.stack.push(value);
    Ok(())
}

pub fn nlvm_reverse(machine: &mut Machine, count: u8) -> Result<()> {
    // (reverse (list 1 2 3)) => (3 2 1)
    if count != 1 {
        anyhow::bail!("reverse only support 1 args");
    }
    let Some(Value::List(mut list)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for reverse")
    };
    list.reverse();
    machine.stack.push(Value::List(list));
    Ok(())
}

pub fn nlvm_append(machine: &mut Machine, count: u8) -> Result<()> {
    // (append (list 1 2) (list 3 4)) => (1 2 3 4)
    if count != 2 {
        anyhow::bail!("append only support 2 args");
    }
    let Some(Value::List(mut list2)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for append")
    };
    let Some(Value::List(mut list1)) = machine.stack.pop() else {
        anyhow::bail!("expected list on stack for append")
    };

    list1.append(&mut list2);
    machine.stack.push(Value::List(list1));
    Ok(())
}

pub fn nlvm_last(machine: &mut Machine, count: u8) -> Result<()> {
    // (last (list 1 2 3 4)) => 4
    if count != 1 {
        anyhow::bail!("last only support 1 args");
    }
    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for last")
    };
    match &value {
        Value::List(list) if list.is_empty() => {
            machine.stack.push(Value::List(vec![]));
        }
        Value::List(list) => {
            if let Some(value) = list.last() {
                machine.stack.push(value.clone());
                return Ok(());
            }
            anyhow::bail!("expected list to have at least 1 for `last`");
        }
        _ => anyhow::bail!("expected list on stack for last"),
    }
    Ok(())
}

pub fn nlvm_cdr(machine: &mut Machine, count: u8) -> Result<()> {
    // (cdr (list 1 2)) => (2)
    if count != 1 {
        anyhow::bail!("cdr only support 1 args");
    }

    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for cdr")
    };
    match &value {
        Value::List(list) if list.is_empty() => {
            machine.stack.push(Value::List(vec![]));
        }
        Value::List(list) => {
            machine.stack.push(Value::List(list[1..].to_vec()));
        }
        _ => anyhow::bail!("expected list on stack for cdr"),
    }

    if let Value::List(list) = value {
        if let Some(Value::List(list)) = list.last() {
            machine.stack.push(list[0].clone());
        }
    }
    Ok(())
}

pub fn nlvm_typeof(machine: &mut Machine, count: u8) -> Result<()> {
    // (typeof 1) => "Number"
    if count != 1 {
        anyhow::bail!("typeof only support 1 args");
    }
    let Some(value) = machine.stack.pop() else {
        panic!("expected value on stack for typeof")
    };
    let output = format!("{:?}", value.type_of());
    machine.stack.push(Value::String(output));
    Ok(())
}

pub fn nlvm_print(machine: &mut Machine, count: u8) -> Result<()> {
    let lock = std::io::stdout().lock();
    let mut writer = std::io::BufWriter::new(lock);
    let mut output = String::new();

    let mut items = Vec::new();
    for i in 0..count {
        let Some(value) = machine.stack.pop() else {
            panic!("expected value on stack for print")
        };
        output.insert_str(0, &format!("{}", value));
        if i == count - 1 {
            items.push(value);
        }
    }
    writer.write_all(output.as_bytes())?;
    writer.flush()?;

    for items in items.into_iter().rev() {
        machine.stack.push(items);
    }

    Ok(())
}

pub fn length(machine: &mut Machine, count: u8) -> Result<()> {
    // (length list)
    if count != 1 {
        anyhow::bail!("length only support 1 args");
    }

    let Some(Value::List(list)) = machine.stack.pop() else {
        anyhow::bail!("expected a List on stack for nth");
    };

    machine.stack.push(Value::F64(list.len() as f64));
    Ok(())
}

pub fn nlvm_assert_eq(machine: &mut Machine, count: u8) -> Result<()> {
    let Some(result) = machine.stack.pop() else {
        anyhow::bail!("expected a value on stack for assert-eq");
    };

    let mut failed = false;
    let mut message = String::new();
    for _ in 0..count - 1 {
        let Some(value) = machine.stack.pop() else {
            anyhow::bail!("expected a value on stack for assert-eq");
        };
        failed = result != value;
        if failed {
            message = format!("assert-eq failed expected {} got {}", result, value);
            break;
        }
    }
    if failed {
        if let Some(Value::String(result)) = machine.stack.pop() {
            eprintln!("TEST: {}", result);
        }
        anyhow::bail!(message);
    }

    Ok(())
}
pub fn list(machine: &mut Machine, count: u8) -> Result<()> {
    let mut items = Vec::new();
    for _ in 0..count {
        let Some(item) = machine.stack.pop() else {
            panic!("expected value on stack for CreateList")
        };
        items.insert(0, item);
    }
    machine.stack.push(Value::List(items));
    Ok(())
}

pub fn cons(machine: &mut Machine, count: u8) -> Result<()> {
    if count != 2 {
        anyhow::bail!("cons only support 2 args");
    }
    let Some(Value::List(mut list)) = machine.stack.pop() else {
        panic!("expected a List on stack for cons")
    };
    let Some(item) = machine.stack.pop() else {
        panic!("expected value on stack for cons")
    };
    list.insert(0, item);
    machine.stack.push(Value::List(list));
    Ok(())
}

pub fn car(machine: &mut Machine, count: u8) -> Result<()> {
    if count != 1 {
        anyhow::bail!("car only support 2 args");
    }
    let Some(Value::List(list)) = machine.stack.pop() else {
        panic!("expected a List on stack for car")
    };
    let item = list.first().cloned().unwrap_or(Value::Bool(false));

    machine.stack.push(item);
    Ok(())
}
