use super::Machine;
use super::Value;
use anyhow::Result;

pub fn nth(machine: &mut Machine, count: u32) -> Result<()> {
    // (nth list index)
    if count != 2 {
        anyhow::bail!("nth only support 2 args");
    }

    let Some(Value::F64(index)) = machine.pop() else {
        anyhow::bail!("expected an index on stack for nth");
    };

    let Some(Value::List(list)) = machine.pop() else {
        anyhow::bail!("expected a List on stack for nth");
    };

    let index = index as usize;
    let Some(value) = list.get(index) else {
        anyhow::bail!("nth index out of bounds");
    };
    machine.push(value.clone());
    Ok(())
}

pub fn length(machine: &mut Machine, count: u32) -> Result<()> {
    // (length list)
    if count != 1 {
        anyhow::bail!("length only support 1 args");
    }

    let Some(Value::List(list)) = machine.pop() else {
        anyhow::bail!("expected a List on stack for nth");
    };

    machine.push(Value::F64(list.len() as f64));
    Ok(())
}

pub fn nlvm_assert_eq(machine: &mut Machine, count: u32) -> Result<()> {
    let Some(result) = machine.pop() else {
        anyhow::bail!("expected a value on stack for assert-eq");
    };

    let mut failed = false;
    let mut message = String::new();
    for _ in 0..count - 1 {
        let Some(value) = machine.pop() else {
            anyhow::bail!("expected a value on stack for assert-eq");
        };
        failed = result != value;
        if failed {
            message = format!("assert-eq failed expected {} got {}", result, value);
            break;
        }
    }
    if failed {
        if let Some(Value::String(result)) = machine.pop() {
            eprintln!("TEST: {}", result);
        }
        anyhow::bail!(message);
    }

    Ok(())
}
pub fn list(machine: &mut Machine, count: u32) -> Result<()> {
    let mut items = Vec::new();
    for _ in (0..count) {
        let Some(item) = machine.pop() else {
            panic!("expected value on stack for CreateList")
        };
        items.insert(0, item);
    }
    machine.push(Value::List(items));
    Ok(())
}
