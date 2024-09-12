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

    for _ in 0..count - 1 {
        let Some(value) = machine.pop() else {
            anyhow::bail!("expected a value on stack for assert-eq");
        };
        if result != value {
            anyhow::bail!("assert-eq failed expected {} got {}", result, value);
        }
    }

    Ok(())
}
