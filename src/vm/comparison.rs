use super::core::VM;
use crate::value::Value;

pub fn handle_eq(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    vm.stack.push(Value::Bool(a == b));
    Ok(())
}

pub fn handle_lt(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x < y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x < y),
        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) < y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x < (y as f64)),
        _ => return Err("Type error in comparison".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}

pub fn handle_gt(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x > y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x > y),
        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) > y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x > (y as f64)),
        _ => return Err("Type error in comparison".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}

pub fn handle_le(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x <= y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x <= y),
        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) <= y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x <= (y as f64)),
        _ => return Err("Type error in comparison".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}

pub fn handle_ge(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Bool(x >= y),
        (Value::Float(x), Value::Float(y)) => Value::Bool(x >= y),
        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) >= y),
        (Value::Float(x), Value::Int(y)) => Value::Bool(x >= (y as f64)),
        _ => return Err("Type error in comparison".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}
