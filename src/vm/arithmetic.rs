use super::core::VM;
use crate::value::Value;

pub fn handle_add_int(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    let a = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    vm.stack.push(Value::Int(a + b));
    Ok(())
}

pub fn handle_sub_int(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    let a = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    vm.stack.push(Value::Int(a - b));
    Ok(())
}

pub fn handle_mul_int(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    let a = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    vm.stack.push(Value::Int(a * b));
    Ok(())
}

pub fn handle_div_int(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    let a = vm.stack.pop().ok_or("Stack underflow")?.as_int()?;
    if b == 0 {
        return Err("Division by zero".to_string());
    }
    vm.stack.push(Value::Int(a / b));
    Ok(())
}

pub fn handle_add(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 + y),
        (Value::Float(x), Value::Int(y)) => Value::Float(x + y as f64),
        _ => return Err("Type error in addition".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}

pub fn handle_sub(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x - y),
        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 - y),
        (Value::Float(x), Value::Int(y)) => Value::Float(x - y as f64),
        _ => return Err("Type error in subtraction".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}

pub fn handle_mul(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
        (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 * y),
        (Value::Float(x), Value::Int(y)) => Value::Float(x * y as f64),
        _ => return Err("Type error in multiplication".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}

pub fn handle_div(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = match (a, b) {
        (Value::Int(x), Value::Int(y)) => {
            if y == 0 {
                return Err("Division by zero".to_string());
            }
            Value::Int(x / y)
        }
        (Value::Float(x), Value::Float(y)) => Value::Float(x / y),
        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 / y),
        (Value::Float(x), Value::Int(y)) => Value::Float(x / y as f64),
        _ => return Err("Type error in division".to_string()),
    };
    vm.stack.push(result);
    Ok(())
}
