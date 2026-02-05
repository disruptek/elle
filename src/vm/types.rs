use super::core::VM;
use crate::value::Value;

pub fn handle_is_nil(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    vm.stack.push(Value::Bool(val.is_nil()));
    Ok(())
}

pub fn handle_is_pair(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    vm.stack.push(Value::Bool(matches!(val, Value::Cons(_))));
    Ok(())
}

pub fn handle_is_number(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    vm.stack
        .push(Value::Bool(matches!(val, Value::Int(_) | Value::Float(_))));
    Ok(())
}

pub fn handle_is_symbol(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    vm.stack.push(Value::Bool(matches!(val, Value::Symbol(_))));
    Ok(())
}

pub fn handle_not(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    vm.stack.push(Value::Bool(!val.is_truthy()));
    Ok(())
}
