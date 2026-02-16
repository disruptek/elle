use super::core::VM;
use crate::arithmetic;
use crate::value::Value;

pub fn handle_add_int(vm: &mut VM) -> Result<(), String> {
    let b = vm
        .stack
        .pop()
        .ok_or("Stack underflow")?
        .as_int()
        .ok_or("Expected integer")?;
    let a = vm
        .stack
        .pop()
        .ok_or("Stack underflow")?
        .as_int()
        .ok_or("Expected integer")?;
    vm.stack.push(Value::int(a + b));
    Ok(())
}

pub fn handle_sub_int(vm: &mut VM) -> Result<(), String> {
    let b = vm
        .stack
        .pop()
        .ok_or("Stack underflow")?
        .as_int()
        .ok_or("Expected integer")?;
    let a = vm
        .stack
        .pop()
        .ok_or("Stack underflow")?
        .as_int()
        .ok_or("Expected integer")?;
    vm.stack.push(Value::int(a - b));
    Ok(())
}

pub fn handle_mul_int(vm: &mut VM) -> Result<(), String> {
    let b = vm
        .stack
        .pop()
        .ok_or("Stack underflow")?
        .as_int()
        .ok_or("Expected integer")?;
    let a = vm
        .stack
        .pop()
        .ok_or("Stack underflow")?
        .as_int()
        .ok_or("Expected integer")?;
    vm.stack.push(Value::int(a * b));
    Ok(())
}

pub fn handle_div_int(vm: &mut VM) -> Result<(), String> {
    let b_val = vm.stack.pop().ok_or("Stack underflow")?;
    let a_val = vm.stack.pop().ok_or("Stack underflow")?;
    let b = b_val.as_int().ok_or("Expected integer")?;
    let a = a_val.as_int().ok_or("Expected integer")?;
    if b == 0 {
        // Create a division-by-zero Condition
        // Exception ID 4 is "division-by-zero" from ExceptionRegistry
        let mut cond = crate::value::Condition::new(4);
        cond.set_field(0, Value::int(a)); // dividend
        cond.set_field(1, Value::int(b)); // divisor
        vm.current_exception = Some(std::rc::Rc::new(cond));
        // Push a marker value (nil) to keep stack consistent
        // The exception interrupt mechanism will handle the exception
        vm.stack.push(Value::NIL);
        return Ok(());
    }
    vm.stack.push(Value::int(a / b));
    Ok(())
}

pub fn handle_add(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = arithmetic::add_values(&a, &b)?;
    vm.stack.push(result);
    Ok(())
}

pub fn handle_sub(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = arithmetic::sub_values(&a, &b)?;
    vm.stack.push(result);
    Ok(())
}

pub fn handle_mul(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;
    let result = arithmetic::mul_values(&a, &b)?;
    vm.stack.push(result);
    Ok(())
}

pub fn handle_div(vm: &mut VM) -> Result<(), String> {
    let b = vm.stack.pop().ok_or("Stack underflow")?;
    let a = vm.stack.pop().ok_or("Stack underflow")?;

    // Check for division by zero and set exception instead of returning error
    let is_zero = match (a.as_int(), b.as_int()) {
        (Some(_), Some(y)) => y == 0,
        _ => match (a.as_float(), b.as_float()) {
            (Some(_), Some(y)) => y == 0.0,
            _ => match (a.as_int(), b.as_float()) {
                (Some(_), Some(y)) => y == 0.0,
                _ => match (a.as_float(), b.as_int()) {
                    (Some(_), Some(y)) => y == 0,
                    _ => false,
                },
            },
        },
    };

    if is_zero {
        // Create a division-by-zero Condition
        // Exception ID 4 is "division-by-zero"
        let mut cond = crate::value::Condition::new(4);
        cond.set_field(0, a); // dividend
        cond.set_field(1, b); // divisor
        vm.current_exception = Some(std::rc::Rc::new(cond));
        // Push a marker value (nil) to keep stack consistent
        vm.stack.push(Value::NIL);
        return Ok(());
    }

    let result = arithmetic::div_values(&a, &b)?;
    vm.stack.push(result);
    Ok(())
}
