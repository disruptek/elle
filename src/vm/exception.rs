use crate::value::Value;
use crate::vm::core::{ExceptionHandler, VM};
use std::rc::Rc;

pub fn handle_push_handler(vm: &mut VM, bytecode: &[u8], ip: &mut usize) {
    let handler_offset = vm.read_i16(bytecode, ip);
    let finally_offset = vm.read_i16(bytecode, ip);

    let handler = ExceptionHandler {
        handler_offset,
        finally_offset: if finally_offset >= 0 {
            Some(finally_offset)
        } else {
            None
        },
        stack_depth: vm.stack.len(),
    };

    vm.exception_handlers.push(handler);
}

pub fn handle_pop_handler(vm: &mut VM) {
    vm.exception_handlers.pop();
}

pub fn handle_throw(vm: &mut VM) -> Result<(), String> {
    let value = vm.stack.pop().ok_or("Stack underflow")?;

    let exc = match value {
        Value::Exception(e) => e,
        Value::String(s) => {
            let exc = crate::value::Exception::new((*s).to_string());
            Rc::new(exc)
        }
        _ => {
            return Err(format!("Cannot throw {:?}", value));
        }
    };

    vm.current_exception = Some(exc);
    Err("Exception thrown".to_string())
}

pub fn handle_bind_catch_var(vm: &mut VM, bytecode: &[u8], ip: &mut usize) -> Result<(), String> {
    let _catch_var_idx = vm.read_u16(bytecode, ip);

    if let Some(exc) = &vm.current_exception {
        let exc_value = Value::Exception(exc.clone());
        // Store the exception in the scope stack or variable storage
        // For now, we'll push it on the stack to be captured by the catch scope
        vm.stack.push(exc_value);
    }

    Ok(())
}

pub fn handle_clear_exception(vm: &mut VM) {
    vm.current_exception = None;
}

pub fn handle_jump_on_exception(vm: &mut VM, bytecode: &[u8], ip: &mut usize) {
    let offset = vm.read_i16(bytecode, ip);

    if vm.current_exception.is_some() {
        let new_ip = (*ip as i32) + (offset as i32);
        *ip = new_ip as usize;
    }
}
