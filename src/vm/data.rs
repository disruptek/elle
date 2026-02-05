use super::core::VM;
use crate::value::{cons, Value};
use std::rc::Rc;

pub fn handle_cons(vm: &mut VM) -> Result<(), String> {
    let rest = vm.stack.pop().ok_or("Stack underflow")?;
    let first = vm.stack.pop().ok_or("Stack underflow")?;
    vm.stack.push(cons(first, rest));
    Ok(())
}

pub fn handle_car(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    let cons = val.as_cons()?;
    vm.stack.push(cons.first.clone());
    Ok(())
}

pub fn handle_cdr(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    let cons = val.as_cons()?;
    vm.stack.push(cons.rest.clone());
    Ok(())
}

pub fn handle_make_vector(vm: &mut VM, bytecode: &[u8], ip: &mut usize) -> Result<(), String> {
    let size = vm.read_u8(bytecode, ip) as usize;
    let mut vec = Vec::with_capacity(size);
    for _ in 0..size {
        vec.push(vm.stack.pop().ok_or("Stack underflow")?);
    }
    vec.reverse();
    vm.stack.push(Value::Vector(Rc::new(vec)));
    Ok(())
}

pub fn handle_vector_ref(vm: &mut VM) -> Result<(), String> {
    let idx = vm.stack.pop().ok_or("Stack underflow")?;
    let vec = vm.stack.pop().ok_or("Stack underflow")?;
    let idx = idx.as_int()? as usize;
    let vec = vec.as_vector()?;
    vm.stack
        .push(vec.get(idx).ok_or("Vector index out of bounds")?.clone());
    Ok(())
}

pub fn handle_vector_set(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.pop().ok_or("Stack underflow")?;
    let idx = vm.stack.pop().ok_or("Stack underflow")?;
    let _vec = vm.stack.pop().ok_or("Stack underflow")?;
    let _idx = idx.as_int()? as usize;
    // Note: Vectors are immutable in this implementation
    vm.stack.push(val);
    Ok(())
}
