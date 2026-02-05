use super::core::VM;
use crate::value::Value;

pub fn handle_load_const(vm: &mut VM, bytecode: &[u8], ip: &mut usize, constants: &[Value]) {
    let idx = vm.read_u16(bytecode, ip) as usize;
    vm.stack.push(constants[idx].clone());
}

pub fn handle_load_local(vm: &mut VM, bytecode: &[u8], ip: &mut usize) -> Result<(), String> {
    let idx = vm.read_u8(bytecode, ip) as usize;
    if idx >= vm.stack.len() {
        return Err("Local variable index out of bounds".to_string());
    }
    let val = vm.stack[idx].clone();
    vm.stack.push(val);
    Ok(())
}

pub fn handle_pop(vm: &mut VM) -> Result<(), String> {
    vm.stack.pop().ok_or("Stack underflow")?;
    Ok(())
}

pub fn handle_dup(vm: &mut VM) -> Result<(), String> {
    let val = vm.stack.last().ok_or("Stack underflow")?.clone();
    vm.stack.push(val);
    Ok(())
}
