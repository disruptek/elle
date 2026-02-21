use super::core::VM;
use crate::value::Value;

pub fn handle_load_const(vm: &mut VM, bytecode: &[u8], ip: &mut usize, constants: &[Value]) {
    let idx = vm.read_u16(bytecode, ip) as usize;
    vm.fiber.stack.push(constants[idx]);
}

pub fn handle_load_local(vm: &mut VM, bytecode: &[u8], ip: &mut usize) -> Result<(), String> {
    let _depth = vm.read_u8(bytecode, ip); // depth (currently unused)
    let idx = vm.read_u8(bytecode, ip) as usize;
    let frame_base = vm.current_frame_base();
    let abs_idx = frame_base + idx;
    if abs_idx >= vm.fiber.stack.len() {
        return Err(format!(
            "Local variable index out of bounds: {} (frame_base={}, idx={}, stack_len={})",
            abs_idx,
            frame_base,
            idx,
            vm.fiber.stack.len()
        ));
    }
    let val = vm.fiber.stack[abs_idx];
    vm.fiber.stack.push(val);
    Ok(())
}

pub fn handle_pop(vm: &mut VM) -> Result<(), String> {
    vm.fiber.stack.pop().ok_or("Stack underflow")?;
    Ok(())
}

pub fn handle_dup(vm: &mut VM) -> Result<(), String> {
    let val = *vm.fiber.stack.last().ok_or("Stack underflow")?;
    vm.fiber.stack.push(val);
    Ok(())
}

pub fn handle_dup_n(vm: &mut VM, bytecode: &[u8], ip: &mut usize) -> Result<(), String> {
    let offset = vm.read_u8(bytecode, ip) as usize;
    let stack_len = vm.fiber.stack.len();
    if offset >= stack_len {
        return Err(format!(
            "DupN offset {} out of bounds (stack size {})",
            offset, stack_len
        ));
    }
    let idx = stack_len - 1 - offset;
    let val = vm.fiber.stack[idx];
    vm.fiber.stack.push(val);
    Ok(())
}
