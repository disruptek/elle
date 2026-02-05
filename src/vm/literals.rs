use super::core::VM;
use crate::value::Value;

pub fn handle_nil(vm: &mut VM) {
    vm.stack.push(Value::Nil);
}

pub fn handle_true(vm: &mut VM) {
    vm.stack.push(Value::Bool(true));
}

pub fn handle_false(vm: &mut VM) {
    vm.stack.push(Value::Bool(false));
}
