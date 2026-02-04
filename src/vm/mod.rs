use crate::compiler::bytecode::{Bytecode, Instruction};
use crate::ffi::FFISubsystem;
use crate::value::{cons, Value};
use smallvec::SmallVec;
use std::collections::HashMap;
use std::rc::Rc;

type StackVec = SmallVec<[Value; 256]>;

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub name: String,
    pub ip: usize,
}

pub struct VM {
    stack: StackVec,
    globals: HashMap<u32, Value>,
    call_depth: usize,
    call_stack: Vec<CallFrame>,
    ffi: FFISubsystem,
}

impl VM {
    pub fn new() -> Self {
        VM {
            stack: SmallVec::new(),
            globals: HashMap::new(),
            call_depth: 0,
            call_stack: Vec::new(),
            ffi: FFISubsystem::new(),
        }
    }

    pub fn set_global(&mut self, sym_id: u32, value: Value) {
        self.globals.insert(sym_id, value);
    }

    pub fn get_global(&self, sym_id: u32) -> Option<&Value> {
        self.globals.get(&sym_id)
    }

    /// Get the FFI subsystem.
    pub fn ffi(&self) -> &FFISubsystem {
        &self.ffi
    }

    /// Get a mutable reference to the FFI subsystem.
    pub fn ffi_mut(&mut self) -> &mut FFISubsystem {
        &mut self.ffi
    }

    pub fn format_stack_trace(&self) -> String {
        if self.call_stack.is_empty() {
            "  (no call stack)".to_string()
        } else {
            let mut trace = String::new();
            for (i, frame) in self.call_stack.iter().rev().enumerate() {
                trace.push_str(&format!("  #{}: {} (ip={})\n", i, frame.name, frame.ip));
            }
            trace
        }
    }

    fn with_stack_trace(&self, msg: String) -> String {
        let trace = self.format_stack_trace();
        format!("{}\nStack trace:\n{}", msg, trace)
    }

    #[inline(always)]
    fn read_u8(&self, bytecode: &[u8], ip: &mut usize) -> u8 {
        let val = bytecode[*ip];
        *ip += 1;
        val
    }

    #[inline(always)]
    fn read_u16(&self, bytecode: &[u8], ip: &mut usize) -> u16 {
        let high = bytecode[*ip] as u16;
        let low = bytecode[*ip + 1] as u16;
        *ip += 2;
        (high << 8) | low
    }

    #[inline(always)]
    fn read_i16(&self, bytecode: &[u8], ip: &mut usize) -> i16 {
        self.read_u16(bytecode, ip) as i16
    }

    pub fn execute(&mut self, bytecode: &Bytecode) -> Result<Value, String> {
        self.execute_bytecode(&bytecode.instructions, &bytecode.constants)
    }

    fn execute_bytecode(&mut self, bytecode: &[u8], constants: &[Value]) -> Result<Value, String> {
        let mut ip = 0;

        loop {
            if ip >= bytecode.len() {
                return Err("Unexpected end of bytecode".to_string());
            }

            let instr_byte = bytecode[ip];
            ip += 1;

            let instr: Instruction = unsafe { std::mem::transmute(instr_byte) };

            match instr {
                Instruction::LoadConst => {
                    let idx = self.read_u16(bytecode, &mut ip) as usize;
                    self.stack.push(constants[idx].clone());
                }

                Instruction::LoadLocal => {
                    let idx = self.read_u8(bytecode, &mut ip) as usize;
                    if idx >= self.stack.len() {
                        return Err("Local variable index out of bounds".to_string());
                    }
                    let val = self.stack[idx].clone();
                    self.stack.push(val);
                }

                Instruction::LoadGlobal => {
                    let idx = self.read_u16(bytecode, &mut ip) as usize;
                    if let Value::Symbol(sym_id) = constants[idx] {
                        if let Some(val) = self.globals.get(&sym_id.0) {
                            self.stack.push(val.clone());
                        } else {
                            return Err(format!("Undefined global variable: {:?}", sym_id));
                        }
                    } else {
                        return Err("LoadGlobal expects symbol constant".to_string());
                    }
                }

                Instruction::StoreLocal => {
                    let idx = self.read_u8(bytecode, &mut ip) as usize;
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    if idx >= self.stack.len() {
                        return Err("Local variable index out of bounds".to_string());
                    }
                    self.stack[idx] = val;
                }

                Instruction::StoreGlobal => {
                    let idx = self.read_u16(bytecode, &mut ip) as usize;
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    if let Value::Symbol(sym_id) = constants[idx] {
                        self.globals.insert(sym_id.0, val.clone());
                        self.stack.push(val);
                    } else {
                        return Err("StoreGlobal expects symbol constant".to_string());
                    }
                }

                Instruction::LoadUpvalue => {
                    let _depth = self.read_u8(bytecode, &mut ip);
                    let _idx = self.read_u8(bytecode, &mut ip);
                    // TODO: Implement proper upvalue handling
                    return Err("Upvalues not yet implemented".to_string());
                }

                Instruction::Pop => {
                    self.stack.pop().ok_or("Stack underflow")?;
                }

                Instruction::Dup => {
                    let val = self.stack.last().ok_or("Stack underflow")?.clone();
                    self.stack.push(val);
                }

                Instruction::Call => {
                    let arg_count = self.read_u8(bytecode, &mut ip) as usize;
                    let func = self.stack.pop().ok_or("Stack underflow")?;

                    // Collect arguments
                    let mut args = Vec::with_capacity(arg_count);
                    for _ in 0..arg_count {
                        args.push(self.stack.pop().ok_or("Stack underflow")?);
                    }
                    args.reverse();

                    let result = match func {
                        Value::NativeFn(f) => f(&args)?,
                        Value::Closure(closure) => {
                            self.call_depth += 1;
                            if self.call_depth > 1000 {
                                return Err("Stack overflow".to_string());
                            }

                            // Execute closure bytecode
                            let result = self.execute_bytecode(&closure.bytecode, constants)?;

                            self.call_depth -= 1;
                            result
                        }
                        _ => return Err(format!("Cannot call {:?}", func)),
                    };

                    self.stack.push(result);
                }

                Instruction::TailCall => {
                    // For now, just do a regular call
                    // TODO: Implement proper tail call optimization
                    let arg_count = self.read_u8(bytecode, &mut ip) as usize;
                    let func = self.stack.pop().ok_or("Stack underflow")?;

                    let mut args = Vec::with_capacity(arg_count);
                    for _ in 0..arg_count {
                        args.push(self.stack.pop().ok_or("Stack underflow")?);
                    }
                    args.reverse();

                    let result = match func {
                        Value::NativeFn(f) => f(&args)?,
                        Value::Closure(closure) => {
                            self.execute_bytecode(&closure.bytecode, constants)?
                        }
                        _ => return Err(format!("Cannot call {:?}", func)),
                    };

                    self.stack.push(result);
                }

                Instruction::Return => {
                    return self
                        .stack
                        .pop()
                        .ok_or_else(|| "Stack underflow on return".to_string());
                }

                Instruction::Jump => {
                    let offset = self.read_i16(bytecode, &mut ip);
                    ip = ((ip as i32) + (offset as i32)) as usize;
                }

                Instruction::JumpIfFalse => {
                    let offset = self.read_i16(bytecode, &mut ip);
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    if !val.is_truthy() {
                        ip = ((ip as i32) + (offset as i32)) as usize;
                    }
                }

                Instruction::MakeClosure => {
                    let _idx = self.read_u16(bytecode, &mut ip);
                    let _num_upvalues = self.read_u8(bytecode, &mut ip);
                    // TODO: Implement proper closure creation
                    return Err("Closures not yet fully implemented".to_string());
                }

                Instruction::Cons => {
                    let rest = self.stack.pop().ok_or("Stack underflow")?;
                    let first = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(cons(first, rest));
                }

                Instruction::Car => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    let cons = val.as_cons()?;
                    self.stack.push(cons.first.clone());
                }

                Instruction::Cdr => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    let cons = val.as_cons()?;
                    self.stack.push(cons.rest.clone());
                }

                Instruction::MakeVector => {
                    let size = self.read_u8(bytecode, &mut ip) as usize;
                    let mut vec = Vec::with_capacity(size);
                    for _ in 0..size {
                        vec.push(self.stack.pop().ok_or("Stack underflow")?);
                    }
                    vec.reverse();
                    self.stack.push(Value::Vector(Rc::new(vec)));
                }

                Instruction::VectorRef => {
                    let idx = self.stack.pop().ok_or("Stack underflow")?;
                    let vec = self.stack.pop().ok_or("Stack underflow")?;
                    let idx = idx.as_int()? as usize;
                    let vec = vec.as_vector()?;
                    self.stack
                        .push(vec.get(idx).ok_or("Vector index out of bounds")?.clone());
                }

                Instruction::VectorSet => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    let idx = self.stack.pop().ok_or("Stack underflow")?;
                    let _vec = self.stack.pop().ok_or("Stack underflow")?;
                    let _idx = idx.as_int()? as usize;
                    // Note: Vectors are immutable in this implementation
                    self.stack.push(val);
                }

                Instruction::AddInt => {
                    let b = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    let a = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    self.stack.push(Value::Int(a + b));
                }

                Instruction::SubInt => {
                    let b = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    let a = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    self.stack.push(Value::Int(a - b));
                }

                Instruction::MulInt => {
                    let b = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    let a = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    self.stack.push(Value::Int(a * b));
                }

                Instruction::DivInt => {
                    let b = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    let a = self.stack.pop().ok_or("Stack underflow")?.as_int()?;
                    if b == 0 {
                        return Err("Division by zero".to_string());
                    }
                    self.stack.push(Value::Int(a / b));
                }

                Instruction::Add => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x + y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x + y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 + y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x + y as f64),
                        _ => return Err("Type error in addition".to_string()),
                    };
                    self.stack.push(result);
                }

                Instruction::Sub => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x - y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x - y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 - y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x - y as f64),
                        _ => return Err("Type error in subtraction".to_string()),
                    };
                    self.stack.push(result);
                }

                Instruction::Mul => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Int(x * y),
                        (Value::Float(x), Value::Float(y)) => Value::Float(x * y),
                        (Value::Int(x), Value::Float(y)) => Value::Float(x as f64 * y),
                        (Value::Float(x), Value::Int(y)) => Value::Float(x * y as f64),
                        _ => return Err("Type error in multiplication".to_string()),
                    };
                    self.stack.push(result);
                }

                Instruction::Div => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
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
                    self.stack.push(result);
                }

                Instruction::Eq => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(a == b));
                }

                Instruction::Lt => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Bool(x < y),
                        (Value::Float(x), Value::Float(y)) => Value::Bool(x < y),
                        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) < y),
                        (Value::Float(x), Value::Int(y)) => Value::Bool(x < (y as f64)),
                        _ => return Err("Type error in comparison".to_string()),
                    };
                    self.stack.push(result);
                }

                Instruction::Gt => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Bool(x > y),
                        (Value::Float(x), Value::Float(y)) => Value::Bool(x > y),
                        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) > y),
                        (Value::Float(x), Value::Int(y)) => Value::Bool(x > (y as f64)),
                        _ => return Err("Type error in comparison".to_string()),
                    };
                    self.stack.push(result);
                }

                Instruction::Le => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Bool(x <= y),
                        (Value::Float(x), Value::Float(y)) => Value::Bool(x <= y),
                        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) <= y),
                        (Value::Float(x), Value::Int(y)) => Value::Bool(x <= (y as f64)),
                        _ => return Err("Type error in comparison".to_string()),
                    };
                    self.stack.push(result);
                }

                Instruction::Ge => {
                    let b = self.stack.pop().ok_or("Stack underflow")?;
                    let a = self.stack.pop().ok_or("Stack underflow")?;
                    let result = match (a, b) {
                        (Value::Int(x), Value::Int(y)) => Value::Bool(x >= y),
                        (Value::Float(x), Value::Float(y)) => Value::Bool(x >= y),
                        (Value::Int(x), Value::Float(y)) => Value::Bool((x as f64) >= y),
                        (Value::Float(x), Value::Int(y)) => Value::Bool(x >= (y as f64)),
                        _ => return Err("Type error in comparison".to_string()),
                    };
                    self.stack.push(result);
                }

                Instruction::IsNil => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(val.is_nil()));
                }

                Instruction::IsPair => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(matches!(val, Value::Cons(_))));
                }

                Instruction::IsNumber => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack
                        .push(Value::Bool(matches!(val, Value::Int(_) | Value::Float(_))));
                }

                Instruction::IsSymbol => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack
                        .push(Value::Bool(matches!(val, Value::Symbol(_))));
                }

                Instruction::Not => {
                    let val = self.stack.pop().ok_or("Stack underflow")?;
                    self.stack.push(Value::Bool(!val.is_truthy()));
                }

                Instruction::Nil => {
                    self.stack.push(Value::Nil);
                }

                Instruction::True => {
                    self.stack.push(Value::Bool(true));
                }

                Instruction::False => {
                    self.stack.push(Value::Bool(false));
                }
            }
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
