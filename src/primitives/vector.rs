//! Vector operations primitives
use crate::value::Value;
use std::rc::Rc;

/// Create a vector from arguments
pub fn prim_vector(args: &[Value]) -> Result<Value, String> {
    Ok(Value::Vector(Rc::new(args.to_vec())))
}

/// Get the length of a vector
pub fn prim_vector_length(args: &[Value]) -> Result<Value, String> {
    if args.len() != 1 {
        return Err("vector-length requires exactly 1 argument".to_string());
    }

    match &args[0] {
        Value::Vector(v) => Ok(Value::Int(v.len() as i64)),
        _ => Err("vector-length requires a vector".to_string()),
    }
}

/// Get a reference from a vector at an index
pub fn prim_vector_ref(args: &[Value]) -> Result<Value, String> {
    if args.len() != 2 {
        return Err("vector-ref requires exactly 2 arguments (vector, index)".to_string());
    }

    let vec = args[0].as_vector()?;
    let index = args[1].as_int()? as usize;

    vec.get(index)
        .cloned()
        .ok_or("Vector index out of bounds".to_string())
}

/// Set a value in a vector at an index (returns new vector)
pub fn prim_vector_set(args: &[Value]) -> Result<Value, String> {
    if args.len() != 3 {
        return Err("vector-set! requires exactly 3 arguments (vector, index, value)".to_string());
    }

    let mut vec = args[0].as_vector()?.as_ref().clone();
    let index = args[1].as_int()? as usize;
    let value = args[2].clone();

    if index >= vec.len() {
        return Err("Vector index out of bounds".to_string());
    }

    vec[index] = value;
    Ok(Value::Vector(Rc::new(vec)))
}
