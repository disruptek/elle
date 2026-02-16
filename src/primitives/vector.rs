//! Vector operations primitives
use crate::error::{LError, LResult};
use crate::value::Value;

/// Create a vector from arguments
pub fn prim_vector(args: &[Value]) -> LResult<Value> {
    Ok(Value::vector(args.to_vec()))
}

/// Get the length of a vector
pub fn prim_vector_length(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    if let Some(v) = args[0].as_vector() {
        let borrowed = v.borrow();
        Ok(Value::int(borrowed.len() as i64))
    } else {
        Err(LError::type_mismatch("vector", args[0].type_name()))
    }
}

/// Get a reference from a vector at an index
pub fn prim_vector_ref(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let vec = args[0]
        .as_vector()
        .ok_or_else(|| LError::type_mismatch("vector", args[0].type_name()))?;
    let index = args[1]
        .as_int()
        .ok_or_else(|| LError::type_mismatch("integer", args[1].type_name()))?
        as usize;

    let borrowed = vec.borrow();
    borrowed
        .get(index)
        .cloned()
        .ok_or_else(|| LError::index_out_of_bounds(index as isize, borrowed.len()))
}

/// Set a value in a vector at an index (returns new vector)
pub fn prim_vector_set(args: &[Value]) -> LResult<Value> {
    if args.len() != 3 {
        return Err(LError::arity_mismatch(3, args.len()));
    }

    let vec_ref = args[0]
        .as_vector()
        .ok_or_else(|| LError::type_mismatch("vector", args[0].type_name()))?;
    let index = args[1]
        .as_int()
        .ok_or_else(|| LError::type_mismatch("integer", args[1].type_name()))?
        as usize;
    let value = args[2];

    let mut vec = vec_ref.borrow_mut();
    if index >= vec.len() {
        return Err(LError::index_out_of_bounds(index as isize, vec.len()));
    }

    vec[index] = value;
    drop(vec);
    Ok(Value::vector(vec_ref.borrow().clone()))
}
