//! Table operations primitives (mutable hash tables)
use crate::error::{LError, LResult};
use crate::value::{TableKey, Value};
use std::collections::BTreeMap;

/// Create a mutable table from key-value pairs
/// (table key1 val1 key2 val2 ...)
pub fn prim_table(args: &[Value]) -> LResult<Value> {
    if !args.len().is_multiple_of(2) {
        return Err(LError::argument_error(
            "table requires an even number of arguments (key-value pairs)",
        ));
    }

    let mut map = BTreeMap::new();
    for i in (0..args.len()).step_by(2) {
        let key = value_to_table_key(&args[i])?;
        let value = args[i + 1];
        map.insert(key, value);
    }

    Ok(Value::table_from(map))
}

/// Convert a Value to a TableKey
fn value_to_table_key(val: &Value) -> LResult<TableKey> {
    if val.is_nil() {
        Ok(TableKey::Nil)
    } else if let Some(b) = val.as_bool() {
        Ok(TableKey::Bool(b))
    } else if let Some(i) = val.as_int() {
        Ok(TableKey::Int(i))
    } else if let Some(id) = val.as_symbol() {
        Ok(TableKey::Symbol(crate::value::SymbolId(id)))
    } else if let Some(s) = val.as_string() {
        Ok(TableKey::String(s.to_string()))
    } else {
        Err(LError::type_mismatch(
            "table key (nil, bool, int, symbol, or string)",
            val.type_name(),
        ))
    }
}

/// Get a value from a table by key
/// `(get table key [default])`
pub fn prim_table_get(args: &[Value]) -> LResult<Value> {
    if args.len() < 2 || args.len() > 3 {
        return Err(LError::arity_range(2, 3, args.len()));
    }

    let table = args[0]
        .as_table()
        .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
    let key = value_to_table_key(&args[1])?;
    let default = if args.len() == 3 { args[2] } else { Value::NIL };

    let borrowed = table.borrow();
    Ok(borrowed.get(&key).copied().unwrap_or(default))
}

/// Put a key-value pair into a table (mutable, in-place)
/// (put table key value)
pub fn prim_table_put(args: &[Value]) -> LResult<Value> {
    if args.len() != 3 {
        return Err(LError::arity_mismatch(3, args.len()));
    }

    let table = args[0]
        .as_table()
        .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
    let key = value_to_table_key(&args[1])?;
    let value = args[2];

    table.borrow_mut().insert(key, value);
    Ok(args[0]) // Return the table itself
}

/// Delete a key from a table (mutable, in-place)
/// (del table key)
pub fn prim_table_del(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let table = args[0]
        .as_table()
        .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
    let key = value_to_table_key(&args[1])?;

    table.borrow_mut().remove(&key);
    Ok(args[0]) // Return the table itself
}

/// Polymorphic del - works on both tables and structs
/// For tables: mutates in-place and returns the table
/// For structs: returns a new struct without the field (immutable)
/// `(del collection key)`
pub fn prim_del(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let key = value_to_table_key(&args[1])?;

    if args[0].is_table() {
        let table = args[0]
            .as_table()
            .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
        table.borrow_mut().remove(&key);
        Ok(args[0]) // Return the mutated table
    } else if args[0].is_struct() {
        let s = args[0]
            .as_struct()
            .ok_or_else(|| LError::type_mismatch("struct", args[0].type_name()))?;
        let mut new_map = s.clone();
        new_map.remove(&key);
        Ok(Value::struct_from(new_map)) // Return new struct
    } else {
        Err(LError::type_mismatch(
            "table or struct",
            args[0].type_name(),
        ))
    }
}

/// Get all keys from a table as a list
/// (keys table)
pub fn prim_table_keys(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    let table = args[0]
        .as_table()
        .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
    let borrowed = table.borrow();

    let keys: Vec<Value> = borrowed
        .keys()
        .map(|k| match k {
            TableKey::Nil => Value::NIL,
            TableKey::Bool(b) => Value::bool(*b),
            TableKey::Int(i) => Value::int(*i),
            TableKey::Symbol(sid) => Value::symbol(sid.0),
            TableKey::String(s) => Value::string(s.as_str()),
        })
        .collect();

    Ok(crate::value::list(keys))
}

/// Get all values from a table as a list
/// (values table)
pub fn prim_table_values(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    let table = args[0]
        .as_table()
        .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
    let borrowed = table.borrow();

    let values: Vec<Value> = borrowed.values().copied().collect();
    Ok(crate::value::list(values))
}

/// Check if a table has a key
/// (has-key? table key)
pub fn prim_table_has(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let table = args[0]
        .as_table()
        .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
    let key = value_to_table_key(&args[1])?;

    Ok(Value::bool(table.borrow().contains_key(&key)))
}

/// Get the number of entries in a table
/// (length table)
pub fn prim_table_length(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    let table = args[0]
        .as_table()
        .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
    Ok(Value::int(table.borrow().len() as i64))
}

// ============ POLYMORPHIC FUNCTIONS (work on both tables and structs) ============

/// Polymorphic get - works on both tables and structs
/// `(get collection key [default])`
pub fn prim_get(args: &[Value]) -> LResult<Value> {
    if args.len() < 2 || args.len() > 3 {
        return Err(LError::arity_range(2, 3, args.len()));
    }

    let default = if args.len() == 3 { args[2] } else { Value::NIL };

    if args[0].is_table() {
        let table = args[0]
            .as_table()
            .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
        let key = value_to_table_key(&args[1])?;
        let borrowed = table.borrow();
        Ok(borrowed.get(&key).copied().unwrap_or(default))
    } else if args[0].is_struct() {
        let s = args[0]
            .as_struct()
            .ok_or_else(|| LError::type_mismatch("struct", args[0].type_name()))?;
        let key = value_to_table_key(&args[1])?;
        Ok(s.get(&key).copied().unwrap_or(default))
    } else {
        Err(LError::type_mismatch(
            "table or struct",
            args[0].type_name(),
        ))
    }
}

/// Polymorphic keys - works on both tables and structs
/// `(keys collection)`
pub fn prim_keys(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    if args[0].is_table() {
        let table = args[0]
            .as_table()
            .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
        let borrowed = table.borrow();
        let keys: Vec<Value> = borrowed
            .keys()
            .map(|k| match k {
                TableKey::Nil => Value::NIL,
                TableKey::Bool(b) => Value::bool(*b),
                TableKey::Int(i) => Value::int(*i),
                TableKey::Symbol(sid) => Value::symbol(sid.0),
                TableKey::String(s) => Value::string(s.as_str()),
            })
            .collect();
        Ok(crate::value::list(keys))
    } else if args[0].is_struct() {
        let s = args[0]
            .as_struct()
            .ok_or_else(|| LError::type_mismatch("struct", args[0].type_name()))?;
        let keys: Vec<Value> = s
            .keys()
            .map(|k| match k {
                TableKey::Nil => Value::NIL,
                TableKey::Bool(b) => Value::bool(*b),
                TableKey::Int(i) => Value::int(*i),
                TableKey::Symbol(sid) => Value::symbol(sid.0),
                TableKey::String(st) => Value::string(st.as_str()),
            })
            .collect();
        Ok(crate::value::list(keys))
    } else {
        Err(LError::type_mismatch(
            "table or struct",
            args[0].type_name(),
        ))
    }
}

/// Polymorphic values - works on both tables and structs
/// `(values collection)`
pub fn prim_values(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }

    if args[0].is_table() {
        let table = args[0]
            .as_table()
            .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
        let borrowed = table.borrow();
        let values: Vec<Value> = borrowed.values().copied().collect();
        Ok(crate::value::list(values))
    } else if args[0].is_struct() {
        let s = args[0]
            .as_struct()
            .ok_or_else(|| LError::type_mismatch("struct", args[0].type_name()))?;
        let values: Vec<Value> = s.values().copied().collect();
        Ok(crate::value::list(values))
    } else {
        Err(LError::type_mismatch(
            "table or struct",
            args[0].type_name(),
        ))
    }
}

/// Polymorphic has-key? - works on both tables and structs
/// `(has-key? collection key)`
pub fn prim_has_key(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let key = value_to_table_key(&args[1])?;

    if args[0].is_table() {
        let table = args[0]
            .as_table()
            .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
        Ok(Value::bool(table.borrow().contains_key(&key)))
    } else if args[0].is_struct() {
        let s = args[0]
            .as_struct()
            .ok_or_else(|| LError::type_mismatch("struct", args[0].type_name()))?;
        Ok(Value::bool(s.contains_key(&key)))
    } else {
        Err(LError::type_mismatch(
            "table or struct",
            args[0].type_name(),
        ))
    }
}

/// Polymorphic put - works on both tables and structs
/// For tables: mutates in-place and returns the table
/// For structs: returns a new struct with the updated field (immutable)
/// `(put collection key value)`
pub fn prim_put(args: &[Value]) -> LResult<Value> {
    if args.len() != 3 {
        return Err(LError::arity_mismatch(3, args.len()));
    }

    let key = value_to_table_key(&args[1])?;
    let value = args[2];

    if args[0].is_table() {
        let table = args[0]
            .as_table()
            .ok_or_else(|| LError::type_mismatch("table", args[0].type_name()))?;
        table.borrow_mut().insert(key, value);
        Ok(args[0]) // Return the mutated table
    } else if args[0].is_struct() {
        let s = args[0]
            .as_struct()
            .ok_or_else(|| LError::type_mismatch("struct", args[0].type_name()))?;
        let mut new_map = s.clone();
        new_map.insert(key, value);
        Ok(Value::struct_from(new_map)) // Return new struct
    } else {
        Err(LError::type_mismatch(
            "table or struct",
            args[0].type_name(),
        ))
    }
}
