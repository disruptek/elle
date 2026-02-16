//! File I/O primitives
use crate::error::{LError, LResult};
use crate::value::Value;

/// Read entire file as a string
pub fn prim_slurp(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::read_to_string(path)
            .map(Value::string)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Write string content to a file (overwrites if exists)
pub fn prim_spit(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let path = if let Some(s) = args[0].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[0].type_name()));
    };

    let content = if let Some(s) = args[1].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[1].type_name()));
    };

    std::fs::write(path, content)
        .map(|_| Value::TRUE)
        .map_err(|e| LError::file_read_error(path, e.to_string()))
}

/// Append string content to a file
pub fn prim_append_file(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let path = if let Some(s) = args[0].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[0].type_name()));
    };

    let content = if let Some(s) = args[1].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[1].type_name()));
    };

    use std::fs::OpenOptions;
    use std::io::Write;

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(path)
        .map_err(|e| LError::file_read_error(path, e.to_string()))?;

    file.write_all(content.as_bytes())
        .map(|_| Value::TRUE)
        .map_err(|e| LError::file_read_error(path, e.to_string()))
}

/// Check if a file exists
pub fn prim_file_exists(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        Ok(Value::bool(std::path::Path::new(path).exists()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Check if path is a directory
pub fn prim_is_directory(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        match std::fs::metadata(path) {
            Ok(metadata) => Ok(Value::bool(metadata.is_dir())),
            Err(_) => Ok(Value::FALSE),
        }
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Check if path is a file
pub fn prim_is_file(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        match std::fs::metadata(path) {
            Ok(metadata) => Ok(Value::bool(metadata.is_file())),
            Err(_) => Ok(Value::FALSE),
        }
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Delete a file
pub fn prim_delete_file(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::remove_file(path)
            .map(|_| Value::TRUE)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Delete a directory (must be empty)
pub fn prim_delete_directory(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::remove_dir(path)
            .map(|_| Value::TRUE)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Create a directory
pub fn prim_create_directory(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::create_dir(path)
            .map(|_| Value::TRUE)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Create a directory and all parent directories
pub fn prim_create_directory_all(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::create_dir_all(path)
            .map(|_| Value::TRUE)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Rename a file
pub fn prim_rename_file(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let old_path = if let Some(s) = args[0].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[0].type_name()));
    };

    let new_path = if let Some(s) = args[1].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[1].type_name()));
    };

    std::fs::rename(old_path, new_path)
        .map(|_| Value::TRUE)
        .map_err(|e| LError::file_read_error(old_path, e.to_string()))
}

/// Copy a file
pub fn prim_copy_file(args: &[Value]) -> LResult<Value> {
    if args.len() != 2 {
        return Err(LError::arity_mismatch(2, args.len()));
    }

    let src = if let Some(s) = args[0].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[0].type_name()));
    };

    let dst = if let Some(s) = args[1].as_string() {
        s
    } else {
        return Err(LError::type_mismatch("string", args[1].type_name()));
    };

    std::fs::copy(src, dst)
        .map(|_| Value::TRUE)
        .map_err(|e| LError::file_read_error(src, e.to_string()))
}

/// Get file size in bytes
pub fn prim_file_size(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::metadata(path)
            .map(|metadata| Value::int(metadata.len() as i64))
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// List directory contents
pub fn prim_list_directory(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::read_dir(path)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
            .and_then(|entries| {
                let mut items = Vec::new();
                for entry in entries {
                    match entry {
                        Ok(entry) => {
                            if let Ok(name) = entry.file_name().into_string() {
                                items.push(Value::string(name));
                            }
                        }
                        Err(e) => return Err(LError::file_read_error(path, e.to_string())),
                    }
                }
                Ok(crate::value::list(items))
            })
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Get absolute path
pub fn prim_absolute_path(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::canonicalize(path)
            .map(|abs_path| Value::string(abs_path.to_string_lossy().into_owned()))
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Get current working directory
pub fn prim_current_directory(_args: &[Value]) -> LResult<Value> {
    std::env::current_dir()
        .map(|path| Value::string(path.to_string_lossy().into_owned()))
        .map_err(|e| LError::generic(format!("Failed to get current directory: {}", e)))
}

/// Change current working directory
pub fn prim_change_directory(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::env::set_current_dir(path)
            .map(|_| Value::TRUE)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Join path components (return a properly formatted path)
pub fn prim_join_path(args: &[Value]) -> LResult<Value> {
    if args.is_empty() {
        return Err(LError::arity_at_least(1, args.len()));
    }

    let mut path = std::path::PathBuf::new();
    for arg in args {
        if let Some(s) = arg.as_string() {
            path.push(s);
        } else {
            return Err(LError::type_mismatch("string", arg.type_name()));
        }
    }

    Ok(Value::string(path.to_string_lossy().into_owned()))
}

/// Get file extension
pub fn prim_file_extension(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path_str) = args[0].as_string() {
        let path = std::path::Path::new(path_str);
        match path.extension() {
            Some(ext) => Ok(Value::string(ext.to_string_lossy().into_owned())),
            None => Ok(Value::NIL),
        }
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Get file name (without directory)
pub fn prim_file_name(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path_str) = args[0].as_string() {
        let path = std::path::Path::new(path_str);
        match path.file_name() {
            Some(name) => Ok(Value::string(name.to_string_lossy().into_owned())),
            None => Ok(Value::NIL),
        }
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Get parent directory path
pub fn prim_parent_directory(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path_str) = args[0].as_string() {
        let path = std::path::Path::new(path_str);
        match path.parent() {
            Some(parent) => Ok(Value::string(parent.to_string_lossy().into_owned())),
            None => Ok(Value::NIL),
        }
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}

/// Read lines from a file and return as a list of strings
pub fn prim_read_lines(args: &[Value]) -> LResult<Value> {
    if args.len() != 1 {
        return Err(LError::arity_mismatch(1, args.len()));
    }
    if let Some(path) = args[0].as_string() {
        std::fs::read_to_string(path)
            .map_err(|e| LError::file_read_error(path, e.to_string()))
            .map(|content| {
                let lines: Vec<Value> = content
                    .lines()
                    .map(|line| Value::string(line.to_string()))
                    .collect();
                crate::value::list(lines)
            })
    } else {
        Err(LError::type_mismatch("string", args[0].type_name()))
    }
}
