//! Meta-programming primitives (gensym)
use crate::value::fiber::{SignalBits, SIG_ERROR, SIG_OK};
use crate::value::{error_val, Value};
use std::sync::atomic::{AtomicU32, Ordering};

static GENSYM_COUNTER: AtomicU32 = AtomicU32::new(0);

/// Generate a unique symbol.
///
/// Returns a symbol value (not a string). The symbol is interned in the
/// current symbol table so it can be used in quasiquote templates:
///
/// ```lisp
/// (defmacro with-temp (body)
///   (let ((tmp (gensym "tmp")))
///     `(let ((,tmp 42)) ,body)))
/// ```
pub fn prim_gensym(args: &[Value]) -> (SignalBits, Value) {
    let prefix = if args.is_empty() {
        "G".to_string()
    } else if let Some(s) = args[0].as_string() {
        s.to_string()
    } else if let Some(id) = args[0].as_symbol() {
        format!("G{}", id)
    } else {
        "G".to_string()
    };

    let counter = GENSYM_COUNTER.fetch_add(1, Ordering::SeqCst);
    let sym_name = format!("{}{}", prefix, counter);

    // Intern the symbol name so we return a proper symbol value.
    // This requires the symbol table to be set via set_symbol_table().
    unsafe {
        if let Some(symbols_ptr) = crate::ffi::primitives::context::get_symbol_table() {
            let id = (*symbols_ptr).intern(&sym_name);
            (SIG_OK, Value::symbol(id.0))
        } else {
            (
                SIG_ERROR,
                error_val("error", "gensym: symbol table not available"),
            )
        }
    }
}
