//! Elle crypto plugin — SHA-256 and HMAC-SHA256 via the `sha2` and `hmac` crates.

use hmac::{Hmac, Mac};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;

use elle::effects::Effect;
use elle::plugin::PluginContext;
use elle::primitives::def::PrimitiveDef;
use elle::value::fiber::{SignalBits, SIG_ERROR, SIG_OK};
use elle::value::types::Arity;
use elle::value::{error_val, TableKey, Value};

/// Plugin entry point. Called by Elle when loading the `.so`.
#[no_mangle]
/// # Safety
///
/// Called by Elle's plugin loader via `dlsym`. The caller must pass a valid
/// `PluginContext` reference. Only safe when called from `load_plugin`.
pub unsafe extern "C" fn elle_plugin_init(ctx: &mut PluginContext) -> Value {
    let mut fields = BTreeMap::new();
    for def in PRIMITIVES {
        ctx.register(def);
        let short_name = def.name.strip_prefix("crypto/").unwrap_or(def.name);
        fields.insert(
            TableKey::Keyword(short_name.into()),
            Value::native_fn(def.func),
        );
    }
    Value::struct_from(fields)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Extract byte data from a string or bytes value.
/// Strings are treated as their UTF-8 encoding.
fn extract_byte_data(val: &Value, name: &str, pos: &str) -> Result<Vec<u8>, (SignalBits, Value)> {
    if let Some(bytes) = val.with_string(|s| s.as_bytes().to_vec()) {
        Ok(bytes)
    } else if let Some(b) = val.as_bytes() {
        Ok(b.to_vec())
    } else if let Some(blob_ref) = val.as_blob() {
        Ok(blob_ref.borrow().clone())
    } else {
        Err((
            SIG_ERROR,
            error_val(
                "type-error",
                format!(
                    "{}: {} must be string, bytes, or blob, got {}",
                    name,
                    pos,
                    val.type_name()
                ),
            ),
        ))
    }
}

// ---------------------------------------------------------------------------
// Primitives
// ---------------------------------------------------------------------------

/// SHA-256 hash. Accepts string, bytes, or blob. Returns bytes (32 bytes).
fn prim_sha256(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 1 {
        return (
            SIG_ERROR,
            error_val(
                "arity-error",
                format!("crypto/sha256: expected 1 argument, got {}", args.len()),
            ),
        );
    }
    let data = match extract_byte_data(&args[0], "crypto/sha256", "argument") {
        Ok(d) => d,
        Err(e) => return e,
    };
    let hash = Sha256::digest(&data);
    (SIG_OK, Value::bytes(hash.to_vec()))
}

/// HMAC-SHA256. Accepts (key, message), each string/bytes/blob. Returns bytes (32 bytes).
fn prim_hmac_sha256(args: &[Value]) -> (SignalBits, Value) {
    if args.len() != 2 {
        return (
            SIG_ERROR,
            error_val(
                "arity-error",
                format!(
                    "crypto/hmac-sha256: expected 2 arguments, got {}",
                    args.len()
                ),
            ),
        );
    }
    let key = match extract_byte_data(&args[0], "crypto/hmac-sha256", "key") {
        Ok(d) => d,
        Err(e) => return e,
    };
    let message = match extract_byte_data(&args[1], "crypto/hmac-sha256", "message") {
        Ok(d) => d,
        Err(e) => return e,
    };

    type HmacSha256 = Hmac<Sha256>;
    let mut mac = HmacSha256::new_from_slice(&key).expect("HMAC accepts any key length");
    mac.update(&message);
    let result = mac.finalize().into_bytes();
    (SIG_OK, Value::bytes(result.to_vec()))
}

// ---------------------------------------------------------------------------
// Registration table
// ---------------------------------------------------------------------------

static PRIMITIVES: &[PrimitiveDef] = &[
    PrimitiveDef {
        name: "crypto/sha256",
        func: prim_sha256,
        effect: Effect::none(),
        arity: Arity::Exact(1),
        doc: "SHA-256 hash. Accepts string, bytes, or blob. Returns 32 bytes.",
        params: &["data"],
        category: "crypto",
        example: "(bytes->hex (crypto/sha256 \"hello\"))",
        aliases: &["sha256"],
    },
    PrimitiveDef {
        name: "crypto/hmac-sha256",
        func: prim_hmac_sha256,
        effect: Effect::none(),
        arity: Arity::Exact(2),
        doc: "HMAC-SHA256. Takes (key, message). Returns 32 bytes.",
        params: &["key", "message"],
        category: "crypto",
        example: "(bytes->hex (crypto/hmac-sha256 \"key\" \"message\"))",
        aliases: &["hmac-sha256"],
    },
];
