//! Elle TLS plugin — TLS state machine primitives via rustls.
//!
//! This plugin exposes rustls's UnbufferedClientConnection /
//! UnbufferedServerConnection as pure state machine primitives.
//! All socket I/O is performed in Elle code using stream/read and
//! stream/write on native TCP ports. No I/O happens in this plugin.

use elle::plugin::PluginContext;
use elle::primitives::def::PrimitiveDef;
use elle::signals::Signal;
use elle::value::fiber::{SignalBits, SIG_OK};
use elle::value::types::Arity;
use elle::value::{TableKey, Value};
use std::collections::BTreeMap;

/// Plugin entry point. Called by Elle when loading the `.so`.
///
/// # Safety
///
/// Called by Elle's plugin loader via `dlsym`. The caller must pass a valid
/// `PluginContext` reference. Only safe when called from `load_plugin`.
#[no_mangle]
pub unsafe extern "C" fn elle_plugin_init(ctx: &mut PluginContext) -> Value {
    // Install the ring crypto provider globally. Second call is a no-op (returns Err).
    let _ = rustls::crypto::ring::default_provider().install_default();

    let mut fields = BTreeMap::new();
    for def in PRIMITIVES {
        ctx.register(def);
        let short = def.name.strip_prefix("tls/").unwrap_or(def.name);
        fields.insert(TableKey::Keyword(short.into()), Value::native_fn(def.func));
    }
    Value::struct_from(fields)
}

// ---------------------------------------------------------------------------
// Primitive registration table (empty in chunk 1, filled in chunks 2–3)
// ---------------------------------------------------------------------------

static PRIMITIVES: &[PrimitiveDef] = &[];
