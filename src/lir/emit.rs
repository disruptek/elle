//! LIR to Bytecode emission

use super::types::*;

/// Emits bytecode from LIR
pub struct Emitter {
    #[allow(dead_code)]
    bytecode: Vec<u8>,
}

impl Emitter {
    pub fn new() -> Self {
        Emitter {
            bytecode: Vec::new(),
        }
    }

    /// Emit bytecode from a LIR function
    pub fn emit(&mut self, _func: &LirFunction) -> Vec<u8> {
        // TODO: Implement full emission
        // For now, return empty bytecode
        Vec::new()
    }
}

impl Default for Emitter {
    fn default() -> Self {
        Self::new()
    }
}
