use crate::value::SymbolId;
use rustc_hash::FxHashMap;

/// Symbol interning table for fast symbol comparison
#[derive(Debug)]
pub struct SymbolTable {
    map: FxHashMap<String, SymbolId>,
    names: Vec<String>,
}

impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            map: FxHashMap::default(),
            names: Vec::new(),
        }
    }

    /// Intern a symbol, returning its ID
    pub fn intern(&mut self, name: &str) -> SymbolId {
        if let Some(&id) = self.map.get(name) {
            return id;
        }

        let id = SymbolId(self.names.len() as u32);
        self.names.push(name.to_string());
        self.map.insert(name.to_string(), id);
        id
    }

    /// Get the name of a symbol by ID
    pub fn name(&self, id: SymbolId) -> Option<&str> {
        self.names.get(id.0 as usize).map(|s| s.as_str())
    }

    /// Check if a symbol exists
    pub fn get(&self, name: &str) -> Option<SymbolId> {
        self.map.get(name).copied()
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_interning() {
        let mut table = SymbolTable::new();
        let id1 = table.intern("foo");
        let id2 = table.intern("bar");
        let id3 = table.intern("foo");

        assert_eq!(id1, id3);
        assert_ne!(id1, id2);
        assert_eq!(table.name(id1), Some("foo"));
        assert_eq!(table.name(id2), Some("bar"));
    }
}
