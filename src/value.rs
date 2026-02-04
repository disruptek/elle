use std::fmt;
use std::rc::Rc;

/// Symbol ID for interned symbols
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

/// Function arity specification
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Arity {
    Exact(usize),
    AtLeast(usize),
    Range(usize, usize),
}

impl Arity {
    pub fn matches(&self, n: usize) -> bool {
        match self {
            Arity::Exact(expected) => n == *expected,
            Arity::AtLeast(min) => n >= *min,
            Arity::Range(min, max) => n >= *min && n <= *max,
        }
    }
}

/// Native function type
pub type NativeFn = fn(&[Value]) -> Result<Value, String>;

/// Cons cell for list construction
#[derive(Debug, Clone, PartialEq)]
pub struct Cons {
    pub first: Value,
    pub rest: Value,
}

impl Cons {
    pub fn new(first: Value, rest: Value) -> Self {
        Cons { first, rest }
    }
}

/// Closure with captured environment
#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub bytecode: Rc<Vec<u8>>,
    pub arity: Arity,
    pub env: Rc<Vec<Value>>,
    pub num_locals: usize,
}

/// Core Lisp value type
#[derive(Clone, PartialEq)]
pub enum Value {
    Nil,
    Bool(bool),
    Int(i64),
    Float(f64),
    Symbol(SymbolId),
    String(Rc<str>),
    Cons(Rc<Cons>),
    Vector(Rc<Vec<Value>>),
    Closure(Rc<Closure>),
    NativeFn(NativeFn),
}

impl Value {
    #[inline(always)]
    pub fn is_nil(&self) -> bool {
        matches!(self, Value::Nil)
    }

    #[inline(always)]
    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Nil | Value::Bool(false))
    }

    pub fn as_int(&self) -> Result<i64, String> {
        match self {
            Value::Int(n) => Ok(*n),
            _ => Err(format!("Expected integer, got {:?}", self)),
        }
    }

    pub fn as_float(&self) -> Result<f64, String> {
        match self {
            Value::Float(f) => Ok(*f),
            Value::Int(n) => Ok(*n as f64),
            _ => Err(format!("Expected number, got {:?}", self)),
        }
    }

    pub fn as_symbol(&self) -> Result<SymbolId, String> {
        match self {
            Value::Symbol(id) => Ok(*id),
            _ => Err(format!("Expected symbol, got {:?}", self)),
        }
    }

    pub fn as_cons(&self) -> Result<&Rc<Cons>, String> {
        match self {
            Value::Cons(cons) => Ok(cons),
            _ => Err(format!("Expected cons, got {:?}", self)),
        }
    }

    pub fn as_vector(&self) -> Result<&Rc<Vec<Value>>, String> {
        match self {
            Value::Vector(vec) => Ok(vec),
            _ => Err(format!("Expected vector, got {:?}", self)),
        }
    }

    pub fn as_closure(&self) -> Result<&Rc<Closure>, String> {
        match self {
            Value::Closure(closure) => Ok(closure),
            _ => Err(format!("Expected closure, got {:?}", self)),
        }
    }

    /// Check if value is a proper list
    pub fn is_list(&self) -> bool {
        let mut current = self;
        loop {
            match current {
                Value::Nil => return true,
                Value::Cons(cons) => current = &cons.rest,
                _ => return false,
            }
        }
    }

    /// Convert list to Vec
    pub fn list_to_vec(&self) -> Result<Vec<Value>, String> {
        let mut result = Vec::new();
        let mut current = self.clone();
        loop {
            match current {
                Value::Nil => return Ok(result),
                Value::Cons(cons) => {
                    result.push(cons.first.clone());
                    current = cons.rest.clone();
                }
                _ => return Err("Not a proper list".to_string()),
            }
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Nil => write!(f, "nil"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(fl) => write!(f, "{}", fl),
            Value::Symbol(id) => write!(f, "Symbol({})", id.0),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Cons(cons) => {
                write!(f, "(")?;
                let mut current = Value::Cons(cons.clone());
                loop {
                    match current {
                        Value::Cons(ref c) => {
                            write!(f, "{:?}", c.first)?;
                            match &c.rest {
                                Value::Nil => break,
                                Value::Cons(_) => {
                                    write!(f, " ")?;
                                    current = c.rest.clone();
                                }
                                other => {
                                    write!(f, " . {:?}", other)?;
                                    break;
                                }
                            }
                        }
                        _ => break,
                    }
                }
                write!(f, ")")
            }
            Value::Vector(vec) => {
                write!(f, "[")?;
                for (i, v) in vec.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{:?}", v)?;
                }
                write!(f, "]")
            }
            Value::Closure(_) => write!(f, "<closure>"),
            Value::NativeFn(_) => write!(f, "<native-fn>"),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "{}", s),
            _ => write!(f, "{:?}", self),
        }
    }
}

/// Helper function to construct lists
pub fn list(values: Vec<Value>) -> Value {
    values
        .into_iter()
        .rev()
        .fold(Value::Nil, |acc, v| Value::Cons(Rc::new(Cons::new(v, acc))))
}

/// Helper to create cons cell
#[inline]
pub fn cons(first: Value, rest: Value) -> Value {
    Value::Cons(Rc::new(Cons::new(first, rest)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_list_construction() {
        let l = list(vec![Value::Int(1), Value::Int(2), Value::Int(3)]);
        assert!(l.is_list());
        let vec = l.list_to_vec().unwrap();
        assert_eq!(vec.len(), 3);
    }

    #[test]
    fn test_truthy() {
        assert!(Value::Int(0).is_truthy());
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Bool(false).is_truthy());
        assert!(!Value::Nil.is_truthy());
    }
}
