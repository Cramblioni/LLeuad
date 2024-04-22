//  This is the tree-walking interpreter part of Lleuad.
//  Memory will be managed using rust`s Rc to have
//  reference counting.

//  This is the prototype interpreter. It doesn not implement
//  most features. It is dynamically typed and ignores type annotations.

// The AST our parser generates
use crate::parser::Ast;
use crate::SharedString;
// Rust's `Rc` for reference counting
use std::rc::Rc;
// Using a hashmap as an associative array.
use std::collections::HashMap;

enum Value {
    // -- Dynamic Primitives -- //
    String(String),
    Int(i64),
    Float(f64),
    
    // -- Special dynamic values -- //
    // A structures pool with a reference to
    // the name mapping.
    Struct{
        spec: Rc<StructSpec>,
        items: Vec<ValueRef>,
    },
    // This wraps a function spec.
    Function(Rc<FuncSpec>),
}

// A handy shorthand for a reference counted value.
type ValueRef = Rc<Value>;

// A simple mapping from field names to an index in the structure pool
struct StructSpec {
    mapping: HashMap<String, usize>,
}

// All of the required information
struct FuncSpec {
    param_names: Vec<SharedString>,
    code: Rc<Ast>,
}

