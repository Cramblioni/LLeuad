/*
//  This is the tree-walking interpreter part of Lleuad.
//  Memory will be managed using rust`s Rc to have
//  reference counting.

//  This is the prototype interpreter. It doesn not implement
//  most features. It is dynamically typed and ignores type annotations.

// Importing our parser
//      pulling the AST type directly into our
//      namespace.
use crate::parser::{self, Ast};
use crate::SharedStr;
// Rust's `Rc` for reference counting
use std::rc::Rc;
// Using a hashmap as an associative array.
use std::collections::HashMap;
// This allows us to use the borrow trait
// This makes using `Rc` easier.
use std::borrow::Borrow;
// This is used in place of `&mut`
// Again to do with lifetimes, but we keep ownership.
use std::ptr::NonNull;

enum ValueObj {
    // -- Dynamic Primitives -- //
    String(String),
    Int(i64),
    Float(f64),
    
//    // -- Special dynamic values -- //
//    // A structures pool with a reference to
//    // the name mapping.
//    Struct{
//        spec: Rc<StructSpec>,
//        items: Vec<Value>,
//    },
//    // This wraps a function spec.
    Function(Rc<FuncSpec>),
    Namespace(HashMap<SharedStr, Value>),
}

// A handy shorthand for a reference counted value.
type Value = Rc<ValueObj>;

// A simple mapping from field names to an index in the structure pool
struct StructSpec {
    mapping: HashMap<SharedStr, usize>,
}

// All of the required information to create an execution frame
// including mapping arguments to their respective names.
struct FuncSpec {
    param_names: Vec<SharedStr>,
    code: Vec<Rc<Ast>>,
}

struct ContextFrame {
    varmap: HashMap<SharedStr, Value>,
    prior: Option<NonNull<ContextFrame>>,
}

impl ContextFrame {
    // creating a new context
    fn new() -> Self {
        ContextFrame {
            varmap: HashMap::new(),
            prior: None
        }
    }

    // creating a sub-context
    fn subcontext(&mut self) -> ContextFrame { 
        ContextFrame {
            varmap: HashMap::new(),
            prior: Some(NonNull::new(self).unwrap())
        }
    }

    // getting references to values by name
    // (generic over the key so we can use any string type)
    fn get<Q>(&self, name: &Q) -> Option<&Value>
    where SharedStr : Borrow<Q>, Q: std::cmp::Eq + std::hash::Hash + ?Sized {
        let temp = self.varmap.get(name);
        if temp.is_none() {
            if let Some(prior) = &self.prior {
                return unsafe { prior.as_ref() }.get::<Q>(name);
            }
        }
        return temp;
    }
    // getting mutable references to values by name
    // (generic over the key so we can use any string type)
    fn get_mut<Q>(&mut self, name: &Q) -> Option<&mut Value>
    where SharedStr : Borrow<Q>, Q: std::cmp::Eq + std::hash::Hash + ?Sized {
        let temp = self.varmap.get_mut(name);
        if temp.is_none() {
            if let Some(prior) = &mut self.prior {
                return unsafe { prior.as_mut() }.get_mut::<Q>(name);
            }
        }
        return temp;
    }

    fn set(&mut self, name: &SharedStr, value: Value) {
        match self.get_mut(name) {
            Some(x) => *x = value,
            None => { self.varmap.insert(Rc::clone(name), value); },
        }
    }
    fn set_local(&mut self, name: &SharedStr, value: Value) {
        self.varmap.insert(Rc::clone(name), value);
    }
}

// This function evaluates expressions (aborting on error)
fn eval_expr(frame: &ContextFrame, node: &Ast) -> Value {
    // We bring the syntax node into scope and give it a short name
    use parser::SynNode as SN;

    // we pattern match the ast node to run specific code based
    // on the type of the node.
    match &node.node {
        SN::Ident(name) => {
            let cell = frame.get(name);
            if cell.is_none() {
                panic!("Name lookup failed, {} not in scope", name);
            }
            Rc::clone(cell.unwrap())
        },
        // For literal values, we create value objects from them
        SN::IntLit(val) =>
            return Rc::new(ValueObj::Int(*val)),
        SN::FloatLit(val) =>
            return Rc::new(ValueObj::Float(*val)),
        SN::StringLit(val) => {
            let inner: &str = val.borrow();
            return Rc::new(ValueObj::String(String::from(inner)))
        },

        SN::Attr(_, _) =>
            panic!("attributes"),

        SN::Call(targ, args) => {
            // we evaluate our callee.
            let func = eval_expr(frame, targ.borrow());
            // we check to see if the callee is callable.
            let spec = match func.borrow() {
                ValueObj::Function(spec) => spec,
                _ => panic!("attempt to call non-callable"),
            };

            // We create the context the function will run in.
            //  Then we go through the arguments, evaluating them
            //  and placing them into the new context.
            let mut context = ContextFrame::new();
            let params = &spec.param_names;
            for (param, arg) in params.iter().zip(args.iter()) {
                context.set(param, eval_expr(frame, arg));
            }

            // Now we evaluate the body of the function in the context we
            // made for it. Then we poke around the context to find a return
            // If any.
            interp_block(&mut context, &spec.code);
            let result = context.get("< return >");
            if result.is_none() {
                panic!("void function in expressions");
            }
            return Rc::clone(result.unwrap());
        }

        _ => panic!("Non-Expression term found in expression"),
    }
}

// Applies `interp_node` across all statement nodes in a 
// code block
fn interp_block(frame: &mut ContextFrame, nodes: &[Rc<Ast>]) -> bool {
    for node in nodes {
        if interp_node(frame, node) { return true; };
    }
    return false;
}

// interprets a single statement node.
// returns true if a return is encountered
fn interp_node(frame: &mut ContextFrame, node: &Ast) -> bool {
    use parser::SynNode as SN;
    match &node.node {
        SN::Return(expr) => {
            if expr.is_none() { return true; }
            let val = eval_expr(frame, &expr.unwrap());
            return true;
        },
        SN::Use(lib, path) => {
            let mut root = if lib.is_some() {
                panic!("Loading libraries is not currently supported");
            } else {
                frame.get(&path[0])
            };
            for item in &path[1..] {
                root = get_attrib(root, item);
            }
            frame.set_local(path.last().unwrap(), root);
        }

        _ => panic!("non-statement term found in statement"),
    }
    return false;
}
*/
