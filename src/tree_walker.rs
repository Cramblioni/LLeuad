use crate::SharedStr;
use crate::parser::{Expr, UnOper, BinOper};

use std::collections::HashMap;
use std::rc::Rc;

pub trait Visitor {
    type Value;
    fn visit_ident  (visitor: &mut Self, source: &Expr, val: &SharedStr) -> Self::Value;
    fn visit_int    (visitor: &mut Self, source: &Expr, val: &i64) -> Self::Value;
    fn visit_float  (visitor: &mut Self, source: &Expr, val: &f64) -> Self::Value;
    fn visit_string (visitor: &mut Self, source: &Expr, val: &String) -> Self::Value;
    fn visit_bool   (visitor: &mut Self, source: &Expr, val: &bool) -> Self::Value;
    
    fn visit_attr (visitor: &mut Self, source: &Expr, val: (&Rc<Expr>, &SharedStr)) -> Self::Value;
    fn visit_call (visitor: &mut Self, source: &Expr, val: (&Rc<Expr>, &Vec<Rc<Expr>>)) -> Self::Value;
    fn visit_binop (visitor: &mut Self, source: &Expr, val: (&BinOper, Rc<Expr>, Rc<Expr>)) -> Self::Value;
    fn visit_unop (visitor: &mut Self, source: &Expr, val: (&UnOper, &Rc<Expr>)) -> Self::Value;
}

trait Visitee {
    fn accept<V: Visitor>(&self, visitor: &mut V) -> V::Value;
}

enum ValueObj {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
}
type Value = Rc<ValueObj>;

struct Evaluate(HashMap<SharedStr, Value>);

impl Visitor for Evaluate {    
    type Value = Value;
    fn visit_ident  (visitor: &mut Self, source: &Expr, val: &SharedStr) -> Self::Value {
        return visitor.0.get(val).unwrap_or_else(|| {
            eprintln!("{} unrecognised ident {}", source.loc, val);
            panic!("runtime error");
        }).clone();
    }
    fn visit_int    (visitor: &mut Self, source: &Expr, val: &i64) -> Self::Value {
        return Rc::new(ValueObj::Int(*val));
    }
    fn visit_float  (visitor: &mut Self, source: &Expr, val: &f64) -> Self::Value {
        return Rc::new(ValueObj::Float(*val));
    }
    fn visit_string (visitor: &mut Self, source: &Expr, val: &String) -> Self::Value {
        return Rc::new(ValueObj::String(String::from(val)));
    }
    fn visit_bool   (visitor: &mut Self, source: &Expr, val: &bool) -> Self::Value {
        return Rc::new(ValueObj::Bool(*val));
    }
    
    fn visit_attr (visitor: &mut Self, source: &Expr, val: (&Rc<Expr>, &SharedStr)) -> Self::Value {
        panic!("{} Unsupported operation", source.loc);
    }
    fn visit_call (visitor: &mut Self, source: &Expr, val: (&Rc<Expr>, &Vec<Rc<Expr>>)) -> Self::Value;
    fn visit_binop (visitor: &mut Self, source: &Expr, val: (&BinOper, Rc<Expr>, Rc<Expr>)) -> Self::Value;
    fn visit_unop (visitor: &mut Self, source: &Expr, val: (&UnOper, &Rc<Expr>)) -> Self::Value;
}
