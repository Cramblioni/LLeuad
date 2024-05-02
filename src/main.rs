// This references

mod parser;
mod tree_walker;
mod diag;

use std::rc::Rc;
// an alternative to `String`, allows sharing the strings
// without needing to pass around lifetimes. This is used
// by the interpreter where ownership isn't required.
// (In rust terms, this is immutable)
pub type SharedStr = Rc<str>;

fn main() {
    println!("Hello, world!");
}
