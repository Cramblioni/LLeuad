use crate::SharedStr;

// formatting traits
use std::fmt;

// marks the start and end of this node in the source file.
// more information can be found in [notes/span.md]
#[derive(Clone, PartialEq, Debug)]
pub struct Span {
    // Cloning `SharedStr` does not reallocate the string
    pub source: SharedStr,
    pub line: u32,  // These shouldn't need to be 64-bit
    pub column: u32,
    start: usize,
    len: usize,
}

impl Span {
    pub fn new(source: SharedStr, line: u32, column: u32, start: usize, len: usize)
    -> Span {
        return Self { source, line, column, start, len };
    }
    pub fn position(&self) -> usize {
        return self.start;
    }
    pub fn len(&self) -> usize {
        return self.len;
    }
    // Merges two spans, resulting in a 
    pub fn merge(first: Span, last: Span) -> Span {
        let start = first.start.min(last.start);
        let len = (first.start + first.len).max(last.start + last.len) - start;
        let line = first.line.min(last.line);
        let column = first.column.min(last.column);
        return Span::new(first.source, line, column, start, len);
    }
    pub fn enclose(&mut self, other: Span) {
        let start = self.start.min(other.start);
        let len = (self.start + self.len).max(other.start + other.len) - start;
        
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_fmt(format_args!("{}:{}:{}:",
            &self.source,
            self.line,
            self.column
        ))?;
        return Ok(());
    }
}
