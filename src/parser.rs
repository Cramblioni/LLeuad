// shared strings
use crate::SharedStr;
// Location information for diagnostics
use crate::diag::{Span};
// iterate over strings getting both characters and indices
use std::str::CharIndices;
// Allows us to peek what item an iterator will yield next
use std::iter::Peekable;
// Reference counting
use std::rc::Rc;
// Interior mutability
use std::cell::Cell;

// importing traits to use specific methods
use std::borrow::Borrow;
use std::str::FromStr;

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    typ: TokenType,
    text: &'a str,
    loc: Span
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    // Special
    EOF, Ident, ImportPath, Number,
    String, Boolean,

    // Symbols
    Colon, SemiColon, Assign, Equals,
    NotEquals, Bang, Greater, Lesser,
    GreaterSame, LesserSame, Dot, Comma,
    Ampersand, VerticalBar,

    Asterisk, ForwardSlash,
    Plus, Minus, 
    OpenParen, CloseParen,          // ()
    OpenBracket, CloseBracket,      // []
    OpenBrace, CloseBrace,          // {}

    // Keywords
    Begin, End, Use, Function,
    Local, Const, Public, Type,
    For, In, While, Do, Struct,
    As, Return,
}

// This is a pull lexer
#[derive(Clone)]
pub struct ScannerState<'a> {
    file: SharedStr,
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    line: u32,
    column: u32,
}

impl<'a> ScannerState<'a> {
    fn new(filename: &'_ str, source: &'a str) -> Self {
        ScannerState {
            file: Rc::from(source),
            source: source,
            iter: source.char_indices().peekable(),
            line: 1,
            column: 1,
        }
    }
    // Utility functions
    fn skip_whitespace(&mut self) {
        while self.peek().map(|x|x.is_whitespace()).unwrap_or(false) {
            self.pull();
        }
    }
    fn peek(&mut self) -> Option<char> {
        return Some(self.iter.peek()?.clone().1);
    }
    fn pull(&mut self) -> Option<char> {
        let ret = self.iter.next()?.1;
        if ret == '\n'{
            self.column = 1;
            self.line += 1;
        } else {
            self.column += 1;
        }
        return Some(ret);
    }
    fn at_end(&mut self) -> bool {
        return self.iter.peek().is_none();
    }
    fn byte_position(&mut self) -> usize {
        return self.iter.peek().map(|(x,_)|*x)
            .unwrap_or(self.source.len());
    }
    fn position(&self) -> Span {
        return Span::new(
            Rc::clone(&self.file),
            self.line,
            self.column,
            self.byte_position(),
            0
        )
    }
    
    // This returns `true` iff the current character
    // is the head of a trivial token.
    fn on_boundary(&mut self) -> bool {
        if self.peek().is_none() {
            // hitting the end of the string is
            // the start of the EOF token.
            return true;
        }
        let test = self.peek().unwrap();
        if test.is_whitespace() {
            return true;
        }
        if test.is_alphanumeric() {
            return true;
        }
        return [
            '+', '-', '*', '/', '%', '>', '<',
            ':', ';', '!', '=', '.', '(', ')',
            '{', '}', '[', ']', '"', ',', '&',
            '|',
        ].contains(&test);
    }
    fn create_token(&mut self, start: (Span, usize), typ: TokenType) -> Token<'a> {
        let pos = self.byte_position();
        return Token {
            loc: start.0,
            text: &self.source[start.1 .. pos],
            typ: typ,
        };
    }
    fn skip_comment(&mut self) {
        while match self.pull() {
            None => false,
            Some(x) => x != '\n',
        } {}
    }
}

fn report<S: Borrow<str>>(loc: Span, message: S) {
    eprintln!("{}:{}:{}:{}",
        loc.source, loc.line, loc.column, message.borrow());
}

// To save on indents, this is not a method
fn step_scanner<'a>(this: &mut ScannerState<'a>) -> Option<Token<'a>> {
    this.skip_whitespace();
    let start = (this.position(), this.byte_position());
    let head = match this.pull() {
        Some(x) => x,
        None => {
            return Some(this.create_token(start, TokenType::EOF));
        }
    };
    match head {
        '+' => return Some(this.create_token(start, TokenType::Plus)),
        '-' => return Some(this.create_token(start, TokenType::Minus)),
        '*' => return Some(this.create_token(start, TokenType::Asterisk)),
        '&' => return Some(this.create_token(start, TokenType::Ampersand)),
        '|' => return Some(this.create_token(start, TokenType::VerticalBar)),
        ':' => return Some(this.create_token(start, TokenType::Colon)),
        ';' => return Some(this.create_token(start, TokenType::SemiColon)),
        '.' => return Some(this.create_token(start, TokenType::Dot)),
        ',' => return Some(this.create_token(start, TokenType::Comma)),
        '(' => return Some(this.create_token(start, TokenType::OpenParen)),
        ')' => return Some(this.create_token(start, TokenType::CloseParen)),
        '{' => return Some(this.create_token(start, TokenType::OpenBrace)),
        '}' => return Some(this.create_token(start, TokenType::CloseBrace)),
        '[' => return Some(this.create_token(start, TokenType::OpenBracket)),
        ']' => return Some(this.create_token(start, TokenType::CloseBracket)),

        '/' => if this.peek() == Some('/') {
                this.skip_comment();
                return step_scanner(this);
            } else {
                return Some(this.create_token(start, TokenType::ForwardSlash));
            }

        '<' => if this.peek() == Some('=') {
                    this.pull();
                    return Some(this.create_token(start, TokenType::LesserSame));
                } else {
                    return Some(this.create_token(start, TokenType::Lesser));
                }

        '>' => if this.peek() == Some('=') {
                    this.pull();
                    return Some(this.create_token(start, TokenType::GreaterSame));
                } else {
                    return Some(this.create_token(start, TokenType::Greater));
                }

        '!' => if this.peek() == Some('=') {
                    this.pull();
                    return Some(this.create_token(start, TokenType::NotEquals));
                } else {
                    return Some(this.create_token(start, TokenType::Bang));
                }

        '=' => if this.peek() == Some('=') {
                    this.pull();
                    return Some(this.create_token(start, TokenType::Equals));
                } else {
                    return Some(this.create_token(start, TokenType::Assign));
                }

        x   => scanner_long_step(this, start, x),
    }
}

// this is for non-trivial tokens
// also the only error check.
fn scanner_long_step<'a>
(this: &mut ScannerState<'a>, start: (Span, usize), head: char)
-> Option<Token<'a>>{
    if head.is_alphabetic() {
        // do identifier stuff
        while match this.peek() {
            None => false,
            Some(x) => x.is_alphanumeric() || x == '_',
        } { this.pull(); }
        
        let end = this.byte_position();
        let ident = &this.source[start.1 .. end];
        match ident {
            "begin" => return Some(this.create_token(start, TokenType::Begin)),
            "end" => return Some(this.create_token(start, TokenType::End)),
            "use" => return Some(this.create_token(start, TokenType::Use)),
            "function" => return Some(this.create_token(start, TokenType::Function)),
            "local" => return Some(this.create_token(start, TokenType::Local)),
            "const" => return Some(this.create_token(start, TokenType::Const)),
            "public" => return Some(this.create_token(start, TokenType::Public)),
            "type" => return Some(this.create_token(start, TokenType::Type)),
            "for" => return Some(this.create_token(start, TokenType::For)),
            "in" => return Some(this.create_token(start, TokenType::In)),
            "while" => return Some(this.create_token(start, TokenType::While)),
            "do" => return Some(this.create_token(start, TokenType::Do)),
            "struct" => return Some(this.create_token(start, TokenType::Struct)),
            "as" => return Some(this.create_token(start, TokenType::As)),
            "return" => return Some(this.create_token(start, TokenType::Return)),
            "true" => return Some(this.create_token(start, TokenType::Boolean)),
            "false" => return Some(this.create_token(start, TokenType::Boolean)),
            _ =>  return Some(this.create_token(start, TokenType::Ident)),
        }
    }
    if head.is_numeric() {
        // do number stuff here
        while match this.peek() {
            None => false,
            Some(x) => x.is_numeric(),
        } { this.pull(); }
        if let Some('.') = this.peek() {
            this.pull();
            while match this.peek() {
                None => false,
                Some(x) => x.is_numeric(),
            } { this.pull(); }
        }
        return Some(this.create_token(start, TokenType::Number));
    }
    if head == '"' {
        let mut terminated = false;
        let mut malformed = false;
        while match this.peek() {
            Some(_) => true,
            None => false,
        } {
            let cur = this.pull().unwrap();
            if cur == '"' || cur == '\n' {
                terminated = cur == '"';
                break;
            }
            if cur != '\\' { continue; }
            match this.pull() {
                Some('n' | 't' | '"') => continue,
                _ => (),
            }
            malformed = true;
        }
        if !terminated {
            report(start.0, "unterminated string");
            return None;
        } else if malformed {
            report(start.0, "malformed string");
            return None;
        }
        return Some(this.create_token(start, TokenType::String)); 
    }
    while !this.on_boundary() {
        this.pull();
    };
    report(start.0, "unrecognised chunk");
    return None;
}

fn peek_scanner<'a>(this: &ScannerState<'a>) -> Option<Token<'a>> {
    let mut clop = this.clone();
    return step_scanner(&mut clop);
}

#[cfg(test)]
mod test_scanner {
    use super::*;

    #[test]
    fn expression() {
        let source = "x * 2 + c";
        let expected = [
            TokenType::Ident,
            TokenType::Asterisk,
            TokenType::Number,
            TokenType::Plus,
            TokenType::Ident,
            TokenType::EOF,
        ];

        let mut scanner = ScannerState::new("test.lleu", source);
        for typ in expected {
            let tok = step_scanner(&mut scanner).expect("parse error");
            assert_eq!(typ, tok.typ);
        }
    }

    #[test]
    fn comments() {
        let source = "//comment\nx";

        let mut scanner = ScannerState::new("test.lleu", source);
        assert_eq!(
            step_scanner(&mut scanner).expect("failed parse").typ,
            TokenType::Ident,
        );
    }
    #[test]
    fn error() {
        let source = "#@: `~`";
        let expected = [
            None,
            Some(TokenType::Colon),
            None,
        ];
        
        let mut scanner = ScannerState::new("test.lleu", source);
        for typ in expected {
            let got = step_scanner(&mut scanner).map(|x| x.typ);
            assert_eq!(typ, got);
        }
    }

    #[test]
    fn strings() {
        let source_success = "\"Hello!\""; println!("{}", source_success);
        let source_error1   = "\"Ehh, Later"; println!("{}", source_error1);
        let source_error2   = "\"-\\2-\""; println!("{}", source_error2);

        let mut scanner = ScannerState::new("test.lleu", source_success);
        assert_eq!(
            step_scanner(&mut scanner).expect("failed lex").typ,
            TokenType::String,
        );

        let mut scanner = ScannerState::new("test.lleu", source_error1);
        assert_eq!(
            step_scanner(&mut scanner),
            None,
        );

        let mut scanner = ScannerState::new("test.lleu", source_error2);
        assert_eq!(
            step_scanner(&mut scanner),
            None,
        );
    }
    use std::fs;
    #[test]
    fn source_file() {
        let source = fs::read_to_string("./data/test.lleu")
            .expect("test.lleu is missing");

        let mut scanner = ScannerState::new("./data/test.lleu", &source);
        while let Some(x) = step_scanner(&mut scanner) {
            match x.typ {
                TokenType::EOF => return,
                _ => (),
            }
        }
        assert!(false);
    }
}



// Parsing Time!

// NOTE: Aim to use `Rc` over `Box` or `&`
//      We are going to make use of treesharing.

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    // Location specifics is node dependent
    // Spans can be read about in [notes/span.md]
    pub loc: Span,
    pub node: ExprNode,
}
#[derive(Debug, Clone, PartialEq)]
enum ExprNode {
    Ident(SharedStr),
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),

    Attr(Rc<Expr>, SharedStr),

    Call(Rc<Expr>, Vec<Rc<Expr>>),
    BinOp(BinOper, Rc<Expr>, Rc<Expr>),
    UnOp(UnOper, Rc<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOper {
    // numeric
    Add, Subtract, Multiply, Divide,
    // Comparisons
    Greater, GreaterEqual, Lesser, LesserEqual, Equal, NotEqual,
    // Boolean
    And, Or,
}
#[derive(Debug, Clone, PartialEq)]
pub enum UnOper {
    // numeric
    Negate,
    // Boolean
    Not,
} 

fn skip_statement(scanner: &mut ScannerState) {
    loop {
        let res = step_scanner(scanner);
        if res.is_none() {
            continue;
        }
        let res = res.unwrap();
        // This consumes the semicolon
        if &res.typ == &TokenType::SemiColon {
            return;
        }
        if &res.typ == &TokenType::EOF {
           return; 
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
struct ParseError{
    loc: Span,
    expected: Vec<TokenType>,
    got: TokenType,
}

impl ParseError {
    fn expected(loc: Span, expected: &[TokenType], got: TokenType) -> ParseError {
        return ParseError {
            loc: loc,
            expected: expected.iter().copied().collect(),
            got: got,
        };
    }
    fn merge(&mut self, expect: &[TokenType]) {
        self.expected.extend_from_slice(expect);
    }
}

fn parse_literal(this: &mut ScannerState) -> Result<Expr, Option<ParseError>> {
    use TokenType as TK;
    match peek_scanner(this).map(|x| x.typ).ok_or(None)? {
        TK::Ident => {
            let got = step_scanner(this).unwrap();
            let ident = SharedStr::from(got.text);
            let node = ExprNode::Ident(ident);
            return Ok(Expr{loc: got.loc, node: node});
        },
        TK::Number   => {
            let got = step_scanner(this).unwrap();
            if got.text.contains('.') {
                // float
                let node = ExprNode::Float(f64::from_str(got.text).unwrap()); 
                return Ok(Expr{loc: got.loc, node: node});
            } else {
                // int
                let node = ExprNode::Int(i64::from_str(got.text).unwrap());
                return Ok(Expr{loc: got.loc, node: node});
            }
        },
        TK::Boolean  => {
            let got = step_scanner(this).unwrap();
            let val = got.text == "true";
            let node = ExprNode::Bool(val);
            return Ok(Expr{loc: got.loc, node: node});
        },
        TK::String => {
            let got = step_scanner(this).unwrap();
            let end = got.text.len().saturating_sub(1);
            let node = ExprNode::String(String::from(&got.text[1 .. end]));
            return Ok(Expr{loc: got.loc, node: node});
        },

        TK::OpenParen => {
            step_scanner(this);
            todo!();
            step_scanner(this).ok_or(None)?;
        },

        x => {
            let got = peek_scanner(this).unwrap();
            return Err(Some(ParseError::expected(got.loc,
                &[TK::Ident, TK::Number, TK::String, TK::Boolean],
                x))
            );
        }
    }
}

fn parse_unary(this: &mut ScannerState) -> Result<Expr, Option<ParseError>> {
    let ahead = peek_scanner(this).ok_or(None)?;
    match 
}

#[cfg(test)]
mod parser_test {
    use super::*;

    #[test]
    fn literals() {
        let source = "10 9.11 false \"Hello!\" ident";
        let expected = [
            ExprNode::Int(10),
            ExprNode::Float(9.11),
            ExprNode::Bool(false),
            ExprNode::String(String::from("Hello!")),
            ExprNode::Ident(SharedStr::from("ident")),
        ];

        let mut scanner = ScannerState::new("test.lleu", source);
        for wants in expected {
            let result = parse_literal(&mut scanner);
            assert!(result.is_ok());
            let result = result.unwrap();
            assert_eq!(result.node, wants);
        }
    }
}
