use std::str::CharIndices;
use std::iter::Peekable;

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Token {
    start: usize,
    len: usize,
    typ: TokenType,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum TokenType {
    // Special
    EOF, Ident, ImportPath, Number,
    String,

    // Symbols
    Colon, SemiColon, Assign, Equals,
    NotEquals, Bang, Greater, Lesser,
    GreaterSame, LesserSame, Dot,

    Asterisk, Percent, ForwardSlash,
    Plus, Minus, 
    OpenParen, CloseParen,          // ()
    OpenBracket, CloseBracket,      // []
    OpenBrace, CloseBrace,          // {}

    // Keywords
    Begin, End, Use, Function,
    Local, Const, Public, Type,
    For, In, While, Do, Struct,
    As,

    // For error reporting
    ErrorUnrecognisedChunk,
    ErrorUnterminatedString,
}

#[derive(Clone)]
pub struct ScannerState<'a> {
    source: &'a str,
    iter: Peekable<CharIndices<'a>>,
    output: Vec<Token>,
    found_error: bool,
}

impl<'a> ScannerState<'a> {
    fn new(source: &'a str) -> Self {
        ScannerState {
            source: source,
            iter: source.char_indices().peekable(),
            output: Vec::new(),
            found_error: false,
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
        return Some(self.iter.next()?.1);
    }
    fn at_end(&mut self) -> bool { self.iter.peek().is_none() }
    fn position(&mut self) -> usize {
        self.iter.peek().map(|(x,_)|*x)
            .unwrap_or(self.source.len())
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
            '{', '}', '[', ']', '"',
        ].contains(&test);
    }
    fn hit_eof(&mut self) -> bool {
        if self.output.len() == 0 {
            return false;
        }
        return self.output[self.output.len() - 1].typ == TokenType::EOF;
    }
    fn emit_token(&mut self, start: usize, typ: TokenType) {
        let pos = self.position();
        self.output.push(Token {
            start: start,
            len: pos - start,
            typ: typ,
        });
    }
    fn skip_comment(&mut self) {
        while match self.pull() {
            None => false,
            Some(x) => x != '\n',
        } {}
    }
}

// To save on indents, this is not a method

fn step_scanner(this: &mut ScannerState) {
    this.skip_whitespace();
    let start = this.position();
    let head = match this.pull() {
        Some(x) => x,
        None => {
            this.emit_token(start, TokenType::EOF);
            return;
        }
    };
    match head {
        '+' => this.emit_token(start, TokenType::Plus),
        '-' => this.emit_token(start, TokenType::Minus),
        '*' => this.emit_token(start, TokenType::Asterisk),
        '%' => this.emit_token(start, TokenType::Percent),
        ':' => this.emit_token(start, TokenType::Colon),
        ';' => this.emit_token(start, TokenType::SemiColon),
        '.' => this.emit_token(start, TokenType::Dot),
        '(' => this.emit_token(start, TokenType::OpenParen),
        ')' => this.emit_token(start, TokenType::CloseParen),
        '{' => this.emit_token(start, TokenType::OpenBrace),
        '}' => this.emit_token(start, TokenType::CloseBrace),
        '[' => this.emit_token(start, TokenType::OpenBracket),
        ']' => this.emit_token(start, TokenType::CloseBracket),

        '/' => if this.peek() == Some('/') { this.skip_comment(); }
                else { this.emit_token(start, TokenType::ForwardSlash); }

        '<' => if this.peek() == Some('=') {
                    this.pull();
                    this.emit_token(start, TokenType::LesserSame);
                } else {
                    this.emit_token(start, TokenType::Lesser);
                }

        '>' => if this.peek() == Some('=') {
                    this.pull();
                    this.emit_token(start, TokenType::GreaterSame);
                } else {
                    this.emit_token(start, TokenType::Greater);
                }

        '!' => if this.peek() == Some('=') {
                    this.pull();
                    this.emit_token(start, TokenType::NotEquals);
                } else {
                    this.emit_token(start, TokenType::Bang);
                }

        '=' => if this.peek() == Some('=') {
                    this.pull();
                    this.emit_token(start, TokenType::Equals);
                } else {
                    this.emit_token(start, TokenType::Assign);
                }

        x   => scanner_long_step(this, start, x),
    }
}

// this is for non-trivial tokens
// also the only error check.
fn scanner_long_step(this: &mut ScannerState, start: usize, head: char) {
    if head.is_alphabetic() {
        // do identifier stuff
        while match this.peek() {
            None => false,
            Some(x) => x.is_alphanumeric() || x == '_',
        } { this.pull(); }
        
        let end = this.position();
        let ident = &this.source[start .. end];
        match ident {
            "begin" => this.emit_token(start, TokenType::Begin),
            "end" => this.emit_token(start, TokenType::End),
            "use" => this.emit_token(start, TokenType::Use),
            "function" => this.emit_token(start, TokenType::Function),
            "local" => this.emit_token(start, TokenType::Local),
            "const" => this.emit_token(start, TokenType::Const),
            "public" => this.emit_token(start, TokenType::Public),
            "type" => this.emit_token(start, TokenType::Type),
            "for" => this.emit_token(start, TokenType::For),
            "in" => this.emit_token(start, TokenType::In),
            "while" => this.emit_token(start, TokenType::While),
            "do" => this.emit_token(start, TokenType::Do),
            "struct" => this.emit_token(start, TokenType::Struct),
            "as" => this.emit_token(start, TokenType::As),
            _ =>  this.emit_token(start, TokenType::Ident),
        }
        return;
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
        this.emit_token(start, TokenType::Number);
        return;
    }
    if head == '"' {
        return;
    }
    while !this.on_boundary() {
        this.pull();
    };
    this.found_error = true;
    this.emit_token(start, TokenType::ErrorUnrecognisedChunk);
}

#[cfg(test)]
mod test_scanner {
    use super::*;
    #[test]
    fn identifier() {
        // I don't trust my own code :)
        let mut scanner = ScannerState::new("ident1 begin");
        while !scanner.at_end() {step_scanner(&mut scanner);}
        let tokens = scanner.output;
        assert_eq!(tokens[0].len, 6);
        assert_eq!(tokens[0].start, 0);

        assert_eq!(tokens[1].typ, TokenType::Begin);
        assert_eq!(tokens[1].start, 7);
    }

    #[test]
    fn expression() {
        let source = "x * 2 + c";
        let expected = [
            Token { start: 0, len: 1, typ: TokenType::Ident },
            Token { start: 2, len: 1, typ: TokenType::Asterisk },
            Token { start: 4, len: 1, typ: TokenType::Number },
            Token { start: 6, len: 1, typ: TokenType::Plus },
            Token { start: 8, len: 1, typ: TokenType::Ident },
        ];

        let mut scanner = ScannerState::new(source);
        while !scanner.at_end() {step_scanner(&mut scanner);}
        let tokens = scanner.output;
        
        for (a, b) in expected.iter().zip(tokens.iter()) {
            assert_eq!(a, b);
        }
    }
    #[test]
    fn error() {
        let source = "#@: `~`";
        
        let mut scanner = ScannerState::new(source);
        while !scanner.at_end() {step_scanner(&mut scanner);}
        assert!(scanner.found_error);

        assert_eq!(
            scanner.output[0].typ,
            TokenType::ErrorUnrecognisedChunk,
        );
        assert_eq!(
            scanner.output[2].typ,
            TokenType::ErrorUnrecognisedChunk,
        );
    }

    #[test]
    fn strings() {
        let source_success = "\"Hello, World!\"";
        let source_error   = "\"Ehh, Later";

        let mut scanner = ScannerState::new(source_success);
        while !scanner.at_end() {step_scanner(&mut scanner);}

        let mut scanner = ScannerState::new(source_error);
        while !scanner.at_end() {step_scanner(&mut scanner);}
    }
}
