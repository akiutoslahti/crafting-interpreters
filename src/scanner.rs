use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug)]
pub enum ScanningError {
    UnexpectedChar(char, String),
    UnterminatedString(String),
}

impl Display for ScanningError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const PREFIX: &str = "Scanning error!";
        const PADDING: usize = 4;
        let pad_msg = |msg: &str| -> String {
            msg.lines()
                .map(|line| format!("{}{}\n", " ".repeat(PADDING), line))
                .collect()
        };

        match self {
            ScanningError::UnexpectedChar(c, msg) => {
                write!(
                    f,
                    "{} Unexpected character \'{}\'.\n\n{}",
                    PREFIX,
                    c,
                    pad_msg(msg)
                )
            }
            ScanningError::UnterminatedString(msg) => {
                write!(f, "{} Unterminated string.\n\n{}", PREFIX, pad_msg(msg))
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut keywords = HashMap::new();
        keywords.insert("and", TokenType::And);
        keywords.insert("class", TokenType::Class);
        keywords.insert("else", TokenType::Else);
        keywords.insert("false", TokenType::False);
        keywords.insert("fun", TokenType::Fun);
        keywords.insert("for", TokenType::For);
        keywords.insert("if", TokenType::If);
        keywords.insert("nil", TokenType::Nil);
        keywords.insert("or", TokenType::Or);
        keywords.insert("print", TokenType::Print);
        keywords.insert("return", TokenType::Return);
        keywords.insert("super", TokenType::Super);
        keywords.insert("this", TokenType::This);
        keywords.insert("true", TokenType::True);
        keywords.insert("var", TokenType::Var);
        keywords.insert("while", TokenType::While);
        keywords
    };
}

#[derive(Debug, Clone)]
pub enum LiteralType {
    Identifier(String),
    String(String),
    Number(f64),
}

#[derive(Clone)]
pub struct Token {
    pub tokentype: TokenType,
    pub lexeme: String,
    pub offset: usize,
    pub literal: Option<LiteralType>,
}

impl Token {
    fn new(
        tokentype: TokenType,
        lexeme: String,
        offset: usize,
        literal: Option<LiteralType>,
    ) -> Self {
        Self {
            tokentype,
            lexeme,
            offset,
            literal,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[{:?}, \"{}\", {}, {:?}]",
            self.tokentype, self.lexeme, self.offset, self.literal
        )
    }
}

pub struct Tokens(pub Vec<Token>);

impl Display for Tokens {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let tokens = &self.0;
        if tokens.is_empty() {
            write!(f, "[]")
        } else {
            let mut first = true;
            let res = tokens.iter().fold(write!(f, "["), |res, token| {
                if first {
                    first = false;
                    res.and_then(|_| write!(f, "{}", token))
                } else {
                    res.and_then(|_| write!(f, ",\n {}", token))
                }
            });
            res.and_then(|_| write!(f, "]"))
        }
    }
}

pub struct Scanner<'a> {
    source: &'a str,
    start: usize,
    current: usize,
    tokens: Vec<Token>,
    errors: Vec<ScanningError>,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            tokens: vec![],
            errors: vec![],
        }
    }

    fn error_msg(&self) -> String {
        let mut offset = self.start;
        let mut lineno: usize = 1;

        for line in self.source.lines() {
            let linelen = line.len() + 1;
            if offset >= linelen {
                offset -= linelen;
                lineno += 1;
            } else {
                break;
            }
        }

        let lineprefix = format!("{}|", lineno);
        let line = self.source.lines().nth(lineno - 1).unwrap().to_owned();
        let padding = offset + lineprefix.len();
        let indicator = format!("{}^-- Here.", " ".repeat(padding));
        format!("{}{}\n{}", lineprefix, line, indicator)
    }

    fn eof(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        c
    }

    fn peek(&self) -> Option<char> {
        if self.eof() {
            None
        } else {
            Some(self.source.chars().nth(self.current).unwrap())
        }
    }

    fn peek_expect(&mut self, expected: char) -> bool {
        match self.peek() {
            Some(c) if c == expected => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.source.len() {
            None
        } else {
            Some(self.source.chars().nth(self.current + 1).unwrap())
        }
    }

    fn add_token(&mut self, tokentype: TokenType, literal: Option<LiteralType>) {
        let lexeme = match tokentype {
            TokenType::Eof => "",
            _ => &self.source[self.start..self.current],
        }
        .to_owned();
        let offset = match tokentype {
            TokenType::Eof => self.source.len(),
            _ => self.start,
        };

        self.tokens
            .push(Token::new(tokentype, lexeme, offset, literal));
    }

    fn add_token_basic(&mut self, tokentype: TokenType) {
        self.add_token(tokentype, None);
    }

    fn add_token_string(&mut self) {
        let value = self.source[self.start + 1..self.current - 1].to_owned();

        self.add_token(TokenType::String, Some(LiteralType::String(value)));
    }

    fn add_token_number(&mut self) {
        let value = &self.source[self.start..self.current];

        let number: f64 = value
            .parse()
            .unwrap_or_else(|_| panic!("Couldn't parse \"{}\" to f64", value));

        self.add_token(TokenType::Number, Some(LiteralType::Number(number)));
    }

    fn add_token_identifier(&mut self) {
        let value = self.source[self.start..self.current].to_owned();

        self.add_token(TokenType::Identifier, Some(LiteralType::Identifier(value)));
    }

    fn scan_string(&mut self) {
        while let Some(c) = self.peek() {
            if c == '"' {
                break;
            }
            self.advance();
        }

        if self.eof() {
            self.errors
                .push(ScanningError::UnterminatedString(self.error_msg()));
            return;
        }

        self.advance();
        self.add_token_string();
    }

    fn scan_number(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_ascii_digit() {
                break;
            }
            self.advance();
        }

        if let Some('.') = self.peek() {
            match self.peek_next() {
                Some(c) if c.is_ascii_digit() => {
                    self.advance();
                    while let Some(c) = self.peek() {
                        if !c.is_ascii_digit() {
                            break;
                        }
                        self.advance();
                    }
                }
                _ => {}
            }
        }

        self.add_token_number();
    }

    fn scan_identifier(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_alphanumeric() && c != '_' {
                break;
            }
            self.advance();
        }

        match KEYWORDS.get(&self.source[self.start..self.current]) {
            Some(token) => self.add_token_basic(*token),
            None => self.add_token_identifier(),
        }
    }

    fn scan_token(&mut self) {
        match self.advance() {
            '(' => self.add_token_basic(TokenType::LeftParen),
            ')' => self.add_token_basic(TokenType::RightParen),
            '{' => self.add_token_basic(TokenType::LeftBrace),
            '}' => self.add_token_basic(TokenType::RightBrace),
            ',' => self.add_token_basic(TokenType::Comma),
            '.' => self.add_token_basic(TokenType::Dot),
            '-' => self.add_token_basic(TokenType::Minus),
            '+' => self.add_token_basic(TokenType::Plus),
            ';' => self.add_token_basic(TokenType::Semicolon),
            '/' => {
                if self.peek_expect('/') {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                } else {
                    self.add_token_basic(TokenType::Slash);
                }
            }
            '*' => self.add_token_basic(TokenType::Star),
            '!' => {
                if self.peek_expect('=') {
                    self.add_token_basic(TokenType::BangEqual);
                } else {
                    self.add_token_basic(TokenType::Bang);
                }
            }
            '=' => {
                if self.peek_expect('=') {
                    self.add_token_basic(TokenType::EqualEqual);
                } else {
                    self.add_token_basic(TokenType::Equal);
                }
            }
            '>' => {
                if self.peek_expect('=') {
                    self.add_token_basic(TokenType::GreaterEqual);
                } else {
                    self.add_token_basic(TokenType::Greater);
                }
            }
            '<' => {
                if self.peek_expect('=') {
                    self.add_token_basic(TokenType::LessEqual);
                } else {
                    self.add_token_basic(TokenType::Less);
                }
            }
            ' ' | '\r' | '\t' | '\n' => {}
            '"' => self.scan_string(),
            c => {
                if c.is_ascii_digit() {
                    self.scan_number();
                } else if c.is_alphabetic() {
                    self.scan_identifier();
                } else {
                    self.errors
                        .push(ScanningError::UnexpectedChar(c, self.error_msg()));
                }
            }
        }
    }

    pub fn scan_tokens(&mut self) {
        while !self.eof() {
            self.start = self.current;
            self.scan_token();
        }

        self.add_token_basic(TokenType::Eof);
    }
}

pub fn scan_tokens(source: &str) -> Result<Tokens, Vec<ScanningError>> {
    let mut scanner = Scanner::new(source);

    scanner.scan_tokens();

    if !scanner.errors.is_empty() {
        Err(scanner.errors)
    } else {
        Ok(Tokens(scanner.tokens))
    }
}
