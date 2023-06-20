#[derive(Debug, PartialEq)]
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

    // One or two character tokens
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
    For,
    Fun,
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

pub struct Token {
    pub tokentype: TokenType,
    pub offset: usize,
    pub length: usize,
    pub line: usize,
}

impl Token {
    fn new(tokentype: TokenType, offset: usize, length: usize, line: usize) -> Self {
        Self {
            tokentype,
            offset,
            length,
            line,
        }
    }
}

pub struct ScanningError(pub String, pub usize);

pub struct Scanner {
    src: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
}

impl Scanner {
    pub fn new(src: &str) -> Self {
        Self {
            src: src.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_token(&mut self) -> Result<Token, ScanningError> {
        while self.current < self.src.len() {
            match self.peek() {
                ' ' | '\r' | '\t' => self.current += 1,
                '\n' => {
                    self.line += 1;
                    self.current += 1;
                }
                '/' => {
                    if let Some(c) = self.peek_next() {
                        if c == '/' {
                            while self.peek() != '\n' && self.current < self.src.len() {
                                self.current += 1;
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }

        self.start = self.current;

        macro_rules! make_token {
            ( $tokentype: expr) => {
                Ok(Token::new(
                    $tokentype,
                    self.start,
                    self.current - self.start,
                    self.line,
                ))
            };
        }

        if self.current >= self.src.len() {
            return make_token!(TokenType::Eof);
        }

        match self.advance() {
            '(' => make_token!(TokenType::RightParen),
            ')' => make_token!(TokenType::LeftParen),
            '{' => make_token!(TokenType::RightBrace),
            '}' => make_token!(TokenType::LeftBrace),
            ';' => make_token!(TokenType::Semicolon),
            ',' => make_token!(TokenType::Comma),
            '.' => make_token!(TokenType::Dot),
            '-' => make_token!(TokenType::Minus),
            '+' => make_token!(TokenType::Plus),
            '/' => make_token!(TokenType::Slash),
            '*' => make_token!(TokenType::Star),
            '!' => make_token!(if self.match_next('=') {
                TokenType::BangEqual
            } else {
                TokenType::Bang
            }),
            '=' => make_token!(if self.match_next('=') {
                TokenType::EqualEqual
            } else {
                TokenType::Equal
            }),
            '<' => make_token!(if self.match_next('=') {
                TokenType::LessEqual
            } else {
                TokenType::Less
            }),
            '>' => make_token!(if self.match_next('=') {
                TokenType::GreaterEqual
            } else {
                TokenType::Greater
            }),
            '"' => {
                while self.peek() != '"' && self.current < self.src.len() {
                    if self.peek() == '\n' {
                        self.line += 1;
                    }
                    self.current += 1;
                }

                if self.current >= self.src.len() {
                    Err(ScanningError("Unterminated string.".to_string(), self.line))
                } else {
                    self.current += 1;
                    make_token!(TokenType::String)
                }
            }
            c if c.is_ascii_digit() => {
                while self.peek().is_ascii_digit() {
                    self.current += 1;
                }

                if self.peek() == '.' {
                    if let Some(c) = self.peek_next() {
                        if c.is_ascii_digit() {
                            self.current += 1;

                            while self.peek().is_ascii_digit() {
                                self.current += 1;
                            }
                        }
                    }
                }

                make_token!(TokenType::Number)
            }
            c if c.is_alphabetic() => {
                while self.peek().is_alphanumeric() {
                    self.current += 1;
                }
                make_token!(self.identifier_type())
            }
            _ => Err(ScanningError(
                "Unexpected character.".to_string(),
                self.line,
            )),
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.src[self.current - 1]
    }

    fn peek(&self) -> char {
        self.src[self.current]
    }

    fn peek_next(&self) -> Option<char> {
        if self.current + 1 >= self.src.len() {
            return None;
        }
        Some(self.src[self.current + 1])
    }

    fn match_next(&mut self, expected: char) -> bool {
        if self.current >= self.src.len() {
            return false;
        }
        if self.src[self.current] != expected {
            return false;
        }
        self.current += 1;
        true
    }

    fn identifier_type(&self) -> TokenType {
        match self.src[self.start] {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'f' if self.current - self.start > 1 => match self.src[self.start + 1] {
                'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                'o' => self.check_keyword(2, 1, "r", TokenType::For),
                'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                _ => TokenType::Identifier,
            },
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' if self.current - self.start > 1 => match self.src[self.start + 1] {
                'h' => self.check_keyword(2, 2, "is", TokenType::This),
                'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                _ => TokenType::Identifier,
            },
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    fn check_keyword(
        &self,
        start: usize,
        length: usize,
        rest: &str,
        tokentype: TokenType,
    ) -> TokenType {
        if self.current - self.start == start + length
            && self.src[self.start + start..self.start + start + length]
                .iter()
                .collect::<String>()
                == rest
        {
            return tokentype;
        }
        TokenType::Identifier
    }
}
