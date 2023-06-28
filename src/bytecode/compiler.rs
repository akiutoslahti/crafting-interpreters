use std::{cell::RefCell, collections::HashMap, rc::Rc, u8};

use lazy_static::lazy_static;

#[cfg(feature = "debug_print_code")]
use crate::bytecode::debugger::disassemble_chunk;

use super::{
    chunk::{Chunk, OpCode},
    errors::{LoxError, Reportable},
    scanner::{Scanner, Token, TokenType},
    value::Value,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

lazy_static! {
    static ref NEXT_PRECEDENCE: HashMap<Precedence, Precedence> = {
        let mut next_precedence = HashMap::new();
        next_precedence.insert(Precedence::None, Precedence::Assignment);
        next_precedence.insert(Precedence::Assignment, Precedence::Or);
        next_precedence.insert(Precedence::Or, Precedence::And);
        next_precedence.insert(Precedence::And, Precedence::Equality);
        next_precedence.insert(Precedence::Equality, Precedence::Comparison);
        next_precedence.insert(Precedence::Comparison, Precedence::Term);
        next_precedence.insert(Precedence::Term, Precedence::Factor);
        next_precedence.insert(Precedence::Factor, Precedence::Unary);
        next_precedence.insert(Precedence::Unary, Precedence::Call);
        next_precedence.insert(Precedence::Call, Precedence::Primary);
        next_precedence
    };
}

fn next_precedence(precedence: Precedence) -> Precedence {
    match NEXT_PRECEDENCE.get(&precedence) {
        Some(precedence) => *precedence,
        None => panic!("No next higher precedence for precedence {:?}", precedence),
    }
}

enum ParseFn {
    Number,
    Grouping,
    Unary,
    Binary,
}

struct ParseRule {
    prefix: Option<ParseFn>,
    infix: Option<ParseFn>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(prefix: Option<ParseFn>, infix: Option<ParseFn>, precedence: Precedence) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

impl Default for ParseRule {
    fn default() -> Self {
        Self::new(None, None, Precedence::None)
    }
}

lazy_static! {
    static ref RULES: HashMap<TokenType, ParseRule> = {
        let mut rules = HashMap::new();
        rules.insert(
            TokenType::LeftParen,
            ParseRule::new(Some(ParseFn::Grouping), None, Precedence::None),
        );
        rules.insert(TokenType::RightParen, ParseRule::default());
        rules.insert(TokenType::LeftBrace, ParseRule::default());
        rules.insert(TokenType::RightBrace, ParseRule::default());
        rules.insert(TokenType::Comma, ParseRule::default());
        rules.insert(TokenType::Dot, ParseRule::default());
        rules.insert(
            TokenType::Minus,
            ParseRule::new(
                Some(ParseFn::Unary),
                Some(ParseFn::Binary),
                Precedence::Term,
            ),
        );
        rules.insert(
            TokenType::Plus,
            ParseRule::new(None, Some(ParseFn::Binary), Precedence::Term),
        );
        rules.insert(TokenType::Semicolon, ParseRule::default());
        rules.insert(
            TokenType::Slash,
            ParseRule::new(None, Some(ParseFn::Binary), Precedence::Factor),
        );
        rules.insert(
            TokenType::Star,
            ParseRule::new(None, Some(ParseFn::Binary), Precedence::Factor),
        );
        rules.insert(TokenType::Bang, ParseRule::default());
        rules.insert(TokenType::BangEqual, ParseRule::default());
        rules.insert(TokenType::Equal, ParseRule::default());
        rules.insert(TokenType::EqualEqual, ParseRule::default());
        rules.insert(TokenType::Greater, ParseRule::default());
        rules.insert(TokenType::GreaterEqual, ParseRule::default());
        rules.insert(TokenType::Less, ParseRule::default());
        rules.insert(TokenType::LessEqual, ParseRule::default());
        rules.insert(TokenType::Identifier, ParseRule::default());
        rules.insert(TokenType::String, ParseRule::default());
        rules.insert(
            TokenType::Number,
            ParseRule::new(Some(ParseFn::Number), None, Precedence::None),
        );
        rules.insert(TokenType::And, ParseRule::default());
        rules.insert(TokenType::Class, ParseRule::default());
        rules.insert(TokenType::Else, ParseRule::default());
        rules.insert(TokenType::False, ParseRule::default());
        rules.insert(TokenType::For, ParseRule::default());
        rules.insert(TokenType::Fun, ParseRule::default());
        rules.insert(TokenType::If, ParseRule::default());
        rules.insert(TokenType::Nil, ParseRule::default());
        rules.insert(TokenType::Or, ParseRule::default());
        rules.insert(TokenType::Print, ParseRule::default());
        rules.insert(TokenType::Return, ParseRule::default());
        rules.insert(TokenType::Super, ParseRule::default());
        rules.insert(TokenType::This, ParseRule::default());
        rules.insert(TokenType::True, ParseRule::default());
        rules.insert(TokenType::Var, ParseRule::default());
        rules.insert(TokenType::While, ParseRule::default());
        rules.insert(TokenType::Eof, ParseRule::default());
        rules.insert(TokenType::None, ParseRule::default());
        rules
    };
}

fn get_rule(tokentype: TokenType) -> &'static ParseRule {
    match RULES.get(&tokentype) {
        Some(rule) => rule,
        None => panic!("No ParseRule for tokentype {:?}", tokentype),
    }
}

struct Parser {
    current: Token,
    previous: Token,
    error: bool,
    panic: bool,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            current: Token::default(),
            previous: Token::default(),
            error: false,
            panic: false,
        }
    }
}

pub struct Compiler {
    scanner: Scanner,
    parser: Parser,
    chunk: Rc<RefCell<Chunk>>,
}

impl Compiler {
    pub fn new(src: &str, chunk: Rc<RefCell<Chunk>>) -> Self {
        Self {
            scanner: Scanner::new(src),
            parser: Parser::new(),
            chunk,
        }
    }

    fn error(&mut self, error: &dyn Reportable) {
        if self.parser.panic {
            return;
        }
        self.parser.panic = true;
        error.report(&self.scanner.src);
        self.parser.error = true;
    }

    fn advance(&mut self) {
        self.parser.previous = self.parser.current;

        loop {
            match self.scanner.scan_token() {
                Ok(token) => {
                    self.parser.current = token;
                    break;
                }
                Err(error) => self.error(&error),
            };
        }
    }

    fn consume(&mut self, tokentype: TokenType, msg: &str) {
        if self.parser.current.tokentype == tokentype {
            self.advance();
            return;
        }

        self.error(&LoxError::new(
            msg.to_string(),
            Some(self.parser.current.offset),
            Some(self.parser.current.length),
            self.parser.current.line,
        ))
    }

    fn emit_op(&mut self, opcode: OpCode) {
        self.chunk
            .borrow_mut()
            .write_opcode(opcode, self.parser.previous.line)
    }

    fn emit_op_single_operand(&mut self, opcode: OpCode, operand: u8) {
        self.emit_op(opcode);
        self.chunk
            .borrow_mut()
            .write_chunk(operand, self.parser.previous.line)
    }

    fn emit_constant(&mut self, val: f64) {
        let constant = self.chunk.borrow_mut().add_constant(val);
        if constant > u8::MAX as usize {
            self.error(&LoxError::new(
                "Too many constants in one chunk.".to_string(),
                Some(self.parser.current.offset),
                Some(self.parser.current.length),
                self.parser.current.line,
            ));
        } else {
            self.emit_op_single_operand(OpCode::Constant, constant as u8);
        }
    }

    pub fn compile(&mut self) -> Result<(), ()> {
        self.advance();
        self.expression();

        self.consume(TokenType::Eof, "Expect end of expression.");
        self.end_compiler();

        if self.parser.error {
            return Err(());
        }
        Ok(())
    }

    fn end_compiler(&mut self) {
        #[cfg(feature = "debug_print_code")]
        if !self.parser.error {
            disassemble_chunk(&self.chunk.borrow(), "code");
        }
        self.emit_op(OpCode::Return);
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let token = self.parser.previous;
        let val = self.scanner.src[token.offset..token.offset + token.length]
            .iter()
            .collect::<String>();
        let val: Value = val
            .parse()
            .unwrap_or_else(|_| panic!("Couldn't parse \"{}\" to f64", val));
        self.emit_constant(val);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator = self.parser.previous.tokentype;

        self.parse_precedence(Precedence::Unary);

        match operator {
            TokenType::Minus => self.emit_op(OpCode::Negate),
            _ => todo!(),
        }
    }

    fn binary(&mut self) {
        let operator = self.parser.previous.tokentype;
        let rule = get_rule(operator);
        self.parse_precedence(next_precedence(rule.precedence));

        match operator {
            TokenType::Plus => self.emit_op(OpCode::Add),
            TokenType::Minus => self.emit_op(OpCode::Sub),
            TokenType::Star => self.emit_op(OpCode::Mul),
            TokenType::Slash => self.emit_op(OpCode::Div),
            _ => todo!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        match &get_rule(self.parser.previous.tokentype).prefix {
            Some(prefixfn) => {
                self.apply_parsefn(prefixfn);
                while precedence <= get_rule(self.parser.current.tokentype).precedence {
                    self.advance();
                    if let Some(infixfn) = &get_rule(self.parser.previous.tokentype).infix {
                        self.apply_parsefn(infixfn);
                    }
                }
            }
            None => {
                self.error(&LoxError::new(
                    "Expect expression.".to_string(),
                    Some(self.parser.current.offset),
                    Some(self.parser.current.length),
                    self.parser.current.line,
                ));
            }
        }
    }

    fn apply_parsefn(&mut self, parsefn: &ParseFn) {
        match parsefn {
            ParseFn::Number => self.number(),
            ParseFn::Grouping => self.grouping(),
            ParseFn::Unary => self.unary(),
            ParseFn::Binary => self.binary(),
        }
    }
}
