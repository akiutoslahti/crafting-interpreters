use crate::{
    ast::{BinaryOp, BinaryOpType, Expr, Literal, Stmt, UnaryOp, UnaryOpType},
    scanner::{LiteralType, Token, TokenType},
};
use lazy_static::lazy_static;
use std::{collections::HashMap, fmt::Display};

lazy_static! {
    static ref BINARYOPS: HashMap<TokenType, BinaryOpType> = {
        let mut ops = HashMap::new();
        ops.insert(TokenType::EqualEqual, BinaryOpType::Equal);
        ops.insert(TokenType::BangEqual, BinaryOpType::NotEqual);
        ops.insert(TokenType::Less, BinaryOpType::Less);
        ops.insert(TokenType::LessEqual, BinaryOpType::LessEqual);
        ops.insert(TokenType::Greater, BinaryOpType::Greater);
        ops.insert(TokenType::GreaterEqual, BinaryOpType::GreaterEqual);
        ops.insert(TokenType::Plus, BinaryOpType::Add);
        ops.insert(TokenType::Minus, BinaryOpType::Sub);
        ops.insert(TokenType::Star, BinaryOpType::Mul);
        ops.insert(TokenType::Slash, BinaryOpType::Div);
        ops
    };
    static ref UNARYOPS: HashMap<TokenType, UnaryOpType> = {
        let mut ops = HashMap::new();
        ops.insert(TokenType::Minus, UnaryOpType::Negate);
        ops.insert(TokenType::Bang, UnaryOpType::Not);
        ops
    };
}

#[allow(clippy::enum_variant_names)]
pub enum ParsingError {
    MissingExpression(Token, String),
    UnmetExpectation(Token, TokenType, String),
    InvalidAssignmentTarget(String),
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const PREFIX: &str = "Parsing error!";
        const PADDING: usize = 4;
        let pad_msg = |msg: &str| -> String {
            msg.lines()
                .map(|line| format!("{}{}\n", " ".repeat(PADDING), line))
                .collect()
        };
        match self {
            ParsingError::MissingExpression(token, msg) => {
                write!(
                    f,
                    "{} Expected expression, got token: {:?} ({:?}).\n\n{}",
                    PREFIX,
                    token.tokentype,
                    token.lexeme,
                    pad_msg(msg)
                )
            }
            ParsingError::UnmetExpectation(token, expected, msg) => {
                write!(
                    f,
                    "{} Expected token: {:?}, got token: {:?}.\n\n{}",
                    PREFIX,
                    expected,
                    token.tokentype,
                    pad_msg(msg)
                )
            }
            ParsingError::InvalidAssignmentTarget(msg) => {
                write!(
                    f,
                    "{} Invalid assignment target.\n\n{}",
                    PREFIX,
                    pad_msg(msg)
                )
            }
        }
    }
}

pub struct Parser<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            current: 0,
        }
    }

    fn error_msg(&self, token: &Token) -> String {
        if token.tokentype == TokenType::Eof {
            let lineno = self.source.lines().count();
            let lineprefix = format!("{}|", lineno);
            let line = self.source.lines().nth(lineno - 1).unwrap();
            let padding = lineprefix.len() + line.len();
            let indicator = format!("{}^-- Here.", " ".repeat(padding));
            return format!("{}{}\n{}", lineprefix, line, indicator);
        }

        let mut offset = token.offset;
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
        let line = self.source.lines().nth(lineno - 1).unwrap();
        let padding = offset + lineprefix.len();
        let indicator = format!("{}^-- Here.", " ".repeat(padding));
        format!("{}{}\n{}", lineprefix, line, indicator)
    }

    fn eof(&self) -> bool {
        self.tokens[self.current].tokentype == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        if !self.eof() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, tokentype: TokenType) -> bool {
        if self.eof() {
            return false;
        }

        self.peek().tokentype == tokentype
    }

    fn match_next(&mut self, tokens: Vec<TokenType>) -> bool {
        for token in tokens {
            if self.check(token) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn consume(&mut self, tokentype: TokenType) -> Result<&Token, ParsingError> {
        if self.check(tokentype) {
            return Ok(self.advance());
        }

        let token = self.peek();
        Err(ParsingError::UnmetExpectation(
            token.clone(),
            tokentype,
            self.error_msg(token),
        ))
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.eof() {
            if self.previous().tokentype == TokenType::Semicolon {
                return;
            }

            match self.peek().tokentype {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => {}
            }

            self.advance();
        }
    }

    fn expression(&mut self) -> Result<Expr, ParsingError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParsingError> {
        let expr = self.equality()?;

        if self.match_next(vec![TokenType::Equal]) {
            let token = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable { name, offset } = expr {
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(value),
                    offset,
                });
            } else {
                return Err(ParsingError::InvalidAssignmentTarget(
                    self.error_msg(&token),
                ));
            }
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.comparison()?;

        while self.match_next(vec![TokenType::BangEqual, TokenType::EqualEqual]) {
            let op = token_to_binaryop(self.previous());
            let rhs = Box::new(self.comparison()?);
            let lhs = Box::new(expr);
            expr = Expr::Binary { op, lhs, rhs };
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.term()?;

        while self.match_next(vec![
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let op = token_to_binaryop(self.previous());
            let rhs = Box::new(self.term()?);
            let lhs = Box::new(expr);
            expr = Expr::Binary { op, lhs, rhs };
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.factor()?;

        while self.match_next(vec![TokenType::Plus, TokenType::Minus]) {
            let op = token_to_binaryop(self.previous());
            let rhs = Box::new(self.factor()?);
            let lhs = Box::new(expr);
            expr = Expr::Binary { op, lhs, rhs };
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.unary()?;

        while self.match_next(vec![TokenType::Star, TokenType::Slash]) {
            let op = token_to_binaryop(self.previous());
            let rhs = Box::new(self.unary()?);
            let lhs = Box::new(expr);
            expr = Expr::Binary { op, lhs, rhs };
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParsingError> {
        if self.match_next(vec![TokenType::Minus, TokenType::Bang]) {
            let op = token_to_unaryop(self.previous());
            let rhs = Box::new(self.unary()?);
            Ok(Expr::Unary { op, rhs })
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> Result<Expr, ParsingError> {
        if self.match_next(vec![
            TokenType::Number,
            TokenType::String,
            TokenType::False,
            TokenType::True,
            TokenType::Nil,
        ]) {
            let literal = token_to_literal(self.previous())?;
            return Ok(Expr::Literal(literal));
        }

        if self.match_next(vec![TokenType::Identifier]) {
            let token = self.previous();

            let name;
            if let Some(LiteralType::Identifier(s)) = &token.literal {
                name = s.clone();
            } else {
                return Err(ParsingError::UnmetExpectation(
                    token.clone(),
                    TokenType::Identifier,
                    self.error_msg(token),
                ));
            }

            return Ok(Expr::Variable {
                name,
                offset: token.offset,
            });
        }

        if self.match_next(vec![TokenType::LeftParen]) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen)?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        let token = self.peek();
        Err(ParsingError::MissingExpression(
            token.clone(),
            self.error_msg(token),
        ))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParsingError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Expression(expr))
    }

    fn print_statement(&mut self) -> Result<Stmt, ParsingError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon)?;
        Ok(Stmt::Print(expr))
    }

    fn block_statement(&mut self) -> Result<Stmt, ParsingError> {
        let mut statements: Vec<Stmt> = vec![];

        while !self.check(TokenType::RightBrace) {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace)?;

        Ok(Stmt::Block(statements))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParsingError> {
        self.consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen)?;

        let then_branch = Box::new(self.statement()?);
        let mut else_branch = None;
        if self.match_next(vec![TokenType::Else]) {
            else_branch = Some(Box::new(self.statement()?));
        }

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn statement(&mut self) -> Result<Stmt, ParsingError> {
        if self.match_next(vec![TokenType::LeftBrace]) {
            return self.block_statement();
        }
        if self.match_next(vec![TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_next(vec![TokenType::If]) {
            return self.if_statement();
        }

        self.expression_statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParsingError> {
        let name = if let Some(LiteralType::Identifier(s)) =
            &self.consume(TokenType::Identifier)?.literal
        {
            s.clone()
        } else {
            panic!("Unexpected error that shouldn't be possible.")
        };

        let initializer = if self.match_next(vec![TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon)?;

        Ok(Stmt::Var { name, initializer })
    }

    fn declaration(&mut self) -> Result<Stmt, ParsingError> {
        let stmt = if self.match_next(vec![TokenType::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };

        if stmt.is_err() {
            self.synchronize();
        }

        stmt
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParsingError>> {
        let mut statements = vec![];
        let mut errors = vec![];

        while !self.eof() {
            match self.declaration() {
                Ok(stmt) => statements.push(stmt),
                Err(err) => errors.push(err),
            }
        }

        if !errors.is_empty() {
            Err(errors)
        } else {
            Ok(statements)
        }
    }
}

fn token_to_binaryop(token: &Token) -> BinaryOp {
    match BINARYOPS.get(&token.tokentype) {
        Some(op) => BinaryOp::new(*op, token.offset),
        None => panic!(
            "Invalid tokentype ({:?}) for binary operation",
            token.tokentype
        ),
    }
}

fn token_to_unaryop(token: &Token) -> UnaryOp {
    match UNARYOPS.get(&token.tokentype) {
        Some(op) => UnaryOp::new(*op, token.offset),
        None => panic!(
            "Invalid tokentype ({:?}) for unary operation",
            token.tokentype
        ),
    }
}

fn token_to_literal(token: &Token) -> Result<Literal, ParsingError> {
    match token.tokentype {
        TokenType::Number => match &token.literal {
            Some(LiteralType::Number(n)) => Ok(Literal::Number(*n)),
            _ => panic!(
                "Mismatch between literal ({:?}) and token type ({:?})",
                token.literal, token.tokentype
            ),
        },
        TokenType::String => match &token.literal {
            Some(LiteralType::String(s)) => Ok(Literal::String(s.clone())),
            _ => panic!(
                "Mismatch between literal ({:?}) and token type ({:?})",
                token.literal, token.tokentype
            ),
        },
        TokenType::False => Ok(Literal::False),
        TokenType::True => Ok(Literal::True),
        TokenType::Nil => Ok(Literal::Nil),
        _ => panic!("Invalid tokentype ({:?}) for literal", token.tokentype),
    }
}
