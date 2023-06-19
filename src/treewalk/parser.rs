use super::{
    ast::{
        BinaryOp, BinaryOpType, Expr, Literal, LogicalOp, LogicalOpType, Stmt, UnaryOp,
        UnaryOpType, Variable,
    },
    scanner::{LiteralType, Token, TokenType},
};
use lazy_static::lazy_static;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

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
    static ref LOGICALOPS: HashMap<TokenType, LogicalOpType> = {
        let mut ops = HashMap::new();
        ops.insert(TokenType::And, LogicalOpType::And);
        ops.insert(TokenType::Or, LogicalOpType::Or);
        ops
    };
}

#[derive(Debug)]
pub enum ParsingError {
    MissingExpression(Token, String),
    UnmetExpectation(Token, TokenType, String),
    InvalidAssignmentTarget(String),
    TooManyArguments(String),
    TooManyParameters(String),
}

impl PartialEq for ParsingError {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (
                ParsingError::MissingExpression(..),
                ParsingError::MissingExpression(..)
            ) | (
                ParsingError::UnmetExpectation(..),
                ParsingError::UnmetExpectation(..)
            ) | (
                ParsingError::InvalidAssignmentTarget(..),
                ParsingError::InvalidAssignmentTarget(..)
            ) | (
                ParsingError::TooManyArguments(..),
                ParsingError::TooManyArguments(..)
            ) | (
                ParsingError::TooManyParameters(..),
                ParsingError::TooManyParameters(..)
            )
        )
    }
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
            ParsingError::MissingExpression(token, msg) => write!(
                f,
                "{} Expected expression, got token: {:?} ({:?}).\n\n{}",
                PREFIX,
                token.tokentype,
                token.lexeme,
                pad_msg(msg)
            ),
            ParsingError::UnmetExpectation(token, expected, msg) => write!(
                f,
                "{} Expected token: {:?}, got token: {:?}.\n\n{}",
                PREFIX,
                expected,
                token.tokentype,
                pad_msg(msg)
            ),
            ParsingError::InvalidAssignmentTarget(msg) => write!(
                f,
                "{} Invalid assignment target.\n\n{}",
                PREFIX,
                pad_msg(msg)
            ),
            ParsingError::TooManyArguments(msg) => write!(
                f,
                "{} Too many (> 255) arguments for a function call.\n\n{}",
                PREFIX,
                pad_msg(msg)
            ),
            ParsingError::TooManyParameters(msg) => write!(
                f,
                "{} Too many (> 255) parameters for a function.\n\n{}",
                PREFIX,
                pad_msg(msg)
            ),
        }
    }
}

pub struct Parser<'a> {
    source: &'a str,
    tokens: Vec<Token>,
    current: usize,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str, tokens: Vec<Token>) -> Self {
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

    fn check(&self, tokentype: TokenType) -> Option<bool> {
        if self.eof() {
            return None;
        }

        Some(self.peek().tokentype == tokentype)
    }

    fn match_next(&mut self, tokens: Vec<TokenType>) -> bool {
        for token in tokens {
            if self.check(token).is_some_and(|b| b) {
                self.advance();
                return true;
            }
        }

        false
    }

    fn consume(&mut self, tokentype: TokenType) -> Result<&Token, ParsingError> {
        if self.check(tokentype).is_some_and(|b| b) {
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
        let expr = self.or()?;

        if self.match_next(vec![TokenType::Equal]) {
            let token = self.previous().clone();
            let value = self.assignment()?;

            if let Expr::Variable(var) = expr {
                return Ok(Expr::Assign {
                    var,
                    value: Box::new(value),
                });
            } else if let Expr::Get { object, property } = expr {
                return Ok(Expr::Set {
                    object,
                    property,
                    value: Box::new(value),
                });
            } else {
                return Err(ParsingError::InvalidAssignmentTarget(
                    self.error_msg(&token),
                ));
            }
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.and()?;

        if self.match_next(vec![TokenType::Or]) {
            let op = token_to_logicalop(self.previous());
            let rhs = Box::new(self.and()?);
            let lhs = Box::new(expr);
            expr = Expr::Logical { op, lhs, rhs }
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.equality()?;

        if self.match_next(vec![TokenType::And]) {
            let op = token_to_logicalop(self.previous());
            let rhs = Box::new(self.equality()?);
            let lhs = Box::new(expr);
            expr = Expr::Logical { op, lhs, rhs }
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
            self.call()
        }
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParsingError> {
        let mut arguments = vec![];
        if !self.check(TokenType::RightParen).is_some_and(|b| b) {
            loop {
                if arguments.len() >= 255 {
                    let token = self.peek();
                    return Err(ParsingError::TooManyArguments(self.error_msg(token)));
                }
                arguments.push(self.expression()?);
                if !self.match_next(vec![TokenType::Comma]) {
                    break;
                }
            }
        }

        let paren = self.consume(TokenType::RightParen)?;

        Ok(Expr::Call {
            callee: Box::new(callee),
            paren: paren.offset,
            arguments,
        })
    }

    fn call(&mut self) -> Result<Expr, ParsingError> {
        let mut expr = self.primary()?;

        loop {
            if self.match_next(vec![TokenType::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_next(vec![TokenType::Dot]) {
                let property = self.consume(TokenType::Identifier)?;
                let name = if let Some(LiteralType::Identifier(name)) = &property.literal {
                    name.clone()
                } else {
                    panic!(
                        "Mismatch between literal ({:?}) and token type ({:?})",
                        property.literal, property.tokentype
                    );
                };
                expr = Expr::Get {
                    object: Box::new(expr),
                    property: Variable {
                        name,
                        offset: property.offset,
                    },
                }
            } else {
                break;
            }
        }

        Ok(expr)
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

        if self.match_next(vec![TokenType::Super]) {
            let offset = self.previous().offset;
            self.consume(TokenType::Dot)?;
            let token = self.consume(TokenType::Identifier)?;
            let name = if let Some(LiteralType::Identifier(s)) = &token.literal {
                s.clone()
            } else {
                panic!(
                    "Mismatch between literal ({:?}) and token type ({:?})",
                    token.literal, token.tokentype
                );
            };
            return Ok(Expr::Super {
                offset,
                method: Variable {
                    name,
                    offset: token.offset,
                },
            });
        }

        if self.match_next(vec![TokenType::This]) {
            return Ok(Expr::This(self.previous().offset));
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

            return Ok(Expr::Variable(Variable {
                name,
                offset: token.offset,
            }));
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

    fn block_statement(&mut self) -> Result<Vec<Stmt>, ParsingError> {
        let mut statements: Vec<Stmt> = vec![];

        while !self.check(TokenType::RightBrace).is_some_and(|b| b) {
            statements.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace)?;

        Ok(statements)
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

    fn while_statement(&mut self) -> Result<Stmt, ParsingError> {
        self.consume(TokenType::LeftParen)?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen)?;

        let body = Box::new(self.statement()?);
        Ok(Stmt::While { condition, body })
    }

    fn for_statement(&mut self) -> Result<Stmt, ParsingError> {
        self.consume(TokenType::LeftParen)?;
        let initializer = if self.match_next(vec![TokenType::Semicolon]) {
            None
        } else if self.match_next(vec![TokenType::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if self.check(TokenType::Semicolon).is_some_and(|b| b) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::Semicolon)?;

        let increment = if self.check(TokenType::RightParen).is_some_and(|b| b) {
            None
        } else {
            Some(self.expression()?)
        };
        self.consume(TokenType::RightParen)?;

        let mut body = self.statement()?;

        if let Some(increment) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
        }

        body = match condition {
            Some(expr) => Stmt::While {
                condition: expr,
                body: Box::new(body),
            },
            None => Stmt::While {
                condition: Expr::Literal(Literal::True),
                body: Box::new(body),
            },
        };

        if let Some(initializer) = initializer {
            body = Stmt::Block(vec![initializer, body]);
        }

        Ok(body)
    }

    fn function_statement(&mut self) -> Result<Stmt, ParsingError> {
        let token = self.consume(TokenType::Identifier)?;
        let name = if let Some(LiteralType::Identifier(s)) = &token.literal {
            s.clone()
        } else {
            panic!(
                "Mismatch between literal ({:?}) and token type ({:?})",
                token.literal, token.tokentype
            );
        };
        let offset = token.offset;
        self.consume(TokenType::LeftParen)?;

        let mut parameters: Vec<Variable> = vec![];
        if !self.check(TokenType::RightParen).is_some_and(|b| b) {
            loop {
                if parameters.len() >= 255 {
                    let token = self.peek();
                    return Err(ParsingError::TooManyParameters(self.error_msg(token)));
                }

                let token = self.consume(TokenType::Identifier)?;
                let name = if let Some(LiteralType::Identifier(s)) = &token.literal {
                    s.clone()
                } else {
                    panic!(
                        "Mismatch between literal ({:?}) and token type ({:?})",
                        token.literal, token.tokentype
                    );
                };
                parameters.push(Variable {
                    name,
                    offset: token.offset,
                });

                if !self.match_next(vec![TokenType::Comma]) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen)?;

        self.consume(TokenType::LeftBrace)?;
        let body = self.block_statement()?;

        Ok(Stmt::Function {
            var: Variable { name, offset },
            parameters,
            body,
        })
    }

    fn return_statement(&mut self, offset: usize) -> Result<Stmt, ParsingError> {
        let expr = if !self.check(TokenType::Semicolon).is_some_and(|b| b) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon)?;

        Ok(Stmt::Return { expr, offset })
    }

    fn class_statement(&mut self) -> Result<Stmt, ParsingError> {
        let token = self.consume(TokenType::Identifier)?;
        let name = if let Some(LiteralType::Identifier(s)) = &token.literal {
            s.clone()
        } else {
            panic!(
                "Mismatch between literal ({:?}) and token type ({:?})",
                token.literal, token.tokentype
            );
        };
        let var = Variable {
            name,
            offset: token.offset,
        };

        let superclass = if self.match_next(vec![TokenType::Less]) {
            let token = self.consume(TokenType::Identifier)?;
            let name = if let Some(LiteralType::Identifier(s)) = &token.literal {
                s.clone()
            } else {
                panic!(
                    "Mismatch between literal ({:?}) and token type ({:?})",
                    token.literal, token.tokentype
                );
            };
            Some(Variable {
                name,
                offset: token.offset,
            })
        } else {
            None
        };

        let mut methods: Vec<Stmt> = vec![];
        self.consume(TokenType::LeftBrace)?;
        while !self.check(TokenType::RightBrace).is_some_and(|b| b) {
            methods.push(self.function_statement()?)
        }
        self.consume(TokenType::RightBrace)?;

        Ok(Stmt::Class {
            var,
            superclass,
            methods,
        })
    }

    fn statement(&mut self) -> Result<Stmt, ParsingError> {
        if self.match_next(vec![TokenType::LeftBrace]) {
            return Ok(Stmt::Block(self.block_statement()?));
        }
        if self.match_next(vec![TokenType::Print]) {
            return self.print_statement();
        }
        if self.match_next(vec![TokenType::If]) {
            return self.if_statement();
        }
        if self.match_next(vec![TokenType::While]) {
            return self.while_statement();
        }
        if self.match_next(vec![TokenType::For]) {
            return self.for_statement();
        }
        if self.match_next(vec![TokenType::Fun]) {
            return self.function_statement();
        }
        if self.match_next(vec![TokenType::Return]) {
            return self.return_statement(self.previous().offset);
        }
        if self.match_next(vec![TokenType::Class]) {
            return self.class_statement();
        }

        self.expression_statement()
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParsingError> {
        let token = self.consume(TokenType::Identifier)?;
        let offset = token.offset;
        let name = if let Some(LiteralType::Identifier(s)) = &token.literal {
            s.clone()
        } else {
            panic!(
                "Mismatch between literal ({:?}) and token type ({:?})",
                token.literal, token.tokentype
            );
        };

        let initializer = if self.match_next(vec![TokenType::Equal]) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(TokenType::Semicolon)?;

        Ok(Stmt::Var {
            var: Variable { name, offset },
            initializer,
        })
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

    fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParsingError>> {
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

pub fn parse_statements(src: &str, tokens: Vec<Token>) -> Result<Vec<Stmt>, Vec<ParsingError>> {
    let mut parser = Parser::new(src, tokens);
    parser.parse()
}

fn token_to_logicalop(token: &Token) -> LogicalOp {
    match LOGICALOPS.get(&token.tokentype) {
        Some(op) => LogicalOp::new(*op, token.offset),
        None => panic!(
            "Invalid tokentype ({:?}) for logical operation",
            token.tokentype
        ),
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

#[cfg(test)]
mod tests {
    use crate::treewalk::{
        ast::Stmt,
        scanner::{scan_tokens, Token, TokenType},
    };

    use super::{parse_statements, ParsingError};

    fn parse(src: &str) -> Result<Vec<Stmt>, Vec<ParsingError>> {
        let tokens = scan_tokens(src).unwrap();
        parse_statements(src, tokens)
    }

    fn check_parsing_error(src: &str, kind: ParsingError) {
        let res = parse(src);
        assert!(res.is_err());
        if let Some(errors) = res.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(errors[0], kind);
        }
    }

    #[test]
    fn test_missing_expression() {
        check_parsing_error(
            "var foo =",
            ParsingError::MissingExpression(Token::default(), "".to_string()),
        )
    }

    #[test]
    fn test_unmet_expectation() {
        check_parsing_error(
            "var 1 = 2;",
            ParsingError::UnmetExpectation(Token::default(), TokenType::Eof, "".to_string()),
        )
    }

    #[test]
    fn test_invalid_assignment() {
        check_parsing_error(
            "1 = 2;",
            ParsingError::InvalidAssignmentTarget("".to_string()),
        )
    }

    #[test]
    fn test_too_many_parameters() {
        let mut params = "param1".to_string();
        for n in 2..=256 {
            params = format!("{}, param{}", params, n);
        }
        let src = format!("fun foo({}) {{}}", params);

        check_parsing_error(&src, ParsingError::TooManyParameters("".to_string()));
    }

    #[test]
    fn test_too_many_arguments() {
        let mut args = "arg1".to_string();
        for n in 2..=256 {
            args = format!("{}, arg{}", args, n);
        }
        let src = format!("foo({})", args);

        check_parsing_error(&src, ParsingError::TooManyArguments("".to_string()));
    }
}
