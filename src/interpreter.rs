use crate::ast::{BinaryOp, BinaryOpType, Expr, Literal, UnaryOp, UnaryOpType};
use std::{f64::EPSILON, fmt::Display};

pub enum InterpreterError {
    InvalidUnaryOperand(UnaryOpType, Value, String),
    InvalidBinaryOperands(BinaryOpType, Value, Value, String),
    DivisionByZero(String),
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const PREFIX: &str = "Interpreter error!";
        const PADDING: usize = 4;
        let pad_msg = |msg: &str| -> String {
            msg.lines()
                .map(|line| format!("{}{}\n", " ".repeat(PADDING), line))
                .collect()
        };
        match self {
            InterpreterError::InvalidUnaryOperand(op, rhs, msg) => write!(
                f,
                "{} Invalid operand ({}) for unary expression ({}).\n\n{}",
                PREFIX,
                rhs,
                op,
                pad_msg(msg)
            ),
            InterpreterError::InvalidBinaryOperands(op, lhs, rhs, msg) => write!(
                f,
                "{} Invalid operands ({}, {}) for binary expression ({}).\n\n{}",
                PREFIX,
                lhs,
                rhs,
                op,
                pad_msg(msg)
            ),
            InterpreterError::DivisionByZero(msg) => {
                write!(f, "{} Division by zero.\n\n{}", PREFIX, pad_msg(msg))
            }
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Bool(true) => write!(f, "true"),
            Value::Bool(false) => write!(f, "false"),
            Value::Nil => write!(f, "nil"),
        }
    }
}
struct Interpreter<'a> {
    source: &'a str,
}

impl<'a> Interpreter<'a> {
    fn new(source: &'a str) -> Self {
        Self { source }
    }

    fn error_msg(&self, offset: usize) -> String {
        let mut offset = offset;
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

    fn expression(&self, expr: Expr) -> Result<Value, InterpreterError> {
        match expr {
            Expr::Literal(l) => Ok(self.literal(l)),
            Expr::Unary { op, rhs } => self.unary(op, *rhs),
            Expr::Binary { lhs, op, rhs } => self.binary(*lhs, op, *rhs),
            Expr::Grouping(expr) => self.expression(*expr),
        }
    }

    fn literal(&self, literal: Literal) -> Value {
        match literal {
            Literal::Number(n) => Value::Number(n),
            Literal::String(s) => Value::String(s),
            Literal::False => Value::Bool(false),
            Literal::True => Value::Bool(true),
            Literal::Nil => Value::Nil,
        }
    }

    fn unary(&self, op: UnaryOp, rhs: Expr) -> Result<Value, InterpreterError> {
        let rhs = self.expression(rhs)?;

        match (op.optype, rhs.clone()) {
            (UnaryOpType::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOpType::Not, val) => Ok(Value::Bool(!is_truthy(val))),
            _ => Err(InterpreterError::InvalidUnaryOperand(
                op.optype,
                rhs,
                self.error_msg(op.offset),
            )),
        }
    }

    fn binary(&self, lhs: Expr, op: BinaryOp, rhs: Expr) -> Result<Value, InterpreterError> {
        let lhs = self.expression(lhs)?;
        let rhs = self.expression(rhs)?;

        match (op.optype, lhs.clone(), rhs.clone()) {
            (BinaryOpType::Equal, a, b) => Ok(Value::Bool(is_equal(a, b))),
            (BinaryOpType::NotEqual, a, b) => Ok(Value::Bool(!is_equal(a, b))),
            (BinaryOpType::Less, Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a < b)),
            (BinaryOpType::LessEqual, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a <= b))
            }
            (BinaryOpType::Greater, Value::Number(a), Value::Number(b)) => Ok(Value::Bool(a > b)),
            (BinaryOpType::GreaterEqual, Value::Number(a), Value::Number(b)) => {
                Ok(Value::Bool(a >= b))
            }
            (BinaryOpType::Add, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
            (BinaryOpType::Sub, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
            (BinaryOpType::Mul, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
            (BinaryOpType::Div, _, Value::Number(b)) if b == 0.0 => {
                Err(InterpreterError::DivisionByZero(self.error_msg(op.offset)))
            }
            (BinaryOpType::Div, Value::Number(a), Value::Number(b)) => Ok(Value::Number(a / b)),
            (BinaryOpType::Add, Value::String(a), Value::String(b)) => {
                Ok(Value::String(format!("{}{}", a, b)))
            }
            _ => Err(InterpreterError::InvalidBinaryOperands(
                op.optype,
                lhs,
                rhs,
                self.error_msg(op.offset),
            )),
        }
    }

    pub fn interpret(&self, expr: Expr) -> Result<Value, InterpreterError> {
        self.expression(expr)
    }
}

pub fn interpret(source: &str, expr: Expr) -> Result<Value, InterpreterError> {
    let interpreter = Interpreter::new(source);
    interpreter.interpret(expr)
}

fn is_truthy(val: Value) -> bool {
    !matches!(val, Value::Nil | Value::Bool(false))
}

fn is_equal(a: Value, b: Value) -> bool {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => (a - b).abs() < EPSILON,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}
