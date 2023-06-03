use crate::ast::{
    BinaryOp, BinaryOpType, Expr, Literal, LogicalOp, LogicalOpType, Stmt, UnaryOp, UnaryOpType,
};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    f64::EPSILON,
    fmt::Display,
    rc::Rc,
};

pub enum InterpreterError {
    InvalidUnaryOperand(UnaryOpType, Value, String),
    InvalidBinaryOperands(BinaryOpType, Value, Value, String),
    DivisionByZero(String),
    UnknownVariable(String, String),
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
                "{} Invalid operand ({}) for unary expression ({:?}).\n\n{}",
                PREFIX,
                rhs,
                op,
                pad_msg(msg)
            ),
            InterpreterError::InvalidBinaryOperands(op, lhs, rhs, msg) => write!(
                f,
                "{} Invalid operands ({}, {}) for binary expression ({:?}).\n\n{}",
                PREFIX,
                lhs,
                rhs,
                op,
                pad_msg(msg)
            ),
            InterpreterError::DivisionByZero(msg) => {
                write!(f, "{} Division by zero.\n\n{}", PREFIX, pad_msg(msg))
            }
            InterpreterError::UnknownVariable(name, msg) => {
                write!(
                    f,
                    "{} Unknown variable \"{}\".\n\n{}",
                    PREFIX,
                    name,
                    pad_msg(msg)
                )
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

struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, Value>,
}

impl Environment {
    fn new() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    fn new_with_enclosing(enclosing: Rc<RefCell<Environment>>) -> Self {
        Self {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    fn define(&mut self, name: String, val: Value) {
        self.values.insert(name, val);
    }

    fn get(&self, name: &String) -> Option<Value> {
        match self.values.get(name) {
            Some(val) => Some(val.clone()),
            None => match &self.enclosing {
                Some(enclosing) => enclosing.borrow().get(name),
                None => None,
            },
        }
    }

    fn assign(&mut self, name: String, val: Value) -> Result<(), ()> {
        if let Entry::Occupied(mut e) = self.values.entry(name.clone()) {
            e.insert(val);
            Ok(())
        } else {
            match &self.enclosing {
                Some(enclosing) => enclosing.borrow_mut().assign(name, val),
                None => Err(()),
            }
        }
    }
}

pub struct Interpreter {
    source: Rc<String>,
    environment: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            source: Rc::new("".to_string()),
            environment: Rc::new(RefCell::new(Environment::new())),
        }
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

    fn expression(&mut self, expr: &Expr) -> Result<Value, InterpreterError> {
        match expr {
            Expr::Literal(l) => Ok(self.literal(l)),
            Expr::Logical { op, lhs, rhs } => self.logical(op, lhs, rhs),
            Expr::Unary { op, rhs } => self.unary(op, rhs),
            Expr::Binary { op, lhs, rhs } => self.binary(op, lhs, rhs),
            Expr::Grouping(expr) => self.expression(expr),
            Expr::Variable { name, offset } => match self.environment.borrow().get(name) {
                Some(name) => Ok(name),
                None => Err(InterpreterError::UnknownVariable(
                    name.clone(),
                    self.error_msg(*offset),
                )),
            },
            Expr::Assign {
                name,
                value,
                offset,
            } => {
                let value = self.evaluate(value)?;
                match self
                    .environment
                    .borrow_mut()
                    .assign(name.clone(), value.clone())
                {
                    Ok(..) => Ok(value),
                    Err(..) => Err(InterpreterError::UnknownVariable(
                        name.clone(),
                        self.error_msg(*offset),
                    )),
                }
            }
        }
    }

    fn literal(&self, literal: &Literal) -> Value {
        match literal {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::False => Value::Bool(false),
            Literal::True => Value::Bool(true),
            Literal::Nil => Value::Nil,
        }
    }

    fn logical(
        &mut self,
        op: &LogicalOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> Result<Value, InterpreterError> {
        let lhs = self.expression(lhs)?;

        match op.optype {
            LogicalOpType::Or if is_truthy(&lhs) => return Ok(lhs),
            LogicalOpType::And if !is_truthy(&lhs) => return Ok(lhs),
            _ => {}
        }

        self.expression(rhs)
    }

    fn unary(&mut self, op: &UnaryOp, rhs: &Expr) -> Result<Value, InterpreterError> {
        let rhs = self.expression(rhs)?;

        match (op.optype, rhs.clone()) {
            (UnaryOpType::Negate, Value::Number(n)) => Ok(Value::Number(-n)),
            (UnaryOpType::Not, val) => Ok(Value::Bool(!is_truthy(&val))),
            _ => Err(InterpreterError::InvalidUnaryOperand(
                op.optype,
                rhs,
                self.error_msg(op.offset),
            )),
        }
    }

    fn binary(&mut self, op: &BinaryOp, lhs: &Expr, rhs: &Expr) -> Result<Value, InterpreterError> {
        let lhs = self.expression(lhs)?;
        let rhs = self.expression(rhs)?;

        match (op.optype, lhs.clone(), rhs.clone()) {
            (BinaryOpType::Equal, a, b) => Ok(Value::Bool(is_equal(&a, &b))),
            (BinaryOpType::NotEqual, a, b) => Ok(Value::Bool(!is_equal(&a, &b))),
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

    fn evaluate(&mut self, expr: &Expr) -> Result<Value, InterpreterError> {
        self.expression(expr)
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result<(), InterpreterError> {
        let previous = self.environment.clone();
        self.environment = Rc::new(RefCell::new(Environment::new_with_enclosing(
            self.environment.clone(),
        )));

        let mut err = Ok(());
        for statement in statements {
            err = self.execute(statement);
            if err.is_err() {
                break;
            }
        }

        self.environment = previous;

        err
    }

    fn execute_if(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) -> Result<(), InterpreterError> {
        if is_truthy(&self.evaluate(condition)?) {
            self.execute(then_branch)?;
        } else if let Some(stmt) = else_branch {
            self.execute(stmt)?;
        }

        Ok(())
    }

    fn execute_while(&mut self, condition: &Expr, body: &Stmt) -> Result<(), InterpreterError> {
        while is_truthy(&self.evaluate(condition)?) {
            self.execute(body)?;
        }

        Ok(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), InterpreterError> {
        match stmt {
            Stmt::Expression(expr) => {
                self.evaluate(expr)?;
            }
            Stmt::Print(expr) => {
                let val = self.evaluate(expr)?;
                println!("{}", val);
            }
            Stmt::Var { name, initializer } => {
                // TODO Remove implicit variable initialization?
                let mut val = Value::Nil;
                if let Some(expr) = initializer {
                    val = self.evaluate(expr)?;
                }
                self.environment.borrow_mut().define(name.clone(), val);
            }
            Stmt::Block(statements) => self.execute_block(statements)?,
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => self.execute_if(condition, then_branch, else_branch)?,
            Stmt::While { condition, body } => self.execute_while(condition, body)?,
        }

        Ok(())
    }

    pub fn interpret(
        &mut self,
        source: Rc<String>,
        statements: Vec<Stmt>,
    ) -> Result<(), InterpreterError> {
        self.source = source;

        for stmt in &statements {
            self.execute(stmt)?;
        }

        Ok(())
    }
}

fn is_truthy(val: &Value) -> bool {
    !matches!(val, Value::Nil | Value::Bool(false))
}

fn is_equal(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => (a - b).abs() < EPSILON,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::String(a), Value::String(b)) => a == b,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}
