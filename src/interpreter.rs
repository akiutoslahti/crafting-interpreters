use crate::ast::{
    BinaryOp, BinaryOpType, Expr, Literal, LogicalOp, LogicalOpType, Stmt, UnaryOp, UnaryOpType,
};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    f64::EPSILON,
    fmt::{Debug, Display},
    rc::Rc,
    time::{SystemTime, SystemTimeError},
};

pub enum InterpreterError {
    InvalidUnaryOperand(UnaryOpType, Value, String),
    InvalidBinaryOperands(BinaryOpType, Value, Value, String),
    DivisionByZero(String),
    UnknownVariable(String, String),
    Return(Value),
    InvalidArgumentCount(usize, usize, String),
    SystemTimeError(SystemTimeError),
    NotCallable(Value, String),
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
            InterpreterError::UnknownVariable(name, msg) => write!(
                f,
                "{} Unknown variable \"{}\".\n\n{}",
                PREFIX,
                name,
                pad_msg(msg)
            ),
            InterpreterError::Return(_) => panic!("Unhandled Return statement!"),
            InterpreterError::InvalidArgumentCount(got, expected, msg) => write!(
                f,
                "{} Invalid argument count (got: {}, expected: {}).\n\n{}",
                PREFIX,
                got,
                expected,
                pad_msg(msg)
            ),
            InterpreterError::SystemTimeError(err) => {
                write!(f, "SystemTimeError difference: {:?}", err.duration())
            }
            InterpreterError::NotCallable(val, msg) => write!(
                f,
                "{} Couldn't cast {} to 'Callable'.\n\n{}",
                PREFIX,
                val,
                pad_msg(msg)
            ),
        }
    }
}

trait Callable {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError>;
}

#[derive(Clone)]
pub struct Primitive {
    name: String,
    arity: usize,
    function: fn(&mut Interpreter, Vec<Value>) -> Result<Value, InterpreterError>,
}

impl Debug for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fun {}>", self.name)
    }
}

impl Callable for Primitive {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        (self.function)(interpreter, arguments)
    }
}

#[derive(Clone)]
pub struct Function {
    name: String,
    parameters: Vec<String>,
    body: Stmt,
    closure: Rc<RefCell<Environment>>,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fun {}>", self.name)
    }
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.parameters.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        let mut environment = Environment::new_with_enclosing(self.closure.clone());
        self.parameters
            .iter()
            .zip(arguments.iter())
            .for_each(|(param, arg)| environment.define(param.clone(), arg.clone()));

        if let Stmt::Block(statements) = &self.body {
            match interpreter.execute_block(statements, environment) {
                Err(InterpreterError::Return(val)) => Ok(val),
                Err(err) => Err(err),
                _ => Ok(Value::Nil),
            }
        } else {
            Ok(Value::Nil)
        }
    }
}

#[derive(Clone)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
    Primitive(Primitive),
    Function(Function),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Bool(true) => write!(f, "true"),
            Value::Bool(false) => write!(f, "false"),
            Value::Nil => write!(f, "nil"),
            Value::Primitive(fun) => write!(f, "{:?}", fun),
            Value::Function(fun) => write!(f, "{:?}", fun),
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
    #[allow(dead_code)]
    globals: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let global_env = Rc::new(RefCell::new(Environment::new()));

        global_env.borrow_mut().define(
            "clock".to_owned(),
            Value::Primitive(Primitive {
                name: "clock".to_owned(),
                arity: 0,
                function: |_, _| {
                    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                        Ok(time) => time.as_millis(),
                        Err(err) => return Err(InterpreterError::SystemTimeError(err)),
                    };
                    Ok(Value::Number(time as f64))
                },
            }),
        );

        Self {
            source: Rc::new("".to_string()),
            environment: global_env.clone(),
            globals: global_env,
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
            Expr::Call {
                callee,
                paren,
                arguments,
            } => self.call(callee, *paren, arguments),
        }
    }

    fn call(
        &mut self,
        callee: &Expr,
        paren: usize,
        arguments: &[Expr],
    ) -> Result<Value, InterpreterError> {
        let callee = self.evaluate(callee)?;
        let arguments: Result<Vec<Value>, InterpreterError> =
            arguments.iter().map(|arg| self.evaluate(arg)).collect();

        match arguments {
            Ok(arguments) => match to_callable(&callee) {
                Some(callable) => {
                    if callable.arity() != arguments.len() {
                        return Err(InterpreterError::InvalidArgumentCount(
                            arguments.len(),
                            callable.arity(),
                            self.error_msg(paren),
                        ));
                    }
                    callable.call(self, arguments)
                }
                None => Err(InterpreterError::NotCallable(callee, self.error_msg(paren))),
            },
            Err(err) => Err(err),
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
            (BinaryOpType::Add, Value::String(a), Value::Number(b)) => {
                Ok(Value::String(format!("{}{}", a, b)))
            }
            (BinaryOpType::Add, Value::Number(a), Value::String(b)) => {
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

    fn execute_block(
        &mut self,
        statements: &[Stmt],
        environment: Environment,
    ) -> Result<(), InterpreterError> {
        let previous = self.environment.clone();
        self.environment = Rc::new(RefCell::new(environment));

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

    fn execute_return(&mut self, expr: &Option<Expr>) -> Result<(), InterpreterError> {
        let val = match expr {
            Some(expr) => self.evaluate(expr)?,
            None => Value::Nil,
        };

        Err(InterpreterError::Return(val))
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
            Stmt::Block(statements) => self.execute_block(
                statements,
                Environment::new_with_enclosing(self.environment.clone()),
            )?,
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => self.execute_if(condition, then_branch, else_branch)?,
            Stmt::While { condition, body } => self.execute_while(condition, body)?,
            Stmt::Function {
                name,
                parameters,
                body,
            } => self.environment.borrow_mut().define(
                name.to_owned(),
                Value::Function(Function {
                    name: name.to_owned(),
                    parameters: parameters.to_vec(),
                    body: *body.clone(),
                    closure: self.environment.clone(),
                }),
            ),
            Stmt::Return(expr) => self.execute_return(expr)?,
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

fn to_callable(val: &Value) -> Option<&dyn Callable> {
    match val {
        Value::Primitive(fun) => Some(fun),
        Value::Function(fun) => Some(fun),
        _ => None,
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
