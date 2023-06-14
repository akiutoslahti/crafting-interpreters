use crate::ast::{
    BinaryOp, BinaryOpType, Expr, Literal, LogicalOp, LogicalOpType, Stmt, UnaryOp, UnaryOpType,
    Variable,
};
use std::{
    cell::RefCell,
    collections::{hash_map::Entry, HashMap},
    f64::EPSILON,
    fmt::{Debug, Display},
    io::Write,
    rc::Rc,
    time::{SystemTime, SystemTimeError},
};

#[derive(Debug)]
pub enum InterpreterError {
    InvalidUnaryOperand(UnaryOpType, Value, String),
    InvalidBinaryOperands(BinaryOpType, Value, Value, String),
    DivisionByZero(String),
    UnknownVariable(String, String),
    Return(Value),
    InvalidArgumentCount(usize, usize, String),
    SystemTimeError(SystemTimeError),
    NotCallable(Value, String),
    InvalidPropertyAccess(String),
    UndefinedProperty(String, String),
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
            InterpreterError::InvalidPropertyAccess(msg) => write!(
                f,
                "{} Only instances have properties.\n\n{}",
                PREFIX,
                pad_msg(msg)
            ),
            InterpreterError::UndefinedProperty(property, msg) => write!(
                f,
                "{} Undefined property \"{}\".\n\n{}",
                PREFIX,
                property,
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

impl Callable for Rc<RefCell<Primitive>> {
    fn arity(&self) -> usize {
        self.borrow().arity()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        self.borrow().call(interpreter, arguments)
    }
}

#[derive(Clone)]
pub struct Function {
    name: String,
    parameters: Vec<Variable>,
    body: Vec<Stmt>,
    closure: Rc<RefCell<Environment>>,
    is_initializer: bool,
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fun {}>", self.name)
    }
}

impl Function {
    fn bind(&self, instance: Rc<RefCell<Instance>>) -> Self {
        let mut environment = Environment::new_with_enclosing(self.closure.clone());
        environment.define("this".to_string(), Value::Instance(instance));
        Self {
            closure: Rc::new(RefCell::new(environment)),
            ..self.clone()
        }
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
            .for_each(|(param, arg)| environment.define(param.name.clone(), arg.clone()));

        if self.is_initializer {
            let this = if let Some(value) = self.closure.borrow().get_at(0, &"this".to_string()) {
                value
            } else {
                panic!("Unexpected error caused by resolver. Couldn't find \"this\" binding when executing class method");
            };

            match interpreter.execute_block(&self.body, environment) {
                Err(InterpreterError::Return(..)) => Ok(this),
                Err(err) => Err(err),
                _ => Ok(this),
            }
        } else {
            match interpreter.execute_block(&self.body, environment) {
                Err(InterpreterError::Return(value)) => Ok(value),
                Err(err) => Err(err),
                _ => Ok(Value::Nil),
            }
        }
    }
}

impl Callable for Rc<RefCell<Function>> {
    fn arity(&self) -> usize {
        self.borrow().arity()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        self.borrow().call(interpreter, arguments)
    }
}

#[derive(Clone)]
pub struct Class {
    name: String,
    methods: HashMap<String, Rc<RefCell<Function>>>,
}

impl Debug for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}

impl Callable for Rc<RefCell<Class>> {
    fn arity(&self) -> usize {
        if let Some(function) = self.borrow().find_method("this") {
            return function.borrow().arity();
        }
        0
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Value>,
    ) -> Result<Value, InterpreterError> {
        let instance = Rc::new(RefCell::new(Instance::new(self.clone())));

        if let Some(function) = self.borrow().find_method("init") {
            #[allow(unused_must_use)]
            {
                function
                    .borrow()
                    .bind(instance.clone())
                    .call(interpreter, arguments);
            }
        }

        Ok(Value::Instance(instance))
    }
}

impl Class {
    fn find_method(&self, method: &str) -> Option<Rc<RefCell<Function>>> {
        self.methods.get(method).cloned()
    }
}

#[derive(Clone)]
pub struct Instance {
    class: Rc<RefCell<Class>>,
    fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: Rc<RefCell<Class>>) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }

    fn get(&self, instance: Rc<RefCell<Instance>>, name: &str) -> Option<Value> {
        match self.fields.get(name) {
            Some(property) => Some(property.clone()),
            None => self.class.borrow().find_method(name).map(|function| {
                Value::Function(Rc::new(RefCell::new(function.borrow().bind(instance))))
            }),
        }
    }

    fn set(&mut self, property: String, value: Value) {
        self.fields.insert(property, value);
    }
}

impl Debug for Instance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<instance {:?}>", self.class.borrow())
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Nil,
    Primitive(Rc<RefCell<Primitive>>),
    Function(Rc<RefCell<Function>>),
    Class(Rc<RefCell<Class>>),
    Instance(Rc<RefCell<Instance>>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(true) => write!(f, "true"),
            Value::Bool(false) => write!(f, "false"),
            Value::Nil => write!(f, "nil"),
            Value::Primitive(fun) => write!(f, "{:?}", fun.borrow()),
            Value::Function(fun) => write!(f, "{:?}", fun.borrow()),
            Value::Class(class) => write!(f, "{:?}", class.borrow()),
            Value::Instance(instance) => write!(f, "{:?}", instance.borrow()),
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
        self.values.get(name).cloned()
    }

    fn get_at(&self, dist: usize, name: &String) -> Option<Value> {
        if dist == 0 {
            return self.get(name);
        }
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow().get_at(dist - 1, name),
            None => panic!("Unexpected error caused by resolver. Variable was resolved too deep."),
        }
    }

    fn assign(&mut self, name: String, val: Value) -> Result<(), ()> {
        if let Entry::Occupied(mut e) = self.values.entry(name) {
            e.insert(val);
            Ok(())
        } else {
            Err(())
        }
    }

    fn assign_at(&mut self, dist: usize, name: String, val: Value) -> Result<(), ()> {
        if dist == 0 {
            return self.assign(name, val);
        }
        match &self.enclosing {
            Some(enclosing) => enclosing.borrow_mut().assign_at(dist - 1, name, val),
            None => panic!("Unexpected error caused by resolver. Variable was resolved too deep."),
        }
    }
}

pub struct Interpreter {
    source: Rc<String>,
    environment: Rc<RefCell<Environment>>,
    #[allow(dead_code)]
    globals: Rc<RefCell<Environment>>,
    out: Option<Vec<u8>>,
    locals: HashMap<Variable, usize>,
}

impl Interpreter {
    fn new_local(out: Option<Vec<u8>>) -> Self {
        let global_env = Rc::new(RefCell::new(Environment::new()));

        global_env.borrow_mut().define(
            "clock".to_owned(),
            Value::Primitive(Rc::new(RefCell::new(Primitive {
                name: "clock".to_owned(),
                arity: 0,
                function: |_, _| {
                    let time = match SystemTime::now().duration_since(SystemTime::UNIX_EPOCH) {
                        Ok(time) => time.as_millis(),
                        Err(err) => return Err(InterpreterError::SystemTimeError(err)),
                    };
                    Ok(Value::Number(time as f64))
                },
            }))),
        );

        Self {
            source: Rc::new("".to_string()),
            environment: global_env.clone(),
            globals: global_env,
            out,
            locals: HashMap::new(),
        }
    }

    pub fn new() -> Self {
        Self::new_local(None)
    }

    #[allow(dead_code)]
    pub fn new_no_stdout() -> Self {
        Self::new_local(Some(Vec::<u8>::new()))
    }

    #[allow(dead_code)]
    fn output(&self) -> Option<String> {
        match &self.out {
            Some(vec) => match String::from_utf8(vec.clone()) {
                Ok(str) => Some(str),
                Err(err) => panic!("Couldn't convert output to String: {}", err),
            },
            _ => None,
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
            Expr::Variable(var) => self.lookup_variable(var),
            Expr::Assign { var, value } => self.assign_variable(var, value),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => self.call(callee, *paren, arguments),
            Expr::Get { object, property } => self.get_expr(object, property),
            Expr::Set {
                object,
                property,
                value,
            } => self.set_expr(object, property, value),
            Expr::This(offset) => self.lookup_variable(&Variable {
                name: "this".to_string(),
                offset: *offset,
            }),
        }
    }

    fn assign_variable(&mut self, var: &Variable, value: &Expr) -> Result<Value, InterpreterError> {
        let value = self.evaluate(value)?;
        let res = match self.locals.get(var) {
            Some(dist) => {
                self.environment
                    .borrow_mut()
                    .assign_at(*dist, var.name.clone(), value.clone())
            }
            None => self
                .globals
                .borrow_mut()
                .assign(var.name.clone(), value.clone()),
        };

        match res {
            Ok(..) => Ok(value),
            Err(..) => Err(InterpreterError::UnknownVariable(
                var.name.clone(),
                self.error_msg(var.offset),
            )),
        }
    }

    fn lookup_variable(&self, var: &Variable) -> Result<Value, InterpreterError> {
        let res = match self.locals.get(var) {
            Some(dist) => self.environment.borrow().get_at(*dist, &var.name),
            None => self.globals.borrow().get(&var.name),
        };

        match res {
            Some(val) => Ok(val),
            None => Err(InterpreterError::UnknownVariable(
                var.name.clone(),
                self.error_msg(var.offset),
            )),
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

    fn get_expr(&mut self, object: &Expr, property: &Variable) -> Result<Value, InterpreterError> {
        let object = self.evaluate(object)?;
        match object {
            Value::Instance(instance) => {
                match instance.borrow().get(instance.clone(), &property.name) {
                    Some(value) => Ok(value),
                    None => Err(InterpreterError::UndefinedProperty(
                        property.name.clone(),
                        self.error_msg(property.offset),
                    )),
                }
            }
            _ => Err(InterpreterError::InvalidPropertyAccess(
                self.error_msg(property.offset),
            )),
        }
    }

    fn set_expr(
        &mut self,
        object: &Expr,
        property: &Variable,
        value: &Expr,
    ) -> Result<Value, InterpreterError> {
        let object = self.evaluate(object)?;

        match object {
            Value::Instance(instance) => {
                let value = self.evaluate(value)?;
                instance
                    .borrow_mut()
                    .set(property.name.clone(), value.clone());
                Ok(value)
            }
            _ => todo!(),
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
                match &mut self.out {
                    Some(vec) => {
                        if let Err(err) = writeln!(vec, "{}", val) {
                            panic!("Couldn't write to output buffer: {}", err);
                        }
                    }
                    None => println!("{}", val),
                }
            }
            Stmt::Var { var, initializer } => {
                // TODO Remove implicit variable initialization?
                let mut val = Value::Nil;
                if let Some(expr) = initializer {
                    val = self.evaluate(expr)?;
                }
                self.environment.borrow_mut().define(var.name.clone(), val);
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
                var,
                parameters,
                body,
            } => self.environment.borrow_mut().define(
                var.name.clone(),
                Value::Function(Rc::new(RefCell::new(Function {
                    name: var.name.clone(),
                    parameters: parameters.to_vec(),
                    body: body.to_vec(),
                    closure: self.environment.clone(),
                    is_initializer: false,
                }))),
            ),
            Stmt::Return { expr, offset: _ } => self.execute_return(expr)?,
            Stmt::Class { var, methods } => {
                self.environment
                    .borrow_mut()
                    .define(var.name.clone(), Value::Nil);

                let mut class_methods: HashMap<String, Rc<RefCell<Function>>> = HashMap::new();
                for method in methods {
                    if let Stmt::Function {
                        var,
                        parameters,
                        body,
                    } = method
                    {
                        class_methods.insert(
                            var.name.clone(),
                            Rc::new(RefCell::new(Function {
                                name: var.name.clone(),
                                parameters: parameters.to_vec(),
                                body: body.to_vec(),
                                closure: self.environment.clone(),
                                is_initializer: var.name == "init",
                            })),
                        );
                    }
                    // Check error?
                }

                if let Err(..) = self.environment.borrow_mut().assign(
                    var.name.clone(),
                    Value::Class(Rc::new(RefCell::new(Class {
                        name: var.name.clone(),
                        methods: class_methods,
                    }))),
                ) {
                    return Err(InterpreterError::UnknownVariable(
                        var.name.clone(),
                        self.error_msg(var.offset),
                    ));
                }
            }
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

    pub fn resolve(&mut self, var: &Variable, depth: usize) {
        self.locals.insert(var.clone(), depth);
    }
}

fn to_callable(val: &Value) -> Option<&dyn Callable> {
    match val {
        Value::Primitive(fun) => Some(fun),
        Value::Function(fun) => Some(fun),
        Value::Class(class) => Some(class),
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

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{parser::parse_statements, resolver::resolve_variables, scanner::scan_tokens};

    use super::Interpreter;

    // TODO Add tests for interpreter errors

    fn interpret(src: &str) -> String {
        let src = Rc::new(src.to_owned());
        let tokens = scan_tokens(&src).unwrap();
        let statements = parse_statements(&src, tokens).unwrap();
        let mut interpreter = Interpreter::new_no_stdout();
        resolve_variables(&src, &mut interpreter, &statements).unwrap();
        interpreter.interpret(src.clone(), statements).unwrap();
        match interpreter.output() {
            Some(str) => str,
            None => panic!("Couldn't get output from interpreter"),
        }
    }

    fn assert_output_eq(src: &str, expectation: &str) {
        assert_eq!(interpret(src), expectation);
    }

    #[test]
    fn test_while_loop() {
        assert_output_eq(
            "\
            var i = 0;\n\
            while (i < 3) {\n\
                print i;\n\
                i = i + 1;\n\
            }\n\
            ",
            "0\n1\n2\n",
        );
    }

    #[test]
    fn test_for_loop() {
        assert_output_eq(
            "\
            for (var i = 0; i < 3; i = i + 1) print i;\n\
            ",
            "0\n1\n2\n",
        );
    }

    #[test]
    fn test_if1() {
        assert_output_eq(
            "\
            fun a(b) {\n\
                if (b != 0) return true;\n\
                return false;\n\
            }\n\
            print a(0);\n\
            ",
            "false\n",
        );
    }

    #[test]
    fn test_if2() {
        assert_output_eq(
            "\
            fun a(b) {\n\
                if (b != 0) return true;\n\
                return false;\n\
            }\n\
            print a(1);\n\
            ",
            "true\n",
        );
    }

    #[test]
    fn test_if_else1() {
        assert_output_eq(
            "\
            fun a(b) {\n\
                if (b != 0) return true;\n\
                else return false;\n\
            }\n\
            print a(0);\n\
            ",
            "false\n",
        );
    }

    #[test]
    fn test_if_else2() {
        assert_output_eq(
            "\
            fun a(b) {\n\
                if (b != 1) return true;\n\
                else return false;\n\
            }\n\
            print a(0);\n\
            ",
            "true\n",
        );
    }

    #[test]
    fn test_global_var() {
        assert_output_eq(
            "\
            var asd = \"asd\";\n\
            fun a() { print asd; }\n\
            a();\n\
            ",
            "asd\n",
        );
    }

    #[test]
    fn test_shadowing() {
        assert_output_eq(
            "\
            var asd = \"asd\";\n\
            fun a() {\n\
                var asd = \"das\";\n\
                print asd;\n\
            }\n\
            a();\n\
            ",
            "das\n",
        );
    }

    #[test]
    fn test_shadowing_nested() {
        assert_output_eq(
            "\
            var a = \"global a\";\n\
            var b = \"global b\";\n\
            var c = \"global c\";\n\
            {\n\
                var a = \"outer a\";\n\
                var b = \"outer b\";\n\
                {\n\
                    var a = \"inner a\";\n\
                    print a;\n\
                    print b;\n\
                    print c;\n\
                }\n\
                print a;\n\
                print b;\n\
                print c;\n\
            }\n\
            print a;\n\
            print b;\n\
            print c;\n\
            ",
            "\
            inner a\n\
            outer b\n\
            global c\n\
            outer a\n\
            outer b\n\
            global c\n\
            global a\n\
            global b\n\
            global c\n\
            ",
        );
    }

    #[test]
    fn test_scope() {
        assert_output_eq(
            "\
            {\n\
                var a = \"first\";\n\
                print a;\n\
            }\n\
            {\n\
                var a = \"second\";\n\
                print a;\n\
            }",
            "first\nsecond\n",
        )
    }

    #[test]
    fn test_print() {
        assert_output_eq(
            "\
            fun a() {}\n\
            print a;\n\
            print \"foo\";\n\
            print 1;\n\
            print true;\n\
            print false;\n\
            print nil;\n\
            ",
            "<fun a>\nfoo\n1\ntrue\nfalse\nnil\n",
        );
    }

    #[test]
    fn test_implicit_return1() {
        assert_output_eq(
            "\
            fun a() {}\n\
            print a();\n\
            ",
            "nil\n",
        )
    }

    #[test]
    fn test_implicit_return2() {
        assert_output_eq(
            "\
            fun a() { return; }\n\
            print a();\n\
            ",
            "nil\n",
        )
    }

    #[test]
    fn test_return() {
        assert_output_eq(
            "\
            fun a() { return \"foo\"; }\n\
            print a();\n\
            ",
            "foo\n",
        )
    }

    #[test]
    fn test_fib() {
        assert_output_eq(
            "\
            fun fib(n) {\n\
                if (n < 2) return n;\n\
                return fib(n - 1) + fib(n - 2);\n\
            }\n\
            print fib(20);\
            ",
            "6765\n",
        )
    }

    #[test]
    fn test_closure_example() {
        assert_output_eq(
            "\
            fun makeCounter() {\n\
                var i = 0;\n\
                fun count() {\n\
                    i = i + 1;\n\
                    print i;\n\
                }\n\
                return count;\n\
            }\n\
            var counter = makeCounter();\n\
            counter();\n\
            counter();\
            ",
            "1\n2\n",
        )
    }

    #[test]
    fn test_binding_variables() {
        assert_output_eq(
            "\
            var a = \"global\";\n\
            {\n\
                fun showA() { print a; }\n\
                showA();\n\
                var a = \"block\";\n\
                showA();\n\
            }\n\
            ",
            "global\nglobal\n",
        );
    }

    #[test]
    fn test_class_declaration() {
        assert_output_eq(
            "\
            class A {\n\
                b() {}\n\
            }\n\
            print A;\n\
            ",
            "<class A>\n",
        );
    }

    #[test]
    fn test_class_instantiation() {
        assert_output_eq(
            "\
            class A {}\n\
            var a = A();\n\
            print a;\n\
            ",
            "<instance <class A>>\n",
        );
    }

    #[test]
    fn test_instance_property() {
        assert_output_eq(
            "\
            class A {}\n\
            var a = A();\n\
            a.b = \"c\";\n\
            print a.b;\n\
            ",
            "c\n",
        )
    }

    #[test]
    fn test_class_method() {
        assert_output_eq(
            "\
            class Foo {\n\
                bar() {\n\
                    print \"baz\";\n\
                }\n\
            }\n\
            Foo().bar();\n\
            ",
            "baz\n",
        );
    }

    #[test]
    fn test_this_binding1() {
        assert_output_eq(
            "\
            class Foo {\n\
                bar() {\n\
                    print this;\n\
                }\n\
            }\n\
            var method = Foo().bar;\n\
            method();\n\
            ",
            "<instance <class Foo>>\n",
        )
    }

    #[test]
    fn test_this_binding2() {
        assert_output_eq(
            "\
            class Cake {\n\
                taste() {\n\
                var adjective = \"delicious\";
                print \"The \" + this.flavor + \" cake is \" + adjective + \"!\";\n\
                }\n\
            }\n\
            var cake = Cake();\n\
            cake.flavor = \"German chocolate\";\n\
            cake.taste();\n\
            ",
            "The German chocolate cake is delicious!\n",
        )
    }

    #[test]
    fn test_this_binding3() {
        assert_output_eq(
            "\
            class Foo {\n\
                getCallback() {\n\
                    fun localFunction() {\n\
                        print this;\n\
                    }\n\
                    return localFunction;\n\
                }\n\
            }\n\
            var callback = Foo().getCallback();\n\
            callback();\n\
            ",
            "<instance <class Foo>>\n",
        )
    }

    #[test]
    fn test_class_initializer() {
        assert_output_eq(
            "\
            class Foo {\n\
                init() {\n\
                    this.bar = \"baz\";\n\
                }\n\
                print_bar() {\n\
                    print this.bar;\n\
                }
            }
            var foo = Foo();\n\
            foo.print_bar();\n\
            ",
            "baz\n",
        )
    }

    #[test]
    fn test_initializer_return_implicit() {
        assert_output_eq(
            "\
            class Foo {\n\
                init() {\n\
                    this.bar = \"baz\";\n\
                }\n\
                print_bar() {\n\
                    print this.bar;\n\
                }\n\
            }\n\
            var foo = Foo();\n\
            print foo.init();\n\
            ",
            "<instance <class Foo>>\n",
        )
    }

    #[test]
    fn test_initializer_return_explicit() {
        assert_output_eq(
            "\
            class Foo {\n\
                init() {\n\
                return;\n\
                }\n\
            }\n\
            var foo = Foo();\n\
            print foo.init();\n\
            ",
            "<instance <class Foo>>\n",
        )
    }
}
