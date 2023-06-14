use std::{collections::HashMap, fmt::Display};

use crate::{
    ast::{Expr, Stmt, Variable},
    interpreter::Interpreter,
};

#[derive(Debug)]
pub enum ResolutionError {
    DoubleDeclaration(String, String),
    InvalidErrorReturn(String),
    VariableAccessInInitializer(String, String),
    InitializerReturnWithValue(String),
    UseThisOutsideClass(String),
}

impl PartialEq for ResolutionError {
    fn eq(&self, other: &Self) -> bool {
        matches!(
            (self, other),
            (
                ResolutionError::DoubleDeclaration(..),
                ResolutionError::DoubleDeclaration(..)
            ) | (
                ResolutionError::InvalidErrorReturn(..),
                ResolutionError::InvalidErrorReturn(..)
            ) | (
                ResolutionError::VariableAccessInInitializer(..),
                ResolutionError::VariableAccessInInitializer(..)
            ) | (
                ResolutionError::InitializerReturnWithValue(..),
                ResolutionError::InitializerReturnWithValue(..)
            ) | (
                ResolutionError::UseThisOutsideClass(..),
                ResolutionError::UseThisOutsideClass(..)
            )
        )
    }
}

impl Display for ResolutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        const PREFIX: &str = "Resolution error!";
        const PADDING: usize = 4;
        let pad_msg = |msg: &str| -> String {
            msg.lines()
                .map(|line| format!("{}{}\n", " ".repeat(PADDING), line))
                .collect()
        };
        match self {
            ResolutionError::DoubleDeclaration(name, msg) => write!(
                f,
                "{} Variable \"{}\" already declared in this scope.\n\n{}",
                PREFIX,
                name,
                pad_msg(msg)
            ),
            ResolutionError::InvalidErrorReturn(msg) => write!(
                f,
                "{} Return statement outside of function.\n\n{}",
                PREFIX,
                pad_msg(msg)
            ),
            ResolutionError::VariableAccessInInitializer(name, msg) => write!(
                f,
                "{} Variable \"{}\" accessed in its own initializer.\n\n{}",
                PREFIX,
                name,
                pad_msg(msg)
            ),
            ResolutionError::UseThisOutsideClass(msg) => write!(
                f,
                "{} Can't use 'this' outside of a class.\n\n{}",
                PREFIX,
                pad_msg(msg)
            ),
            ResolutionError::InitializerReturnWithValue(msg) => write!(
                f,
                "{} Can't return value from initializer.\n\n{}",
                PREFIX,
                pad_msg(msg)
            ),
        }
    }
}

#[derive(Copy, Clone)]
pub enum FunctionType {
    None,
    Function,
    Initializer,
    Method,
}

#[derive(Copy, Clone)]
pub enum ClassType {
    None,
    Class,
}

pub struct Resolver<'a> {
    source: &'a str,
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    current_function: FunctionType,
    current_class: ClassType,
    errors: Vec<ResolutionError>,
}

impl<'a> Resolver<'a> {
    fn new(source: &'a str, interpreter: &'a mut Interpreter) -> Self {
        Self {
            source,
            interpreter,
            scopes: vec![],
            current_function: FunctionType::None,
            current_class: ClassType::None,
            errors: vec![],
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

    fn begin_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, var: &Variable) {
        if let Some(map) = self.scopes.last_mut() {
            if map.contains_key(&var.name) {
                self.errors.push(ResolutionError::DoubleDeclaration(
                    var.name.to_owned(),
                    self.error_msg(var.offset),
                ))
            } else {
                map.insert(var.name.to_owned(), false);
            }
        }
    }

    fn define(&mut self, name: &str) {
        if let Some(map) = self.scopes.last_mut() {
            map.insert(name.to_owned(), true);
        }
    }

    fn resolve(&mut self, statements: &[Stmt]) {
        for statement in statements {
            self.resolve_statement(statement);
        }
    }

    fn resolve_statement(&mut self, statement: &Stmt) {
        match statement {
            Stmt::Block(statements) => self.resolve_block_stmt(statements),
            Stmt::Var { var, initializer } => self.resolve_var_stmt(var, initializer),
            Stmt::Function {
                var,
                parameters,
                body,
            } => self.resolve_function_stmt(var, parameters, body, FunctionType::Function),
            Stmt::Expression(expr) => self.resolve_expression(expr),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => self.resolve_if_stmt(condition, then_branch, else_branch),
            Stmt::Print(expr) => self.resolve_expression(expr),
            Stmt::Return { expr, offset } => match self.current_function {
                FunctionType::None => self
                    .errors
                    .push(ResolutionError::InvalidErrorReturn(self.error_msg(*offset))),
                FunctionType::Initializer if expr.is_some() => {
                    self.errors
                        .push(ResolutionError::InitializerReturnWithValue(
                            self.error_msg(*offset),
                        ))
                }
                _ => {
                    if let Some(expr) = expr {
                        self.resolve_expression(expr);
                    }
                }
            },
            Stmt::While { condition, body } => {
                self.resolve_expression(condition);
                self.resolve_statement(body);
            }
            Stmt::Class { var, methods } => self.resolve_class_statement(var, methods),
        }
    }

    fn resolve_block_stmt(&mut self, statements: &[Stmt]) {
        self.begin_scope();
        self.resolve(statements);
        self.end_scope();
    }

    fn resolve_var_stmt(&mut self, var: &Variable, initializer: &Option<Expr>) {
        self.declare(var);
        if let Some(expr) = initializer {
            self.resolve_expression(expr);
        }
        self.define(&var.name);
    }

    fn resolve_function_stmt(
        &mut self,
        var: &Variable,
        parameters: &[Variable],
        body: &[Stmt],
        function_type: FunctionType,
    ) {
        self.declare(var);
        self.define(&var.name);

        let eclosing_function = self.current_function;
        self.current_function = function_type;

        self.begin_scope();
        for parameter in parameters {
            self.declare(parameter);
            self.define(&parameter.name);
        }
        self.resolve(body);
        self.end_scope();

        self.current_function = eclosing_function;
    }

    fn resolve_if_stmt(
        &mut self,
        condition: &Expr,
        then_branch: &Stmt,
        else_branch: &Option<Box<Stmt>>,
    ) {
        self.resolve_expression(condition);
        self.resolve_statement(then_branch);
        if let Some(stmt) = else_branch {
            self.resolve_statement(stmt);
        }
    }

    fn resolve_class_statement(&mut self, var: &Variable, methods: &[Stmt]) {
        let enclosing_class = self.current_class;
        self.current_class = ClassType::Class;

        self.declare(var);
        self.define(&var.name);

        self.begin_scope();
        self.scopes
            .last_mut()
            .unwrap()
            .insert("this".to_string(), true);
        for method in methods {
            if let Stmt::Function {
                var,
                parameters,
                body,
            } = method
            {
                let mut function_type = FunctionType::Method;
                if var.name == "init" {
                    function_type = FunctionType::Initializer;
                }
                self.resolve_function_stmt(var, parameters, body, function_type)
            }
            // Check error?
        }
        self.end_scope();

        self.current_class = enclosing_class;
    }

    fn resolve_expression(&mut self, expr: &Expr) {
        match expr {
            Expr::Variable(var) => self.resolve_variable_expr(var),
            Expr::Assign { var, value } => {
                self.resolve_expression(value);
                self.resolve_local(var);
            }
            Expr::Binary { op: _, lhs, rhs } => {
                self.resolve_expression(lhs);
                self.resolve_expression(rhs);
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => self.resolve_call_expr(callee, arguments),
            Expr::Grouping(expr) => self.resolve_expression(expr),
            Expr::Literal(_) => {}
            Expr::Logical { op: _, lhs, rhs } => {
                self.resolve_expression(lhs);
                self.resolve_expression(rhs);
            }
            Expr::Unary { op: _, rhs } => self.resolve_expression(rhs),
            Expr::Get {
                object,
                property: _,
            } => self.resolve_expression(object),
            Expr::Set {
                object,
                property: _,
                value,
            } => {
                self.resolve_expression(object);
                self.resolve_expression(value);
            }
            Expr::This(offset) => match self.current_class {
                ClassType::Class => self.resolve_local(&Variable {
                    name: "this".to_string(),
                    offset: *offset,
                }),
                _ => self.errors.push(ResolutionError::UseThisOutsideClass(
                    self.error_msg(*offset),
                )),
            },
        }
    }

    fn resolve_variable_expr(&mut self, var: &Variable) {
        if let Some(map) = self.scopes.last() {
            if let Some(false) = map.get(&var.name) {
                self.errors
                    .push(ResolutionError::VariableAccessInInitializer(
                        var.name.clone(),
                        self.error_msg(var.offset),
                    ));
                return;
            }
        }

        self.resolve_local(var);
    }

    fn resolve_call_expr(&mut self, callee: &Expr, arguments: &[Expr]) {
        self.resolve_expression(callee);
        for argument in arguments {
            self.resolve_expression(argument);
        }
    }

    fn resolve_local(&mut self, var: &Variable) {
        if !self.scopes.is_empty() {
            let mut idx = self.scopes.len() - 1;
            loop {
                if self.scopes[idx].contains_key(&var.name) {
                    self.interpreter.resolve(var, self.scopes.len() - 1 - idx);
                    break;
                }
                if idx != 0 {
                    idx -= 1;
                } else {
                    break;
                }
            }
        }
    }
}

pub fn resolve_variables(
    source: &str,
    interpreter: &mut Interpreter,
    statements: &[Stmt],
) -> Result<(), Vec<ResolutionError>> {
    let mut resolver = Resolver::new(source, interpreter);
    resolver.resolve(statements);

    if !resolver.errors.is_empty() {
        Err(resolver.errors)
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::rc::Rc;

    use crate::{interpreter::Interpreter, parser::parse_statements, scanner::scan_tokens};

    use super::{resolve_variables, ResolutionError};

    fn resolve(src: &str) -> Result<(), Vec<ResolutionError>> {
        let src = Rc::new(src.to_owned());
        let tokens = scan_tokens(&src).unwrap();
        let statements = parse_statements(&src, tokens).unwrap();
        let mut interpreter = Interpreter::new_no_stdout();
        resolve_variables(&src, &mut interpreter, &statements)
    }

    fn check_resolution_error(src: &str, kind: ResolutionError) {
        let res = resolve(src);
        assert!(res.is_err());
        if let Some(errors) = res.err() {
            assert_eq!(errors.len(), 1);
            assert_eq!(errors[0], kind);
        }
    }

    #[test]
    fn test_double_declaration() {
        check_resolution_error(
            "\
            {\n\
                var a = \"a\";\n\
                var a = \"a\";\n\
            }\n",
            ResolutionError::DoubleDeclaration(format!(""), format!("")),
        );
    }

    #[test]
    fn test_return_outside_function1() {
        check_resolution_error(
            "return \"foo\";\n",
            ResolutionError::InvalidErrorReturn(format!("")),
        );
    }

    #[test]
    fn test_return_outside_function2() {
        check_resolution_error(
            "\
            { \n\
                return \"foo\";\n\
            }\n",
            ResolutionError::InvalidErrorReturn(format!("")),
        );
    }

    #[test]
    fn test_var_initializer_self_access() {
        check_resolution_error(
            "\
            {\n\
                var a = a + 1;\n\
            }\n",
            ResolutionError::VariableAccessInInitializer(format!(""), format!("")),
        );
    }

    #[test]
    fn test_initializer_value_return() {
        check_resolution_error(
            "\
            class Foo {\n\
                init() {\n\
                    return \"asd\";\n\
                }\n\
            }\n\
            ",
            ResolutionError::InitializerReturnWithValue(format!("")),
        );
    }

    #[test]
    fn test_this_outside_class_method1() {
        check_resolution_error(
            "\
            {\n\
                print this;\n\
            }\n\
            ",
            ResolutionError::UseThisOutsideClass(format!("")),
        );
    }

    #[test]
    fn test_this_outside_class_method2() {
        check_resolution_error(
            "\
            fun foo() {\n\
                print this;\n\
            }\n\
            ",
            ResolutionError::UseThisOutsideClass(format!("")),
        );
    }
}
