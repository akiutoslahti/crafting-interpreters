#![allow(dead_code)]

use std::fmt::Display;

pub enum Expr {
    Literal(Literal),
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        op: BinaryOp,
        rhs: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Variable {
        name: String,
        offset: usize,
    },
    Assign {
        name: String,
        value: Box<Expr>,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Unary { op, rhs } => write!(f, "({} {})", op.optype, rhs),
            Expr::Binary { lhs, op, rhs } => write!(f, "({} {} {})", op.optype, lhs, rhs),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Variable { name, offset: _ } => write!(f, "{}", name),
            Expr::Assign { name, value } => write!(f, "{} = {}", name, value),
        }
    }
}

pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(n) => write!(f, "{}", n),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::True => write!(f, "true"),
            Literal::False => write!(f, "false"),
            Literal::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Copy, Clone)]
pub enum UnaryOpType {
    Negate,
    Not,
}

impl Display for UnaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOpType::Negate => write!(f, "-"),
            UnaryOpType::Not => write!(f, "!"),
        }
    }
}

pub struct UnaryOp {
    pub optype: UnaryOpType,
    pub offset: usize,
}

impl UnaryOp {
    pub fn new(optype: UnaryOpType, offset: usize) -> Self {
        Self { optype, offset }
    }
}

#[derive(Copy, Clone)]
pub enum BinaryOpType {
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for BinaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOpType::Equal => write!(f, "=="),
            BinaryOpType::NotEqual => write!(f, "!="),
            BinaryOpType::Less => write!(f, "<"),
            BinaryOpType::LessEqual => write!(f, "<="),
            BinaryOpType::Greater => write!(f, ">"),
            BinaryOpType::GreaterEqual => write!(f, ">="),
            BinaryOpType::Add => write!(f, "+"),
            BinaryOpType::Sub => write!(f, "-"),
            BinaryOpType::Mul => write!(f, "*"),
            BinaryOpType::Div => write!(f, "/"),
        }
    }
}

pub struct BinaryOp {
    pub optype: BinaryOpType,
    pub offset: usize,
}

impl BinaryOp {
    pub fn new(optype: BinaryOpType, offset: usize) -> Self {
        Self { optype, offset }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::BinaryOp;
    use crate::ast::BinaryOpType;
    use crate::ast::Expr;
    use crate::ast::Literal;
    use crate::ast::UnaryOp;
    use crate::ast::UnaryOpType;

    #[test]
    fn check_astprinter_example() {
        let expr = Expr::Binary {
            lhs: Box::new(Expr::Unary {
                op: UnaryOp::new(UnaryOpType::Negate, 0),
                rhs: Box::new(Expr::Literal(Literal::Number(123.0))),
            }),
            op: BinaryOp::new(BinaryOpType::Mul, 0),
            rhs: Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                45.67,
            ))))),
        };
        assert_eq!(format!("{}", expr), "(* (- 123) (group 45.67))");
    }
}

pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var {
        name: String,
        initializer: Option<Expr>,
    },
    Block(Vec<Stmt>),
}

impl Display for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "{}", expr),
            Stmt::Print(expr) => write!(f, "(print {})", expr),
            Stmt::Var { name, initializer } => match initializer {
                Some(expr) => write!(f, "var {} = {}", name, expr),
                None => write!(f, "var {}", name),
            },
            // TODO Print properly
            Stmt::Block(_statements) => todo!(),
        }
    }
}
