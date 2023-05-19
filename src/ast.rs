#![allow(dead_code)]

use std::fmt::Display;

enum Expr {
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
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Unary { op, rhs } => write!(f, "({} {})", op, rhs),
            Expr::Binary { lhs, op, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
        }
    }
}

enum Literal {
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
            Literal::String(s) => write!(f, "{}", s),
            _ => todo!(),
        }
    }
}

enum UnaryOp {
    Negate,
    Not,
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOp::Negate => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}

enum BinaryOp {
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

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOp::Equal => write!(f, "="),
            BinaryOp::NotEqual => write!(f, "!="),
            BinaryOp::Less => write!(f, "<"),
            BinaryOp::LessEqual => write!(f, "="),
            BinaryOp::Greater => write!(f, ">"),
            BinaryOp::GreaterEqual => write!(f, ">="),
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Sub => write!(f, "-"),
            BinaryOp::Mul => write!(f, "*"),
            BinaryOp::Div => write!(f, "/"),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::BinaryOp;
    use crate::ast::Expr;
    use crate::ast::Literal;
    use crate::ast::UnaryOp;

    #[test]
    fn check_astprinter_example() {
        let expr = Expr::Binary {
            lhs: Box::new(Expr::Unary {
                op: UnaryOp::Negate,
                rhs: Box::new(Expr::Literal(Literal::Number(123.0))),
            }),
            op: BinaryOp::Mul,
            rhs: Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                45.67,
            ))))),
        };
        assert_eq!(format!("{}", expr), "(* (- 123) (group 45.67))");
    }
}
