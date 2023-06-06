use std::fmt::Debug;

#[derive(Clone)]
pub enum Expr {
    Literal(Literal),
    Logical {
        op: LogicalOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        op: UnaryOp,
        rhs: Box<Expr>,
    },
    Binary {
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Grouping(Box<Expr>),
    Variable(Variable),
    Assign {
        var: Variable,
        value: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: usize,
        arguments: Vec<Expr>,
    },
}

impl Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{:?}", l),
            Expr::Logical { op, lhs, rhs } => write!(f, "({:?} {:?} {:?})", op.optype, lhs, rhs),
            Expr::Unary { op, rhs } => write!(f, "({:?} {:?})", op.optype, rhs),
            Expr::Binary { op, lhs, rhs } => write!(f, "({:?} {:?} {:?})", op.optype, lhs, rhs),
            Expr::Grouping(expr) => write!(f, "(group {:?})", expr),
            Expr::Variable(var) => write!(f, "{}", var.name),
            Expr::Assign { var, value } => write!(f, "{} = {:?}", var.name, value),
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => write!(f, "{:?}({:?})", callee, arguments),
        }
    }
}

#[derive(Clone)]
pub enum Literal {
    Number(f64),
    String(String),
    True,
    False,
    Nil,
}

impl Debug for Literal {
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
pub enum LogicalOpType {
    And,
    Or,
}

impl Debug for LogicalOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOpType::And => write!(f, "and"),
            LogicalOpType::Or => write!(f, "or"),
        }
    }
}

#[derive(Clone)]
pub struct LogicalOp {
    pub optype: LogicalOpType,
    pub offset: usize,
}

impl LogicalOp {
    pub fn new(optype: LogicalOpType, offset: usize) -> Self {
        Self { optype, offset }
    }
}

#[derive(Copy, Clone)]
pub enum UnaryOpType {
    Negate,
    Not,
}

impl Debug for UnaryOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UnaryOpType::Negate => write!(f, "-"),
            UnaryOpType::Not => write!(f, "!"),
        }
    }
}

#[derive(Clone)]
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

impl Debug for BinaryOpType {
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

#[derive(Clone)]
pub struct BinaryOp {
    pub optype: BinaryOpType,
    pub offset: usize,
}

impl BinaryOp {
    pub fn new(optype: BinaryOpType, offset: usize) -> Self {
        Self { optype, offset }
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Variable {
    pub name: String,
    pub offset: usize,
}

#[derive(Clone)]
pub enum Stmt {
    Expression(Expr),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print(Expr),
    Var {
        var: Variable,
        initializer: Option<Expr>,
    },
    Block(Vec<Stmt>),
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
    Function {
        var: Variable,
        parameters: Vec<Variable>,
        body: Vec<Stmt>,
    },
    Return {
        expr: Option<Expr>,
        offset: usize,
    },
}

impl Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Expression(expr) => write!(f, "{:?}", expr),
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => write!(
                f,
                "if ({:?}) then {:?} else {:?}",
                condition, then_branch, else_branch
            ),
            Stmt::Print(expr) => write!(f, "print {:?}", expr),
            Stmt::Var { var, initializer } => match initializer {
                Some(expr) => write!(f, "var {} = {:?}", var.name, expr),
                None => write!(f, "var {}", var.name),
            },
            Stmt::Block(statements) => write!(f, "Block {:#?}", statements),
            Stmt::While { condition, body } => write!(f, "while {:?} {:#?}", condition, body),
            Stmt::Function {
                var,
                parameters,
                body,
            } => write!(f, "{}({:?}) {:#?}", var.name, parameters, body),
            Stmt::Return { expr, offset: _ } => match expr {
                Some(expr) => write!(f, "return {:?}", expr),
                None => write!(f, "return"),
            },
        }
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
    fn test_astprinter_example() {
        let expr = Expr::Binary {
            op: BinaryOp::new(BinaryOpType::Mul, 0),
            lhs: Box::new(Expr::Unary {
                op: UnaryOp::new(UnaryOpType::Negate, 0),
                rhs: Box::new(Expr::Literal(Literal::Number(123.0))),
            }),
            rhs: Box::new(Expr::Grouping(Box::new(Expr::Literal(Literal::Number(
                45.67,
            ))))),
        };
        assert_eq!(format!("{:?}", expr), "(* (- 123) (group 45.67))");
    }
}
