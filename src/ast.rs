use std::fmt::{Debug, Display};

#[derive(Clone, Debug)]
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
    Get {
        object: Box<Expr>,
        property: Variable,
    },
    Set {
        object: Box<Expr>,
        property: Variable,
        value: Box<Expr>,
    },
    This(usize),
    Super {
        offset: usize,
        method: Variable,
    },
}

// TODO Should we drop this? Or make it less insane?
impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(l) => write!(f, "{}", l),
            Expr::Logical { op, lhs, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::Unary { op, rhs } => write!(f, "({} {})", op, rhs),
            Expr::Binary { op, lhs, rhs } => write!(f, "({} {} {})", op, lhs, rhs),
            Expr::Grouping(expr) => write!(f, "(group {})", expr),
            Expr::Variable(var) => write!(f, "{}", var),
            Expr::Assign { var, value } => write!(f, "{} = {}", var, value),
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                let res = write!(f, "{}(", callee);
                let mut first = true;
                let res = arguments.iter().fold(res, |res, arg| {
                    if first {
                        first = false;
                        res.and_then(|_| write!(f, "{}", arg))
                    } else {
                        res.and_then(|_| write!(f, ", {}", arg))
                    }
                });
                res.and_then(|_| write!(f, ")"))
            }
            Expr::Get { object, property } => write!(f, "{}.{}", object, property),
            Expr::Set {
                object,
                property,
                value,
            } => write!(f, "{}.{} = {}", object, property, value),
            Expr::This(..) => write!(f, "this"),
            Expr::Super { offset: _, method } => write!(f, "super.{}", method),
        }
    }
}

#[derive(Clone, Debug)]
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

#[derive(Copy, Clone, Debug)]
pub enum LogicalOpType {
    And,
    Or,
}

impl Display for LogicalOpType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LogicalOpType::And => write!(f, "&&"),
            LogicalOpType::Or => write!(f, "||"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct LogicalOp {
    pub optype: LogicalOpType,
    pub offset: usize,
}

impl LogicalOp {
    pub fn new(optype: LogicalOpType, offset: usize) -> Self {
        Self { optype, offset }
    }
}

impl Display for LogicalOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.optype)
    }
}

#[derive(Copy, Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct UnaryOp {
    pub optype: UnaryOpType,
    pub offset: usize,
}

impl UnaryOp {
    pub fn new(optype: UnaryOpType, offset: usize) -> Self {
        Self { optype, offset }
    }
}

impl Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.optype)
    }
}

#[derive(Copy, Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct BinaryOp {
    pub optype: BinaryOpType,
    pub offset: usize,
}

impl BinaryOp {
    pub fn new(optype: BinaryOpType, offset: usize) -> Self {
        Self { optype, offset }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.optype)
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Variable {
    pub name: String,
    pub offset: usize,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Clone, Debug)]
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
    Class {
        var: Variable,
        superclass: Option<Variable>,
        methods: Vec<Stmt>,
    },
}

#[cfg(test)]
mod tests {
    use crate::ast::BinaryOp;
    use crate::ast::BinaryOpType;
    use crate::ast::Expr;
    use crate::ast::Literal;
    use crate::ast::UnaryOp;
    use crate::ast::UnaryOpType;
    use crate::ast::Variable;

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
        assert_eq!(format!("{}", expr), "(* (- 123) (group 45.67))");
    }

    #[test]
    fn test_astprinter_call() {
        let expr = Expr::Call {
            callee: Box::new(Expr::Variable(Variable {
                name: "foo".to_string(),
                offset: 0,
            })),
            paren: 0,
            arguments: vec![
                Expr::Literal(Literal::Number(1.0)),
                Expr::Literal(Literal::String("bar".to_string())),
            ],
        };
        assert_eq!(format!("{}", expr), "foo(1, \"bar\")");
    }
}
