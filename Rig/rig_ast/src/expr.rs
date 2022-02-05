use crate::op::{BinaryOperator, LogicalOperator, UnaryOperator};
use rig_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    AssignmentExpr {
        name: String,
        value: Box<Expr>,
        span: Span,
    },
    BinaryExpr {
        lhs: Box<Expr>,
        op: BinaryOperator,
        rhs: Box<Expr>,
        span: Span,
    },
    LogicalExpr {
        lhs: Box<Expr>,
        op: LogicalOperator,
        rhs: Box<Expr>,
        span: Span,
    },
    UnaryExpr {
        op: UnaryOperator,
        rhs: Box<Expr>,
        span: Span,
    },
    GetExpr {
        object: Box<Expr>,
        name: String,
        span: Span,
    },
    PathExpr {
        path: Vec<String>,
        span: Span,
    },
    GroupingExpr {
        expr: Box<Expr>,
        span: Span,
    },
    StringLiteralExpr {
        value: String,
        span: Span,
    },
    IntegerLiteralExpr {
        value: i64,
        span: Span,
    },
    FloatLiteralExpr {
        value: f64,
        span: Span,
    },
    SetExpr {
        object: Box<Expr>,
        name: String,
        value: Box<Expr>,
        span: Span,
    },
    VariableExpr {
        name: String,
        span: Span,
    },
    SelfExpr {
        span: Span,
    },
    CallExpr {
        name: Box<Expr>,
        args: Vec<Box<Expr>>,
        span: Span,
    },
}
