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
    BooleanLiteralExpr {
        value: bool,
        span: Span,
    },
    NullLiteralExpr {
        span: Span
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

impl ToString for Expr {
    fn to_string(&self) -> String {
        match self {
            Expr::AssignmentExpr { name, value, .. } => format!("{} = {}", name, value.to_string()),
            Expr::BinaryExpr { lhs, op, rhs, .. } => format!("{} {} {}", lhs.to_string(), op.to_string(), rhs.to_string()),
            Expr::LogicalExpr { lhs, op, rhs, .. } => format!("{} {} {}", lhs.to_string(), op.to_string(), rhs.to_string()),
            Expr::UnaryExpr { op, rhs, .. } => format!("{}{}", op.to_string(), rhs.to_string()),
            Expr::GetExpr { name, object, .. } => format!("{}.{}", object.to_string(), name),
            Expr::PathExpr { path, .. } => path.join("::"),
            Expr::GroupingExpr { expr, .. } => format!("({})", expr.to_string()),
            Expr::StringLiteralExpr { value, .. } => format!("\"{}\"", value),
            Expr::IntegerLiteralExpr { value, .. } => value.to_string(),
            Expr::BooleanLiteralExpr { value, .. } => value.to_string(),
            Expr::NullLiteralExpr { .. } => String::from("null"),
            Expr::FloatLiteralExpr { value, .. } => value.to_string(),
            Expr::SetExpr { object, name, value, .. } => format!("{}.{} = {}", object.to_string(), name, value.to_string()),
            Expr::VariableExpr { name, .. } => name.clone(),
            Expr::SelfExpr { .. } => String::from("self"),
            Expr::CallExpr { name, args, .. } => {
                let mut arg_list = Vec::new();

                for arg in args {
                    arg_list.push(arg.to_string());
                }

                format!("{}({})", name.to_string(), arg_list.join(","))
            }
        }
    }
}