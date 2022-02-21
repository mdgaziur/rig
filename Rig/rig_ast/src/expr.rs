use crate::op::{BinaryOperator, LogicalOperator, UnaryOperator};
use crate::struct_field::StructExprField;
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
        args: Vec<Expr>,
        span: Span,
    },
    StructExpr {
        name: Box<Expr>,
        vals: Vec<StructExprField>,
        span: Span,
    },
}

impl Expr {
    pub fn to_string(&self, depth: usize) -> String {
        match self {
            Expr::AssignmentExpr { name, value, .. } => {
                format!("{} = {}", name, value.to_string(depth))
            }
            Expr::BinaryExpr { lhs, op, rhs, .. } => format!(
                "({} {} {})",
                lhs.to_string(depth),
                op.to_string(),
                rhs.to_string(depth)
            ),
            Expr::LogicalExpr { lhs, op, rhs, .. } => format!(
                "({} {} {})",
                lhs.to_string(depth),
                op.to_string(),
                rhs.to_string(depth)
            ),
            Expr::UnaryExpr { op, rhs, .. } => {
                format!("({}{})", op.to_string(), rhs.to_string(depth))
            }
            Expr::GetExpr { name, object, .. } => format!("{}.{}", object.to_string(depth), name),
            Expr::PathExpr { path, .. } => path.join("::"),
            Expr::GroupingExpr { expr, .. } => format!("({})", expr.to_string(depth)),
            Expr::StringLiteralExpr { value, .. } => format!("{:?}", value),
            Expr::IntegerLiteralExpr { value, .. } => value.to_string(),
            Expr::BooleanLiteralExpr { value, .. } => value.to_string(),
            Expr::NullLiteralExpr { .. } => String::from("null"),
            Expr::FloatLiteralExpr { value, .. } => value.to_string(),
            Expr::SetExpr {
                object,
                name,
                value,
                ..
            } => format!(
                "{}.{} = {}",
                object.to_string(depth),
                name,
                value.to_string(depth)
            ),
            Expr::VariableExpr { name, .. } => name.clone(),
            Expr::SelfExpr { .. } => String::from("self"),
            Expr::CallExpr { name, args, .. } => {
                let mut arg_list = Vec::new();

                for arg in args {
                    arg_list.push(arg.to_string(depth));
                }

                format!("{}({})", name.to_string(depth), arg_list.join(","))
            }
            Expr::StructExpr { name, vals, .. } => {
                let mut string_vals = Vec::new();
                for val in vals {
                    string_vals.push("\t".repeat(depth + 1) + &val.to_string(depth + 1));
                }

                format!(
                    "{} {{\n{}\n{}}}",
                    name.to_string(depth + 1),
                    string_vals.join(",\n"),
                    "\t".repeat(depth)
                )
            }
        }
    }

    pub fn node_name(&self) -> &str {
        match self {
            Expr::AssignmentExpr { .. } => "assignment expression",
            Expr::BinaryExpr { .. } => "binary expression",
            Expr::LogicalExpr { .. } => "logical expression",
            Expr::UnaryExpr { .. } => "unary expression",
            Expr::GetExpr { .. } => "get expression",
            Expr::PathExpr { .. } => "path expression",
            Expr::GroupingExpr { .. } => "grouping expression",
            Expr::StringLiteralExpr { .. } => "string literal",
            Expr::IntegerLiteralExpr { .. } => "integer literal",
            Expr::BooleanLiteralExpr { .. } => "boolean",
            Expr::NullLiteralExpr { .. } => "null",
            Expr::FloatLiteralExpr { .. } => "float literal",
            Expr::SetExpr { .. } => "set expression",
            Expr::VariableExpr { .. } => "variable name",
            Expr::SelfExpr { .. } => "self",
            Expr::CallExpr { .. } => "call expression",
            Expr::StructExpr { .. } => "struct expression",
        }
    }

    pub fn get_span(&self) -> Span {
        // TODO: not proud of this weird way to get span.
        match self {
            Expr::AssignmentExpr { span, .. } => span,
            Expr::BinaryExpr { span, .. } => span,
            Expr::LogicalExpr { span, .. } => span,
            Expr::UnaryExpr { span, .. } => span,
            Expr::GetExpr { span, .. } => span,
            Expr::PathExpr { span, .. } => span,
            Expr::GroupingExpr { span, .. } => span,
            Expr::StringLiteralExpr { span, .. } => span,
            Expr::IntegerLiteralExpr { span, .. } => span,
            Expr::BooleanLiteralExpr { span, .. } => span,
            Expr::NullLiteralExpr { span, .. } => span,
            Expr::FloatLiteralExpr { span, .. } => span,
            Expr::SetExpr { span, .. } => span,
            Expr::VariableExpr { span, .. } => span,
            Expr::SelfExpr { span, .. } => span,
            Expr::CallExpr { span, .. } => span,
            Expr::StructExpr { span, .. } => span,
        }
        .clone()
    }
}
