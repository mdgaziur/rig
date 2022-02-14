use crate::expr::Expr;
use crate::function_prototype::Prototype;
use crate::struct_field::StructField;
use crate::visibility::Visibility;
use rig_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    UseStmt {
        path: Expr,
        visibility: Visibility,
        span: Span,
    },
    StructStmt {
        name: String,
        fields: Vec<StructField>,
        visibility: Visibility,
        span: Span,
    },
    ImplStmt {
        struct_name: String,
        methods: Vec<Box<Stmt>>,
        span: Span,
    },
    FnStmt {
        prototype: Prototype,
        body: Box<Stmt>,
        visibility: Visibility,
        span: Span,
    },
    LetStmt {
        name: String,
        ty: Option<Expr>,
        value: Expr,
        visibility: Visibility,
        span: Span,
    },
    IfStmt {
        condition: Expr,
        body: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
    WhileStmt {
        condition: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    ForStmt {
        var: String,
        iterable: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    PrintStmt {
        expr: Expr,
        span: Span,
    },
    ReturnStmt {
        expr: Expr,
        span: Span,
    },
    BlockStmt {
        exprs: Vec<Box<Stmt>>,
        span: Span,
    },
    ExprStmt {
        expr: Expr,
        span: Span,
    },
    BreakStmt {
        span: Span,
    },
    ContinueStmt {
        span: Span,
    },
}

impl Stmt {
    pub fn to_string(&self, block_depth: usize) -> String {
        let res = match self {
            Stmt::UseStmt {
                path, visibility, ..
            } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                format!("{}use {};", vis, path.to_string(block_depth))
            }
            Stmt::StructStmt {
                visibility,
                name,
                fields,
                ..
            } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                let mut res = format!("{}struct {} {{", vis, name);
                if !fields.is_empty() {
                    res.push('\n');
                }

                for field in fields {
                    res += &format!("{}{},\n", "\t".repeat(block_depth + 1), field.to_string());
                }

                res += &format!("{}}}", "\t".repeat(block_depth));
                res
            }
            Stmt::ImplStmt { struct_name, methods, .. } => {
                let mut stringified_methods = methods.iter()
                    .map(|m| "\t".repeat(block_depth + 1) + &m.to_string(block_depth + 1))
                    .collect::<Vec<String>>()
                    .join("\n");
                if !stringified_methods.is_empty() {
                    stringified_methods.push('\n');
                }

                format!("impl {} {{\n{}{}}}\n", struct_name, stringified_methods, "\t".repeat(block_depth))
            },
            Stmt::FnStmt {
                prototype,
                body,
                visibility,
                ..
            } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                format!("{}fn{:?} {}", vis, prototype, body.to_string(block_depth))
            }
            Stmt::LetStmt {
                name,
                value,
                ty,
                visibility,
                ..
            } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                let type_ = match ty {
                    Some(t) => format!(": {}", t.to_string(block_depth)),
                    None => String::new(),
                };

                format!(
                    "{}let {}{} = {};",
                    vis,
                    name,
                    type_,
                    value.to_string(block_depth)
                )
            }
            Stmt::IfStmt { condition, body, else_branch, .. } => {
                let top = format!("if ({}) {}", condition.to_string(block_depth), body.to_string(block_depth));

                if let Some(else_branch) = else_branch {
                    format!("{} else {}", top, else_branch.to_string(block_depth))
                } else {
                    top
                }
            },
            Stmt::WhileStmt { condition, body, .. } => {
                format!("while ({}) {}",
                        condition.to_string(block_depth),
                    body.to_string(block_depth),
                )
            }
            Stmt::ForStmt { var, iterable, body, .. } => {
                format!("for ({} in {}) {}", var, iterable.to_string(block_depth), body.to_string(block_depth))
            },
            Stmt::PrintStmt { .. } => todo!(),
            Stmt::BlockStmt { exprs, .. } => {
                let mut res = vec![String::from("{")];

                for expr in exprs {
                    res.push(format!(
                        "{}{}",
                        "\t".repeat(block_depth + 1),
                        expr.to_string(block_depth + 1)
                    ));
                }

                res.push(format!("{}}}", "\t".repeat(block_depth)));
                res.join("\n")
            }
            Stmt::ExprStmt { expr, .. } => format!("{};", expr.to_string(block_depth)),
            Stmt::BreakStmt { .. } => String::from("break;"),
            Stmt::ContinueStmt { .. } => String::from("continue;"),
            Stmt::ReturnStmt { expr, .. } => format!("return {};", expr.to_string(block_depth)),
        };

        res
    }
}
