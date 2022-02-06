use crate::expr::Expr;
use crate::function_prototype::Prototype;
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
        methods: Vec<Box<Stmt>>,
        visibility: Visibility,
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
        expr: Box<Stmt>,
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
    }
}

impl ToString for Stmt {
    fn to_string(&self) -> String {
        match self {
            Stmt::UseStmt { path, visibility, .. } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                format!("{}use {};", vis, path.to_string())
            }
            Stmt::StructStmt { .. } => todo!(),
            Stmt::FnStmt { prototype, body, visibility, .. } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                format!("{}fn{:?} {}", vis, prototype, body.to_string())
            },
            Stmt::LetStmt { name,  value, ty, visibility, .. } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                let type_ = match ty {
                    Some(t) => format!(": {}", t.to_string()),
                    None => String::new(),
                };

                format!("{}let {}{} = {};", vis, name, type_, value.to_string())
            }
            Stmt::IfStmt { .. } => todo!(),
            Stmt::WhileStmt { .. } => todo!(),
            Stmt::ForStmt { .. } => todo!(),
            Stmt::PrintStmt { .. } => todo!(),
            Stmt::BlockStmt { exprs, .. } => {
                let mut res = String::from("{");

                if !exprs.is_empty() {
                    res += "\n";
                }
                for expr in exprs {
                    res += &format!("\t{}\n", expr.to_string());
                }

                res += "}";
                res
            }
            Stmt::ExprStmt { expr, .. } => format!("{};", expr.to_string()),
            Stmt::BreakStmt { .. } => String::from("break;"),
            Stmt::ContinueStmt { .. } => String::from("continue;"),
        }
    }
}