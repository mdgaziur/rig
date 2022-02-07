use crate::expr::Expr;
use crate::function_prototype::Prototype;
use crate::visibility::Visibility;
use rig_span::Span;
use crate::struct_field::StructField;

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

impl Stmt {
    pub fn to_string(&self, block_depth: usize) -> String {
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

                format!("{}fn{:?} {}", vis, prototype, body.to_string(block_depth))
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
                    res += &format!("{}{}\n", "\t".repeat(block_depth + 1), expr.to_string(block_depth + 1));
                }

                res += &format!("{}}}", "\t".repeat(block_depth));
                res
            }
            Stmt::ExprStmt { expr, .. } => format!("{};", expr.to_string()),
            Stmt::BreakStmt { .. } => String::from("break;"),
            Stmt::ContinueStmt { .. } => String::from("continue;"),
        }
    }
}