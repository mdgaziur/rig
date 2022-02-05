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
}
