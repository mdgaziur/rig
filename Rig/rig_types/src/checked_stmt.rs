use crate::checked_expr::CheckedExpr;
use crate::{ScopeId, TypeId};
use rig_span::Span;

#[derive(Debug, Clone)]
pub enum CheckedStmt {
    Let(CheckedLetStmt),
    If(CheckedIfStmt),
    While(CheckedWhileStmt),
    For(CheckedForStmt),
    Print(CheckedPrintStmt),
    Return(CheckedReturnStmt),
    Block(CheckedBlockStmt),
    Expr(CheckedExprStmt),
    Break(CheckedBreakStmt),
    Continue(CheckedContinueStmt),
    Mod,
    Match(CheckedMatchStmt),
    Fn(CheckedFnStmt)
}

#[derive(Debug, Clone)]
pub struct CheckedFnStmt {
    pub ty: TypeId,
    pub body: CheckedBlockStmt,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedLetStmt {
    pub name: String,
    pub var_ty: TypeId,
    pub expr: CheckedExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedIfStmt {
    pub condition: CheckedExpr,
    pub body: CheckedBlockStmt,
    pub then_branch: Option<Box<CheckedIfStmt>>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedWhileStmt {
    pub condition: CheckedExpr,
    pub body: CheckedBlockStmt,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedForStmt {
    pub iterable: CheckedExpr,
    pub var_name: String,
    pub var_type: TypeId,
    pub body: CheckedBlockStmt,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedPrintStmt {
    pub expr: CheckedExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedReturnStmt {
    pub expr: CheckedExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedBlockStmt {
    pub scope_id: ScopeId,
    pub returns: bool,
    pub stmts: Vec<CheckedStmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedExprStmt {
    pub expr: CheckedExpr,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedBreakStmt {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedContinueStmt {
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedMatchStmt {
    pub matched_expr: CheckedExpr,
    pub arms: Vec<CheckedMatchArms>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedMatchArms {
    pub expr: CheckedExpr,
    pub body: CheckedBlockStmt,
    pub span: Span,
}
