use crate::TypeId;
use rig_ast::op::{BinaryOperator, LogicalOperator, UnaryOperator};
use rig_span::Span;

#[derive(Debug, Clone)]
pub enum CheckedExpr {
    Variable(CheckedVariable),
    Int(CheckedInteger),
    Boolean(CheckedBoolean),
    Float(CheckedFloat),
    String(CheckedString),
    Null(CheckedNull),
    SelfLit(CheckedSelf),
    Grouping(CheckedGroupingExpr),
    Get(CheckedGet),
    Set(CheckedSet),
    Path(CheckedPath),
    Call(CheckedCall),
    Struct(CheckedStruct),
    Assignment(CheckedAssignment),
    Binary(CheckedBinary),
    Logical(CheckedLogical),
    Unary(CheckedUnary),
}

#[derive(Debug, Clone)]
pub struct CheckedVariable {
    pub name: String,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedInteger {
    pub value: i64,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedBoolean {
    pub value: bool,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedFloat {
    pub value: f64,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedString {
    pub value: String,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedNull {
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedSelf {
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedGroupingExpr {
    pub expr: Box<CheckedExpr>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedGet {
    pub object: Box<CheckedExpr>,
    pub name: String,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedSet {
    pub object: Box<CheckedExpr>,
    pub name: String,
    pub value: Box<CheckedExpr>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedPath {
    pub path: Vec<String>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedCall {
    pub name: Box<CheckedExpr>,
    pub args: Vec<Box<CheckedExpr>>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedStruct {
    pub name: Box<CheckedExpr>,
    pub fields: Vec<CheckedStructField>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedStructField {
    pub name: String,
    pub value: Box<CheckedExpr>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedAssignment {
    pub name: String,
    pub value: Box<CheckedExpr>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedBinary {
    pub lhs: Box<CheckedExpr>,
    pub op: BinaryOperator,
    pub rhs: Box<CheckedExpr>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedLogical {
    pub lhs: Box<CheckedExpr>,
    pub op: LogicalOperator,
    pub rhs: Box<CheckedExpr>,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct CheckedUnary {
    pub op: UnaryOperator,
    pub rhs: Box<CheckedExpr>,
    pub ty: TypeId,
    pub span: Span,
}
