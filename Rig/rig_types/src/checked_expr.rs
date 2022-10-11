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

impl CheckedExpr {
    pub fn ty(&self) -> TypeId {
        match self {
            CheckedExpr::Variable(var) => var.ty,
            CheckedExpr::Int(int) => int.ty,
            CheckedExpr::Boolean(bool) => bool.ty,
            CheckedExpr::Float(float) => float.ty,
            CheckedExpr::String(string) => string.ty,
            CheckedExpr::Null(null) => null.ty,
            CheckedExpr::SelfLit(self_lit) => self_lit.ty,
            CheckedExpr::Grouping(grouping) => grouping.ty,
            CheckedExpr::Get(get) => get.ty,
            CheckedExpr::Set(set) => set.ty,
            CheckedExpr::Path(path) => path.ty,
            CheckedExpr::Call(call) => call.ty,
            CheckedExpr::Struct(struct_) => struct_.ty,
            CheckedExpr::Assignment(assignment) => assignment.ty,
            CheckedExpr::Binary(binary) => binary.ty,
            CheckedExpr::Logical(logical) => logical.ty,
            CheckedExpr::Unary(unary) => unary.ty,
        }
    }

    pub fn span(&self) -> Span {
        match self {
            CheckedExpr::Variable(var) => var.span.clone(),
            CheckedExpr::Int(int) => int.span.clone(),
            CheckedExpr::Boolean(bool) => bool.span.clone(),
            CheckedExpr::Float(float) => float.span.clone(),
            CheckedExpr::String(string) => string.span.clone(),
            CheckedExpr::Null(null) => null.span.clone(),
            CheckedExpr::SelfLit(self_lit) => self_lit.span.clone(),
            CheckedExpr::Grouping(grouping) => grouping.span.clone(),
            CheckedExpr::Get(get) => get.span.clone(),
            CheckedExpr::Set(set) => set.span.clone(),
            CheckedExpr::Path(path) => path.span.clone(),
            CheckedExpr::Call(call) => call.span.clone(),
            CheckedExpr::Struct(struct_) => struct_.span.clone(),
            CheckedExpr::Assignment(assignment) => assignment.span.clone(),
            CheckedExpr::Binary(binary) => binary.span.clone(),
            CheckedExpr::Logical(logical) => logical.span.clone(),
            CheckedExpr::Unary(unary) => unary.span.clone(),
        }
    }
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
