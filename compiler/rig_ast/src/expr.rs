use crate::path::TyPath;
use crate::token::NumberKind;
use rig_intern::InternedString;
use rig_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    String(InternedString),
    Number(Number),
    Boolean(bool),
    Array(Vec<Expr>),
    Bin(BinExpr),
    Logical(LogicalExpr),
    Unary(UnaryExpr),
    FnCall(FnCallExpr),
    MemberAccess(MemberAccessExpr),
    Index(IndexExpr),
    Assign(AssignExpr),
    Struct(StructExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    pub value: InternedString,
    pub kind: NumberKind,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinExpr {
    pub lhs: Box<Expr>,
    pub op: BinOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Plus,
    Minus,
    Multiply,
    Divide,
    And,
    Or,
    Xor,
    LShift,
    RShift,
    Power,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LogicalExpr {
    pub lhs: Box<Expr>,
    pub op: LogicalOp,
    pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOp {
    Less,
    Greater,
    Eq,
    And,
    Or,
    NotEq,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryExpr {
    pub op: UnaryOp,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Not,
    Neg,
    BitwiseNot,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExpr {
    pub path: TyPath,
    pub values: Vec<StructExprProperty>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructExprProperty {
    pub name: InternedString,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCallExpr {
    pub path: TyPath,
    pub args: Vec<FnCallArg>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCallArg {
    pub ty: FnCallArgKind,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnCallArgKind {
    NotAnon(InternedString, Span),
    Anon,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccessExpr {
    pub expr: Box<Expr>,
    pub segments: Vec<MemberAccessSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccessSegment {
    pub ident: InternedString,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IndexExpr {
    pub indexable: Box<Expr>,
    pub index: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignExpr {
    pub assignee: Box<Expr>,
    pub value: Box<Expr>,
}
