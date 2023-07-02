use rig_intern::InternedString;
use rig_span::Span;
use crate::token::NumberKind;

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
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
    Path(PathExpr),
    FnCall(FnCallExpr),
    MemberAccess(MemberAccessExpr),
    Index(IndexExpr),
    Assign(AssignExpr),
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
pub struct PathExpr {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum PathSegment {
    Ident(PathIdentSegment),
    Generic(PathGenericSegment),
    Enum(PathEnumSegment),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathIdentSegment {
    pub ident: InternedString,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathGenericSegment {
    pub tys: Vec<GenericSegmentType>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GenericSegmentType {
    pub ident: InternedString,
    pub trait_bound: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct PathEnumSegment {
    pub items: Vec<EnumSegmentItems>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumSegmentItems {
    pub ident: InternedString,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnCallExpr {
    pub path: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccessExpr {
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
