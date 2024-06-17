use crate::path::TyPath;
use crate::token::{NumberKind, TokenKind};
use rig_intern::InternedString;
use rig_span::Span;
use crate::stmt::Stmt;

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
    Number(NumberExpr),
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
    Path(PathExpr),
    TypeCast(TypeCastExpr),
    Body(Box<BodyExpr>),
    Conditional(Box<ConditionalExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct NumberExpr {
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

impl From<TokenKind> for BinOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Self::Plus,
            TokenKind::PlusEq => Self::Plus,
            TokenKind::Minus => Self::Minus,
            TokenKind::MinusEq => Self::Minus,
            TokenKind::Mul => Self::Multiply,
            TokenKind::MulEq => Self::Multiply,
            TokenKind::Div => Self::Divide,
            TokenKind::DivEq => Self::Divide,
            TokenKind::And => Self::And,
            TokenKind::AndEq => Self::And,
            TokenKind::Or => Self::Or,
            TokenKind::OrEq => Self::Or,
            TokenKind::Xor => Self::Xor,
            TokenKind::XorEq => Self::Xor,
            TokenKind::LShift => Self::LShift,
            TokenKind::LShiftEq => Self::LShift,
            TokenKind::Power => Self::Power,
            TokenKind::PowerEq => Self::Power,
            _ => panic!(
                "`BinOp::From<TokenKind>::from()` called with wrong token kind: `{:?}`!",
                value
            ),
        }
    }
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
    LessEq,
    Greater,
    GreaterEq,
    Eq,
    And,
    Or,
    NotEq,
}

impl From<TokenKind> for LogicalOp {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Less => Self::Less,
            TokenKind::LessEq => Self::LessEq,
            TokenKind::Greater => Self::Greater,
            TokenKind::Eq => Self::Eq,
            TokenKind::NotEq => Self::NotEq,
            TokenKind::And => Self::And,
            TokenKind::Or => Self::Or,
            _ => panic!(
                "`LogicalOp::From<TokenKind>::from()` called with wrong token kind: `{:?}!`",
                value
            ),
        }
    }
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
    pub callable: Box<Expr>,
    pub args: Vec<FnCallArg>,
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
    pub prop: MemberAccessProp,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MemberAccessProp {
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

#[derive(Debug, Clone, PartialEq)]
pub struct PathExpr {
    pub path: TyPath,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeCastExpr {
    pub expr: Box<Expr>,
    pub typ: TyPath,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BodyExpr {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalExpr {
    pub condition: Expr,
    pub body: Expr,
    pub else_: Option<Expr>,
}
