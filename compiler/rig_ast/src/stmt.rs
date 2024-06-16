use crate::expr::Expr;
use crate::path::{PathGenericSegment, TyPath};
use rig_intern::InternedString;
use rig_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    pub kind: Box<StmtKind>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {
    Body(BodyStmt),
    Expr(Expr),
    Enum(EnumStmt),
    Struct(StructStmt),
    Impl(ImplStmt),
    Fn(FnStmt),
    Mod(ModStmt),
    Trait(TraitStmt),
    Use(UseStmt),
    For(ForStmt),
    While(WhileStmt),
    Loop(LoopStmt),
    Const(ConstStmt),
    Let(LetStmt),
    Conditional(ConditionalStmt),
    TyAlias(TyAliasStmt),
}

#[derive(Debug, Clone, PartialEq)]
pub struct BodyStmt {
    pub stmts: Vec<Stmt>,
    pub expr: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumStmt {
    pub name: InternedString,
    pub generic_params: PathGenericSegment,
    pub variants: Vec<EnumVariant>,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVariant {
    NoValue(EnumVariantWithNoValue),
    WithValue(EnumVariantWithValue),
    StructLike(EnumVariantStructLike),
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantWithNoValue {
    pub name: InternedString,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantWithValue {
    pub name: InternedString,
    pub ty: TyPath,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantStructLike {
    pub name: InternedString,
    pub properties: Vec<EnumVariantOrStructProperty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructStmt {
    pub name: InternedString,
    pub generic_params: Option<PathGenericSegment>,
    pub properties: Vec<EnumVariantOrStructProperty>,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantOrStructProperty {
    pub name: InternedString,
    pub ty: TyPath,
    pub span: Span,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ImplStmt {
    pub generic_params: Option<PathGenericSegment>,
    pub trait_bound: Option<TyPath>,
    pub impl_for: TyPath,
    pub items: Vec<Stmt>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnStmt {
    pub prototype: FnPrototype,
    pub body: Stmt,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnPrototype {
    pub name: InternedString,
    pub generic_params: PathGenericSegment,
    pub takes_self: bool,
    pub args: Vec<FnArg>,
    pub ret_ty: Option<FnRet>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnRet {
    pub ty: TyPath,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnArg {
    pub kind: FnArgKind,
    pub mutable: Mutable,
    pub moves: bool,
    pub name: InternedString,
    pub ty: TyPath,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnArgKind {
    Anon,
    NotAnon,
    Vararg,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TyAliasStmt {
    pub alias_name: InternedString,
    pub ty: TyPath,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModStmt {
    pub ident: InternedString,
    pub body: Vec<Stmt>,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TraitStmt {
    pub name: InternedString,
    pub inherits_from: Option<Expr>,
    pub generic_params: PathGenericSegment,
    pub body: Vec<Stmt>,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseStmt {
    pub tree: UseStmtTreeNode,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseStmtTreeNode {
    pub name: InternedString,
    pub children: Vec<UseStmtTreeNode>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ForStmt {
    pub ident: InternedString,
    pub iterable: Expr,
    pub body: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileStmt {
    pub cond: Expr,
    pub body: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoopStmt {
    pub body: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchStmt {
    pub expr: Expr,
    pub arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub cond: MatchArmCond,
    pub body: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum MatchArmCond {
    WildCard,
    MatchExprs(Vec<Expr>),
    EnumVariant(EnumVariant),
    BindIf(MatchArmBindIf),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArmEnumVariant {
    pub path: Expr,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArmBindIf {
    pub binding: InternedString,
    pub cond: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConstStmt {
    pub name: InternedString,
    pub ty: TyPath,
    pub expr: Expr,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetStmt {
    pub mutable: Mutable,
    pub name: InternedString,
    pub ty: Option<TyPath>,
    pub expr: Option<Expr>,
    pub pub_: Pub,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Mutable {
    Yes,
    No,
}

impl From<bool> for Mutable {
    fn from(value: bool) -> Self {
        if value {
            Mutable::Yes
        } else {
            Mutable::No
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ConditionalStmt {
    pub condition: Expr,
    pub body: Stmt,
    pub elif: Option<Stmt>,
    pub else_: Stmt,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pub {
    Yes,
    No,
}

impl From<bool> for Pub {
    fn from(value: bool) -> Self {
        if value {
            Pub::Yes
        } else {
            Pub::No
        }
    }
}
