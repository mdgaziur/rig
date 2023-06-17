use rig_intern::InternedString;
use rig_span::Span;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LexicalToken {
    pub(crate) kind: TokenKind,
    pub(crate) raw: InternedString,
    pub(crate) span: Span,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NumberKind {
    Bin,
    Oct,
    Hex,
    Dec,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum TokenKind {
    // Kinds that represent values
    String(InternedString),
    Number {
        number: InternedString,
        kind: NumberKind,
    },
    True,
    False,
    Ident(InternedString),

    // Keywords
    Pub,
    Struct,
    Fn,
    Trait,
    Mut,
    Const,
    Static,
    Type,
    Use,
    For,
    While,
    Loop,
    If,
    Else,
    Anon,
    Vararg,
    Let,
    Where,
    Impl,
    In,
    Mod,

    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Assign,
    Eq,
    NotEq,
    Less,
    LessEq,
    Greater,
    GreaterEq,
    Plus,
    PlusEq,
    Minus,
    MinusEq,
    Mul,
    MulEq,
    Div,
    DivEq,
    LShift,
    LShiftEq,
    RShift,
    RShiftEq,
    And,
    LogicalAnd,
    AndEq,
    Or,
    LogicalOr,
    OrEq,
    Xor,
    XorEq,
    Not,
    BinaryNot,
    Power,
    PowerEq,
    Colon,
    PathSep,
    Comma,
    Semi,
    Dot,
}
