use rig_span::Span;

/// [Token] contains a lexically analyzed token's information
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    /// Token's type
    pub token_type: TokenType,

    /// The part of the source code
    pub lexeme: String,

    /// Literal
    pub literal: String,

    /// Location
    pub span: Span,
}

/// [TokenType] represents a token type.
#[derive(Debug, PartialEq, Clone)]
pub enum TokenType {
    /// reserved keywords
    Keyword,

    /// strings
    StringLiteral,

    /// numbers
    NumberLiteral,

    /// identifiers
    Identifier,

    /// ::
    PathSeparator,

    /// (
    LeftParen,

    /// )
    RightParen,

    /// {
    LeftBrace,

    /// }
    RightBrace,

    /// [
    LeftThirdBracket,

    /// ]
    RightThirdBracket,

    /// ,
    Comma,

    /// :
    Colon,

    /// ::
    Scope,

    /// !
    Bang,

    /// ;
    Semicolon,

    /// +
    Plus,

    /// +=
    PlusEquals,

    /// -
    Minus,

    /// -=
    MinusEquals,

    /// ->
    Arrow,

    /// *
    Multiply,

    /// *=
    MultiplyEquals,

    /// /
    Divide,

    /// /=
    DivideEquals,

    /// %
    Modulus,

    /// %=
    ModulusEquals,

    /// &
    AndOp,

    /// &=
    AndOpEquals,

    /// &&
    And,

    /// |
    OrOp,

    /// ||
    Or,

    /// |=
    OrOpEquals,

    /// =
    Equal,

    /// ==
    EqualEqual,

    /// !=
    NotEqual,

    /// <
    LessThan,

    /// <<
    LeftShift,

    /// <<=
    LeftShiftEquals,

    /// <=
    LessThanOrEquals,

    /// >
    GreaterThan,

    /// >>
    RightShift,

    /// >>=
    RightShiftEquals,

    /// >=
    GreaterThanOrEquals,

    /// ^
    Xor,

    /// ^=
    XorEquals,

    /// .
    Dot,

    /// end of input
    EOF,
}

pub const KEYWORDS: [&str; 22] = [
    "use", "pub", "extern", "fn", "struct", "impl", "for", "loop", "while", "let", "if", "else",
    "self", "true", "false", "null", "in", "continue", "break", "return", "print", "mod"
];
