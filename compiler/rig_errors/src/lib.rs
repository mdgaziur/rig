use rig_intern::InternedString;
use rig_span::Span;

pub enum CompilerError {
    ErrorsInCode(Vec<CodeError>),
    OtherError(InternedString),
}

#[derive(Debug, Clone)]
pub struct CodeError {
    pub message: InternedString,
    pub hints: Vec<CodeHint>,
    pub notes: Vec<CodeNote>,
    pub error_code: ErrorCode,
    pub pos: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    SyntaxError,
}

impl ErrorCode {
    pub fn to_error_code(&self) -> &str {
        match self {
            ErrorCode::SyntaxError => "E0001",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeHint {
    message: InternedString,
    pos: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct CodeNote {
    message: InternedString,
    pos: Span,
}
