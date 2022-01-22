use std::iter::Map;

pub struct RigSession {
    /// Files consumed by the interpreter
    pub files: Map<String, String>,

    /// Output debug information
    pub debug: DebugInfo,
}

pub enum DebugInfo {
    LexicalTokens,
    AST,
    Bytecode,
}
