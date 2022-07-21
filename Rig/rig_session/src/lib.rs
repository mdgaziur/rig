use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct Session {
    /// Paths to search for while importing
    pub search_paths: Vec<PathBuf>,

    /// Output debug information
    pub debug: DebugInfo,
}

#[derive(Debug, Clone)]
pub enum DebugInfo {
    LexicalTokens,
    AST,
    Bytecode,
    None,
}
