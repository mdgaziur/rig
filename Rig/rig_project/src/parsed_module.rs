use rig_ast::stmt::Stmt;
use rig_ast::token::Token;
use rig_error::RigError;
use rig_lexer::Lexer;
use rig_parser::{parse, Parser};
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub module_name: String,
    pub absolute_path: String,
    pub file_content: String,
    pub lexical_tokens: Vec<Token>,
    pub ast: Vec<Stmt>,
    pub lexer_errors: Vec<RigError>,
    pub parser_errors: Vec<RigError>,
}

impl ParsedModule {
    pub fn new(absolute_path: PathBuf, file_content: String) -> Self {
        let module_name = absolute_path
            .file_name()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string()
            .split(".")
            .collect::<Vec<&str>>()
            .get(0)
            .unwrap()
            .to_string();

        let mut lexer = Lexer::new(&file_content, &absolute_path.to_str().unwrap());
        let (lexical_tokens, lexer_errors) = lexer.lex();

        let mut parser = Parser::new(&lexical_tokens);
        let (ast, parser_errors) = parse(&mut parser);

        Self {
            module_name,
            absolute_path: absolute_path.to_str().unwrap().to_string(),
            file_content,
            lexical_tokens,
            ast,
            lexer_errors,
            parser_errors,
        }
    }

    pub fn has_lexer_errors(&self) -> bool {
        !self.lexer_errors.is_empty()
    }

    pub fn print_lexer_errors(&self) {
        for error in &self.lexer_errors {
            error.print(&self.file_content);
        }
    }

    pub fn has_parser_errors(&self) -> bool {
        !self.parser_errors.is_empty()
    }

    pub fn print_parser_errors(&self) {
        for error in &self.parser_errors {
            error.print(&self.file_content);
        }
    }
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ParsedModuleId(pub usize);
