mod stmt;
mod expr;

use rig_ast::expr::Expr;
use rig_ast::stmt::Stmt;
use rig_ast::token::{Token, TokenType};
use rig_ast::visibility::Visibility;
use rig_error::{ErrorType, RigError};
use rig_span::Span;
use crate::expr::path;
use crate::stmt::program;

pub struct Parser<'p> {
    source: &'p str,
    source_path: &'p str,
    lexical_tokens: &'p [Token],
    pos: usize,
}

impl<'p> Parser<'p> {
    pub fn new(source: &'p str, source_path: &'p str, lexical_tokens: &'p [Token]) -> Self {
        Self {
            source,
            source_path,
            lexical_tokens,
            pos: 0,
        }
    }

    fn is_eof(&self) -> bool {
        self.check(TokenType::EOF)
    }

    /// ## Panics
    /// May panic when EOF isn`t handled correctly.
    /// Example:
    /// 1. When the lexer doesn`t emit EOF and the parser doesn`t stop eating tokens.
    /// 2. The parser doesn`t handle EOF correctly and keeps eating tokens
    fn peek(&self) -> &Token {
        self.lexical_tokens.get(self.pos).unwrap()
    }

    fn try_peek_next(&self) -> Option<&Token> {
        self.lexical_tokens.get(self.pos + 1)
    }

    /// ## Panics
    /// May panic when called at wrong time(calling it when parser.pos is 0)
    fn previous(&self) -> &Token {
        &self.lexical_tokens[self.pos - 1]
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.peek().token_type == token_type
    }

    fn consume(
        &mut self,
        token_type: TokenType,
        message: &str,
        hint: Option<String>,
    ) -> Result<&Token, RigError> {
        if self.check(token_type) {
            self.advance();
            return Ok(self.previous());
        }

        if self.is_eof() {
            return Err(RigError {
                error_type: ErrorType::Hard,
                error_code: String::from("E0005"),
                span: self.peek().span.clone(),
                message: format!("{}, but found unexpected eof", message),
                hint,
                file_path: self.source_path.to_string(),
            })
        }
        Err(RigError {
            error_type: ErrorType::Hard,
            error_code: String::from("E0005"),
            span: self.peek().span.clone(),
            message: format!("{}, but found `{}`", message, self.peek().lexeme),
            hint,
            file_path: self.source_path.to_string(),
        })
    }

    fn synchronize(&mut self) {
        if self.is_eof() {
            return;
        }
        self.advance();

        while !self.is_eof() {
            if let TokenType::Semicolon = self.previous().token_type {
                return;
            }

            match self.peek().token_type {
                TokenType::Keyword => return,
                _ => (),
            }

            self.advance();
        }
    }
}

/// Return: (AST, hadError)
pub fn parse(parser: &mut Parser, file_content: &str) -> (Vec<Stmt>, bool) {
    let mut statements = Vec::new();
    let mut had_error = false;

    while !parser.is_eof() {
        match program(parser) {
            Ok(stmt) => statements.push(stmt),
            Err(e) => {
                had_error = true;
                e.print(file_content);
                parser.synchronize();
            }
        }
    }

    (statements, had_error)
}

fn name_with_type(parser: &mut Parser) -> Result<(Token, Expr), RigError> {
    let name = parser.consume(TokenType::Identifier, "Expected name", None)?.clone();
    let _ = parser.consume(TokenType::Colon, "Expected colon after name", None)?;
    let ty = path(parser)?;

    Ok((name, ty))
}