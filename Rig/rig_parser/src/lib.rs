mod expr;
mod stmt;

use crate::expr::path;
use crate::stmt::program;
use rig_ast::expr::Expr;
use rig_ast::stmt::Stmt;
use rig_ast::token::{Token, TokenType};

use rig_error::{ErrorCode, ErrorType, RigError};

pub struct Parser<'p> {
    source_path: &'p str,
    lexical_tokens: &'p [Token],
    pos: usize,
    /// Errors inside block statement
    block_stmt_errs: Vec<RigError>,
}

impl<'p> Parser<'p> {
    pub fn new(source_path: &'p str, lexical_tokens: &'p [Token]) -> Self {
        Self {
            source_path,
            lexical_tokens,
            pos: 0,
            block_stmt_errs: vec![],
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
                error_code: ErrorCode::E0005,
                span: self.peek().span.clone(),
                message: format!("{}, but found unexpected eof", message),
                hint,
                file_path: self.source_path.to_string(),
            });
        }
        Err(RigError {
            error_type: ErrorType::Hard,
            error_code: ErrorCode::E0005,
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

            if self.peek().token_type == TokenType::Keyword {
                match self.peek().lexeme.as_str() {
                    "struct" | "let" | "if" | "while" | "loop" | "for" | "fn" | "pub"
                    | "return" | "use" | "impl" | "continue" | "break" => break,
                    _ => (),
                }
            }

            self.advance();
        }
    }
}

/// Return: (AST, hadError)
pub fn parse(parser: &mut Parser) -> (Vec<Stmt>, Vec<RigError>) {
    let mut statements = Vec::new();
    let mut errs = Vec::new();

    while !parser.is_eof() {
        match program(parser) {
            Ok(stmt) => {
                if !parser.block_stmt_errs.is_empty() {
                    errs.extend(parser.block_stmt_errs.clone());

                    // clear the block statement errors list for next batch of errors in block statements
                    parser.block_stmt_errs.clear();
                }

                statements.push(stmt)
            }
            Err(e) => {
                errs.push(e);
                parser.synchronize();
            }
        }
    }

    (statements, errs)
}

fn name_with_type(parser: &mut Parser) -> Result<(Token, Expr), RigError> {
    let name = parser
        .consume(TokenType::Identifier, "Expected name", None)?
        .clone();
    parser.consume(TokenType::Colon, "Expected colon after name", None)?;
    let ty = path(parser)?;

    Ok((name, ty))
}
