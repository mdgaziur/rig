mod expr;
mod stmt;

use crate::expr::path;
use crate::stmt::program;
use rig_ast::expr::Expr;
use rig_ast::stmt::Stmt;
use rig_ast::token::{Token, TokenType};

use rig_error::{ErrorCode, ErrorType, RigError};

pub struct Parser<'p> {
    lexical_tokens: &'p [Token],
    pos: usize,
    /// Errors inside block statement
    block_stmt_errs: Vec<RigError>,
}

impl<'p> Parser<'p> {
    pub fn new(lexical_tokens: &'p [Token]) -> Self {
        Self {
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

    /// # Panics
    /// Will panic if position is not set correctly
    fn set_position(&mut self, pos: usize) {
        self.pos = pos;
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, RigError> {
        if self.check(token_type) {
            self.advance();
            return Ok(self.previous());
        }

        if self.is_eof() {
            return Err(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0005,
                &format!("{}, but found unexpected eof", message),
                self.peek().span.clone(),
            ));
        }
        Err(RigError::with_no_hint_and_notes(
            ErrorType::Hard,
            ErrorCode::E0005,
            &format!("{}, but found `{}`", message, self.peek().lexeme),
            self.peek().span.clone(),
        ))
    }

    fn synchronize(&mut self) {
        if self.is_eof() || self.peek().token_type == TokenType::RightBrace {
            return;
        }
        self.advance();

        loop {
            match self.previous().token_type {
                TokenType::Semicolon | TokenType::RightBrace => return,
                TokenType::LeftBrace => {
                    self.set_position(self.pos - 1);
                    return;
                }
                _ => (),
            }

            if self.is_eof() {
                break;
            }

            if self.peek().token_type == TokenType::Keyword {
                match self.peek().lexeme.as_str() {
                    "struct" | "let" | "if" | "while" | "loop" | "for" | "fn" | "pub"
                    | "return" | "use" | "impl" | "continue" | "break" => break,
                    _ => (),
                }
            } else if self.peek().token_type == TokenType::RightBrace {
                return;
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

                let before_sync = parser.peek().clone();
                parser.synchronize();
                if *parser.peek() == before_sync && !parser.is_eof() {
                    parser.advance();
                }
            }
        }
    }

    (statements, errs)
}

fn name_with_type(parser: &mut Parser) -> Result<(Token, Expr), RigError> {
    let name = parser
        .consume(TokenType::Identifier, "Expected name")?
        .clone();
    parser.consume(TokenType::Colon, "Expected colon after name")?;
    let ty = path(parser)?;

    Ok((name, ty))
}
