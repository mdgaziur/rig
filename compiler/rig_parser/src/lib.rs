#![feature(let_chains)]

pub mod expr;
pub mod stmt;
mod ty;

use crate::stmt::parse_program;
use rig_ast::stmt::Stmt;
use rig_ast::token::{LexicalToken, TokenKind};
use rig_errors::{CodeError, Diagnostic, ErrorCode};
use rig_intern::{intern, InternedString};
use rig_span::Span;

/// Recursive descendant parser for RIG programming language.
///
/// ## Design decisions:
///
/// Functions that do the actual parsing generally have the following signature:
///
///  `fn parse_XXX(parser: &mut Parser) -> Result<..., CodeError>`
///
/// The functions try as much as possible to recover from user errors. When an unrecoverable error
/// is encountered, the function returns a [`Result::Err`] containing a [`CodeError`] describing the
/// error. In that case, the parser collects that error, tries to "synchronize" and then continues
/// parsing. The synchronization mechanism is a fallback hence the parser functions should try the best
/// to avoid invoking this.
#[derive(Debug)]
pub struct Parser<'p> {
    tokens: &'p [LexicalToken],
    diags: Vec<CodeError>,
    pos: usize,
    parsing_condition: Vec<()>,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: &'p [LexicalToken]) -> Self {
        Self {
            tokens,
            diags: vec![],
            pos: 0,
            parsing_condition: vec![],
        }
    }

    pub fn parse(&mut self) -> Vec<Stmt> {
        let mut stmts = vec![];

        while !self.is_eof() {
            match parse_program(self) {
                Ok(stmt) => stmts.push(stmt),
                Err(e) => {
                    self.synchronize(&[]);
                    self.diags.push(e)
                }
            }
        }

        stmts
    }

    pub fn synchronize(&mut self, ignore: &[TokenKind]) {
        while !self.is_eof() {
            if !ignore.contains(&self.peek().kind) {
                match self.peek().kind {
                    TokenKind::Semi => {
                        self.advance();
                        return;
                    }
                    TokenKind::Fn
                    | TokenKind::Const
                    | TokenKind::Let
                    | TokenKind::Struct
                    | TokenKind::Impl
                    | TokenKind::Trait
                    | TokenKind::Use
                    | TokenKind::Mod => {
                        return;
                    }
                    _ => (),
                }
            }
            self.advance();
        }
    }

    fn get_span_for_expectation(&self) -> Span {
        if self.is_eof() {
            self.previous().span
        } else {
            self.current_span()
        }
    }

    fn check_eof_with_expectation(&mut self, name: &str) -> Result<(), CodeError> {
        if self.is_eof() {
            Err(CodeError {
                error_code: ErrorCode::SyntaxError,
                message: intern!("unexpected eof after this"),
                hints: vec![Diagnostic {
                    message: intern!(format!("Expected {name} after this")),
                    pos: self.previous().span,
                }],
                notes: vec![],
                pos: self.previous().span,
            })
        } else {
            Ok(())
        }
    }

    pub fn expect(&mut self, token_kind: TokenKind, name: &str) -> Result<LexicalToken, CodeError> {
        self.check_eof_with_expectation(name)?;

        let tok = self.consume();
        if tok.kind != token_kind {
            self.go_back();
            Err(CodeError::unexpected_token_with_hint(
                self.get_span_for_expectation(),
                format!("expected a `{name}` here"),
            ))
        } else {
            Ok(tok)
        }
    }

    pub fn expect_recoverable(
        &mut self,
        token_kind: TokenKind,
        name: &str,
    ) -> (LexicalToken, bool) {
        if let Err(e) = self.check_eof_with_expectation(name) {
            self.diags.push(e);
            return (self.peek(), false);
        }
        let tok = self.consume();

        if tok.kind != token_kind {
            self.go_back();
            self.diags.push(CodeError::unexpected_token_with_hint(
                self.get_span_for_expectation(),
                format!("expected a `{name}` here"),
            ));
            (tok, false)
        } else {
            (tok, true)
        }
    }

    pub fn expect_ident(&mut self) -> Result<(InternedString, Span), CodeError> {
        self.check_eof_with_expectation("an identifier")?;

        let token = self.consume();

        match token.kind {
            TokenKind::Ident(ident) => Ok((ident, token.span)),
            _ => {
                self.go_back();
                Err(CodeError {
                    error_code: ErrorCode::SyntaxError,
                    message: intern!("expected an identifier"),
                    pos: self.get_span_for_expectation(),
                    hints: vec![],
                    notes: vec![],
                })
            }
        }
    }

    #[must_use]
    pub fn consume(&mut self) -> LexicalToken {
        let curr = self.peek();
        self.advance();

        curr
    }

    #[must_use]
    pub fn peek(&self) -> LexicalToken {
        self.tokens[self.pos]
    }

    pub fn try_peek(&self) -> Option<&LexicalToken> {
        self.tokens.get(self.pos)
    }

    pub fn try_peek_next(&self, n: usize) -> Option<&LexicalToken> {
        self.tokens.get(self.pos + n)
    }

    pub fn current_span(&self) -> Span {
        self.peek().span
    }

    pub fn previous(&self) -> LexicalToken {
        self.tokens[self.pos - 1]
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    pub fn advance_without_eof(&mut self) -> Result<(), CodeError> {
        self.advance();

        if self.is_eof() {
            Err(CodeError {
                error_code: ErrorCode::SyntaxError,
                message: intern!("unexpected EOF after this"),
                pos: self.previous().span,
                hints: vec![],
                notes: vec![],
            })
        } else {
            Ok(())
        }
    }

    pub fn go_back(&mut self) {
        self.pos -= 1;
    }

    pub fn is_eof(&self) -> bool {
        self.peek().kind == TokenKind::Eof
    }

    pub fn get_diags(self) -> Vec<CodeError> {
        self.diags
    }
}
