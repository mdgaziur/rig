#![feature(let_chains)]

pub mod expr;
pub mod stmt;
mod ty;

use crate::stmt::parse_program;
use rig_ast::stmt::Stmt;
use rig_ast::token::{LexicalToken, TokenKind};
use rig_errors::{CodeError, Diagnostic, ErrorCode};
use rig_intern::{intern, InternedString};
use rig_lexer::Lexer;
use rig_session::Session;
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
    do_not_parse_empty_struct: usize,
}

impl<'p> Parser<'p> {
    pub fn new(tokens: &'p [LexicalToken]) -> Self {
        Self {
            tokens,
            diags: vec![],
            pos: 0,
            do_not_parse_empty_struct: 0,
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
        if self.is_eof() || self.pos != 0 {
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

    pub fn expect(&mut self, token_kind: TokenKind) -> Result<LexicalToken, CodeError> {
        self.check_eof_with_expectation(token_kind.name())?;

        let tok = self.consume();
        if tok.kind != token_kind {
            self.go_back();
            Err(CodeError::unexpected_token_with_hintpos(
                tok.span,
                format!("expected a `{}` after this", token_kind.name()),
                self.previous().span,
            ))
        } else {
            Ok(tok)
        }
    }

    pub fn expect_recoverable(&mut self, token_kind: TokenKind) -> (LexicalToken, bool) {
        if let Err(e) = self.check_eof_with_expectation(token_kind.name()) {
            self.diags.push(e);
            return (self.peek(), false);
        }
        let tok = self.consume();

        if tok.kind != token_kind {
            self.go_back();
            self.diags.push(CodeError::unexpected_token_with_hintpos(
                tok.span,
                format!("expected a `{}` after this", token_kind.name()),
                self.previous().span,
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
                    message: intern!("expected an identifier after this"),
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
}

pub fn parse_module(session: &mut Session, module_path: InternedString) -> bool {
    let mut had_error = false;
    let debug = session.debug;
    let debug_pretty = session.debug_pretty;
    let module = session.get_module_mut(module_path).unwrap();
    let file_path = module.file_path;
    let file_content = module.file_content.get();

    let lex_res = Lexer::new(file_content.chars(), file_path).lex();
    let mut lexical_tokens = vec![];

    let mut diags = vec![];
    for res in lex_res {
        match res {
            Ok(token) => lexical_tokens.push(token),
            Err(e) => {
                had_error = true;
                diags.push(e);
            }
        }
    }

    let mut parser = Parser::new(&lexical_tokens);
    let ast = parser.parse();
    if debug {
        eprintln!(
            "\x1b[033m\x1b[1m[Start Debug]\n\
        File: {}:{}:{}\n\
        Generated AST from source file `{file_path}`:",
            file!(),
            line!(),
            column!()
        );

        if debug_pretty {
            eprintln!("{:#?}\n", ast);
        } else {
            eprintln!("{:?\n}", ast);
        }

        eprintln!("[End Debug]\x1b[0m");
    }
    module.ast = ast;

    had_error = had_error || !parser.diags.is_empty();
    diags.extend(parser.diags);
    for diag in diags {
        diag.display(session);
    }

    had_error
}
