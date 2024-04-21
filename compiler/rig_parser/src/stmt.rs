/*
 * The RIG Programming Language
 * Copyright (C) 2023  MD Gaziur Rahman Noor
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

use crate::ty::{parse_generic_params, parse_ty_path};
use crate::Parser;
use rig_ast::stmt::{ImplStmt, Stmt, StmtKind};
use rig_ast::token::TokenKind;
use rig_errors::CodeError;

pub fn parse_program(parser: &mut Parser) -> Result<Stmt, CodeError> {
    if parser.peek().kind == TokenKind::Impl {
        parse_impl(parser)
    } else {
        parse_decl(parser, parser.peek().kind == TokenKind::Pub)
    }
}

fn parse_impl(parser: &mut Parser) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let generic_params = if parser.peek().kind == TokenKind::Less {
        Some(parse_generic_params(parser, true)?)
    } else {
        None
    };

    let trait_bound = if let TokenKind::Ident(_) = parser.peek().kind {
        let res = Some(parse_ty_path(parser, false)?);
        parser.expect_recoverable(TokenKind::For, "for");
        res
    } else {
        None
    };

    let impl_for = parse_ty_path(parser, false)?;

    parser.expect_recoverable(TokenKind::LBrace, "{");

    // TODO: parse the body

    parser.expect_recoverable(TokenKind::RBrace, "}");

    Ok(Stmt {
        kind: Box::new(StmtKind::Impl(ImplStmt {
            generic_params,
            trait_bound,
            impl_for,
            items: vec![],
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

fn parse_decl(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    if is_pub {
        parser.advance_without_eof()?;
    }

    match parser.peek().kind {
        TokenKind::Struct => todo!(),
        TokenKind::Enum => todo!(),
        TokenKind::Const => todo!(),
        TokenKind::Let => todo!(),
        TokenKind::Static => todo!(),
        TokenKind::Fn => todo!(),
        TokenKind::Mod => todo!(),
        TokenKind::Trait => todo!(),
        _ => Err(CodeError::unexpected_token(parser.current_span())),
    }
}
