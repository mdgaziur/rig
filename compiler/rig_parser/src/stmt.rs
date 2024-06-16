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

use crate::expr::parse_expr;
use crate::ty::{parse_generic_params, parse_ty_path};
use crate::Parser;
use rig_ast::path::PathSegment;
use rig_ast::stmt::{
    ConstStmt, EnumVariantOrStructProperty, ImplStmt, LetStmt, Mutable, Pub, Stmt, StmtKind,
    StructStmt, UseStmt, UseStmtTreeNode,
};
use rig_ast::token::TokenKind;
use rig_ast::token::TokenKind::PathSep;
use rig_errors::{CodeError, ErrorCode};

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

    let impl_for_span_start = parser.current_span();
    let impl_for = parse_ty_path(parser, false)?;
    let impl_for_span_end = parser.previous().span;
    if matches!(impl_for.segments[0], PathSegment::Generic(_)) {
        parser.diags.push(CodeError::unexpected_token_with_note(
            impl_for_span_start.merge(impl_for_span_end),
            "expected a type path, not a list of generic params",
        ));
    }

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
        TokenKind::Struct => parse_struct_decl(parser, is_pub),
        TokenKind::Enum => todo!(),
        TokenKind::Const => parse_var_decl(parser, is_pub, VarDeclType::Const),
        TokenKind::Let => parse_var_decl(parser, is_pub, VarDeclType::Let),
        TokenKind::Fn => todo!(),
        TokenKind::Mod => todo!(),
        TokenKind::Trait => todo!(),
        TokenKind::Use => parse_use(parser, is_pub),
        _ => Err(CodeError::unexpected_token(parser.current_span())),
    }
}

fn parse_struct_decl(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_span = parser.current_span();
    parser.advance_without_eof()?;

    let (name, _) = parser.expect_ident()?;
    let generic_params = if parser.peek().kind == TokenKind::Less {
        Some(parse_generic_params(parser, true)?)
    } else {
        None
    };

    parser.expect_recoverable(TokenKind::LBrace, "left brace");
    let mut properties = vec![];
    while !parser.is_eof() && parser.peek().kind != TokenKind::RBrace {
        let field_span_start = parser.current_span();
        let is_pub = if parser.peek().kind == TokenKind::Pub {
            parser.advance_without_eof()?;
            true
        } else {
            false
        };
        let (property_name, _) = parser.expect_ident()?;
        parser.expect_recoverable(TokenKind::Colon, "colon");
        let ty_path = parse_ty_path(parser, false)?;
        properties.push(EnumVariantOrStructProperty {
            name: property_name,
            ty: ty_path,
            span: field_span_start.merge(parser.previous().span),
            pub_: Pub::from(is_pub),
        });
        if parser.peek().kind != TokenKind::RBrace {
            parser.expect_recoverable(TokenKind::Comma, "comma");
        }
    }
    parser.expect_recoverable(TokenKind::RBrace, "right brace");

    Ok(Stmt {
        kind: Box::new(StmtKind::Struct(StructStmt {
            name,
            properties,
            generic_params,
            pub_: Pub::from(is_pub),
        })),
        span: start_span.merge(parser.previous().span),
    })
}

#[derive(PartialEq)]
enum VarDeclType {
    Let,
    Const,
}

fn parse_var_decl(
    parser: &mut Parser,
    is_pub: bool,
    decl_type: VarDeclType,
) -> Result<Stmt, CodeError> {
    let start_span = if is_pub {
        parser.previous().span
    } else {
        parser.peek().span
    };

    parser.advance_without_eof()?;

    let mutable = if decl_type == VarDeclType::Let && parser.peek().kind == TokenKind::Mut {
        parser.advance_without_eof()?;
        Mutable::Yes
    } else {
        Mutable::No
    };

    let (name, _) = parser.expect_ident()?;

    let ty = if parser.peek().kind == TokenKind::Colon {
        parser.advance_without_eof()?;
        Some(parse_ty_path(parser, false)?)
    } else {
        None
    };

    let expr = if parser.peek().kind == TokenKind::Assign {
        parser.advance_without_eof()?;
        Some(parse_expr(parser)?)
    } else {
        None
    };

    parser.expect_recoverable(TokenKind::Semi, "semicolon");

    match decl_type {
        VarDeclType::Let => Ok(Stmt {
            kind: Box::new(StmtKind::Let(LetStmt {
                name,
                ty,
                mutable,
                expr,
                pub_: Pub::from(is_pub),
            })),
            span: start_span.merge(parser.previous().span),
        }),
        VarDeclType::Const => {
            let span = start_span.merge(parser.previous().span);

            if ty.is_none() {
                parser.diags.push(CodeError::without_notes_and_hint(
                    "const variable declaration without explicit type isn't allowed",
                    span,
                    ErrorCode::SyntaxError,
                ));
            }
            if expr.is_none() {
                parser.diags.push(CodeError::without_notes_and_hint(
                    "const variable declaration without value isn't allowed",
                    span,
                    ErrorCode::SyntaxError,
                ));
            }

            if ty.is_none() || expr.is_none() {
                return Err(CodeError::without_notes_and_hint(
                    "invalid const declaration",
                    span,
                    ErrorCode::SyntaxError,
                ));
            }

            Ok(Stmt {
                kind: Box::new(StmtKind::Const(ConstStmt {
                    name,
                    ty: ty.unwrap(),
                    expr: expr.unwrap(),
                    pub_: Pub::from(is_pub),
                })),
                span: start_span.merge(parser.previous().span),
            })
        }
    }
}

pub fn parse_use(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_span = parser.current_span();
    parser.advance_without_eof()?;

    fn parse_use_tree(parser: &mut Parser) -> Result<UseStmtTreeNode, CodeError> {
        let mut root_node = {
            let (name, span) = parser.expect_ident()?;

            UseStmtTreeNode {
                name,
                children: vec![],
                span,
            }
        };

        if parser.peek().kind == PathSep {
            parser.advance_without_eof()?;
            if matches!(parser.peek().kind, TokenKind::Ident(_)) {
                root_node.children.push(parse_use_tree(parser)?);
            } else {
                parser.expect_recoverable(TokenKind::LBrace, "left brace");

                loop {
                    root_node.children.push(parse_use_tree(parser)?);

                    if parser.peek().kind == TokenKind::Comma {
                        parser.advance_without_eof()?;
                    } else {
                        break;
                    }
                }

                parser.expect_recoverable(TokenKind::RBrace, "right brace");
            }
        }

        Ok(root_node)
    }

    let tree = parse_use_tree(parser)?;

    parser.expect_recoverable(TokenKind::Semi, "semicolon");

    Ok(Stmt {
        kind: Box::new(StmtKind::Use(UseStmt {
            tree,
            pub_: Pub::from(is_pub),
        })),
        span: start_span.merge(parser.previous().span),
    })
}
