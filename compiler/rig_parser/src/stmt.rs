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

use crate::expr::{parse_body, parse_expr};
use crate::ty::{parse_generic_params, parse_ty_path};
use crate::Parser;
use rig_ast::path::PathSegment;
use rig_ast::stmt::{ConstStmt, EnumStmt, EnumVariant, EnumVariantOrStructProperty, EnumVariantStructLike, EnumVariantWithNoValue, EnumVariantWithValue, FnArg, FnArgKind, FnPrototype, FnRet, FnStmt, ForStmt, ImplStmt, LetStmt, ModStmt, Mutable, Pub, Stmt, StmtKind, StructStmt, TraitStmt, TyAliasStmt, UseStmt, UseStmtTreeNode, WhereClause, WhileStmt};
use rig_ast::token::TokenKind;
use rig_ast::token::TokenKind::PathSep;
use rig_errors::{CodeError, ErrorCode};
use rig_intern::intern;

pub fn parse_program(parser: &mut Parser) -> Result<Stmt, CodeError> {
    if parser.peek().kind == TokenKind::Impl {
        parse_impl(parser)
    } else {
        parse_decl(parser, parser.peek().kind == TokenKind::Pub)
    }
}

pub fn parse_impl(parser: &mut Parser) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let generic_params = if parser.peek().kind == TokenKind::Less {
        Some(parse_generic_params(parser, true)?)
    } else {
        None
    };

    let mut impl_for_span_start = parser.current_span();
    let mut impl_for = parse_ty_path(parser, false)?;
    let mut impl_for_span_end = parser.previous().span;
    if matches!(impl_for.segments[0], PathSegment::Generic(_)) {
        parser.diags.push(CodeError::unexpected_token_with_note(
            impl_for_span_start.merge(impl_for_span_end),
            "invalid type path for this position",
        ));
    }

    let trait_bound = if parser.peek().kind == TokenKind::For {
        parser.advance_without_eof()?;
        let trait_bound = impl_for;

        impl_for_span_start = parser.current_span();
        impl_for = parse_ty_path(parser, false)?;
        impl_for_span_end = parser.previous().span;
        if matches!(impl_for.segments[0], PathSegment::Generic(_)) {
            parser.diags.push(CodeError::unexpected_token_with_note(
                impl_for_span_start.merge(impl_for_span_end),
                "invalid type path for this position",
            ));
        }

        Some(trait_bound)
    } else {
        None
    };

    parser.expect_recoverable(TokenKind::LBrace, "{");

    let mut items = vec![];
    while !parser.is_eof() && parser.peek().kind != TokenKind::RBrace {
        let is_pub = parser.peek().kind == TokenKind::Pub;
        if is_pub {
            parser.advance_without_eof()?;
        }

        match match parser.peek().kind {
            TokenKind::Const => parse_var_decl(parser, is_pub, VarDeclType::Const),
            TokenKind::Fn => parse_fn_decl(parser, is_pub, true, true),
            TokenKind::Type => parse_type_alias(parser, is_pub),
            _ => Err(CodeError::unexpected_token(parser.current_span()))
        } {
            Ok(stmt) => items.push(stmt),
            Err(e) => {
                parser.diags.push(e);
                parser.synchronize(&[]);
            }
        };
    }

    parser.expect_recoverable(TokenKind::RBrace, "}");

    Ok(Stmt {
        kind: Box::new(StmtKind::Impl(ImplStmt {
            generic_params,
            trait_bound,
            impl_for,
            items,
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

pub fn parse_decl(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    if is_pub {
        parser.advance_without_eof()?;
    }

    match parser.peek().kind {
        TokenKind::Struct => parse_struct_decl(parser, is_pub),
        TokenKind::Enum => parse_enum_decl(parser, is_pub),
        TokenKind::Const => parse_var_decl(parser, is_pub, VarDeclType::Const),
        TokenKind::Let => parse_var_decl(parser, is_pub, VarDeclType::Let),
        TokenKind::Fn => parse_fn_decl(parser, is_pub, false, true),
        TokenKind::Mod => parse_mod_decl(parser, is_pub),
        TokenKind::Trait => parse_trait(parser, is_pub),
        TokenKind::Use => parse_use(parser, is_pub),
        TokenKind::Type => parse_type_alias(parser, is_pub),
        _ => Err(CodeError::unexpected_token(parser.current_span())),
    }
}

pub fn parse_fn_prototype(parser: &mut Parser, inside_impl: bool) -> Result<FnPrototype, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let (name, _) = parser.expect_ident()?;
    let generic_params = if parser.peek().kind == TokenKind::Less {
        Some(parse_generic_params(parser, true)?)
    } else {
        None
    };

    parser.expect_recoverable(TokenKind::LParen, "left parenthesis");

    let takes_self = if parser.peek().raw == intern!("self") {
        parser.advance_without_eof()?;
        if parser.peek().kind == TokenKind::Comma {
            parser.advance_without_eof()?;
        }

        if inside_impl {
            true
        } else {
            parser.diags.push(CodeError::unexpected_token_with_note(
                parser.previous().span,
                "`self` is only allowed in struct methods",
            ));
            false
        }
    } else {
        false
    };

    let mut args = vec![];
    while !parser.is_eof() && parser.peek().kind != TokenKind::RParen {
        let span = parser.current_span();
        let kind = match parser.peek().kind {
            TokenKind::Anon => FnArgKind::Anon,
            TokenKind::Vararg => FnArgKind::Vararg,
            _ => FnArgKind::NotAnon,
        };

        if kind != FnArgKind::NotAnon {
            parser.advance_without_eof()?;
        }

        let (name, _) = parser.expect_ident()?;

        parser.expect_recoverable(TokenKind::Colon, "colon");

        let is_mut = if parser.peek().kind == TokenKind::Mut {
            parser.advance_without_eof()?;
            true
        } else {
            false
        };

        let is_move = if parser.peek().kind == TokenKind::Mul {
            parser.advance_without_eof()?;
            true
        } else {
            false
        };

        let ty = parse_ty_path(parser, false)?;

        args.push(FnArg {
            name,
            ty,
            mutable: Mutable::from(is_mut),
            moves: is_move,
            kind,
            span: span.merge(parser.previous().span),
        });

        if parser.peek().kind != TokenKind::RParen {
            parser.expect_recoverable(TokenKind::Comma, "comma");
        }
    }

    parser.expect_recoverable(TokenKind::RParen, "right parenthesis");

    let (ret_ty, ret_ty_span) = if parser.peek().kind == TokenKind::RightArrow {
        parser.advance_without_eof()?;
        let start_sp = parser.current_span();
        (
            Some(parse_ty_path(parser, false)?),
            Some(start_sp.merge(parser.previous().span)),
        )
    } else {
        (None, None)
    };

    let mut where_clauses = vec![];
    if parser.peek().kind == TokenKind::Where {
        parser.advance_without_eof()?;
        loop {
            let (name, span) = parser.expect_ident()?;
            parser.expect_recoverable(TokenKind::Colon, "colon");

            let ty = parse_ty_path(parser, false)?;
            where_clauses.push(WhereClause {
                name,
                ty,
                span: span.merge(parser.previous().span),
            });

            if parser.peek().kind == TokenKind::Comma {
                parser.advance_without_eof()?;
            } else {
                break;
            }
        }
    }

    let prototype_span = start_sp.merge(parser.previous().span);

    Ok(FnPrototype {
        name,
        generic_params,
        takes_self,
        args,
        where_clauses,
        ret_ty: if let (Some(ret_ty), Some(ret_ty_span)) = (ret_ty, ret_ty_span) {
            Some(FnRet {
                ty: ret_ty,
                span: ret_ty_span,
            })
        } else {
            None
        },
        span: prototype_span,
    })
}

pub fn parse_fn_decl(
    parser: &mut Parser,
    is_pub: bool,
    inside_impl: bool,
    require_body: bool,
) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();

    let prototype = parse_fn_prototype(parser, inside_impl)?;
    let body = if parser.peek().kind == TokenKind::LBrace {
        Some(parse_body(parser)?)
    } else {
        if require_body {
            parser.diags.push(CodeError {
                error_code: ErrorCode::MissingFunctionBody,
                message: intern!("function in this position must have a body"),
                hints: vec![],
                notes: vec![],
                pos: prototype.span
            });
        }
        parser.expect_recoverable(TokenKind::Semi, "semicolon");

        None
    };

    Ok(Stmt {
        kind: Box::new(StmtKind::Fn(FnStmt {
            pub_: Pub::from(is_pub),
            prototype,
            body,
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

pub fn parse_type_alias(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let (alias, _) = parser.expect_ident()?;
    parser.expect_recoverable(TokenKind::Assign, "equal");
    let ty = parse_ty_path(parser, false)?;

    parser.expect_recoverable(TokenKind::Semi, "semicolon");

    Ok(Stmt {
        kind: Box::new(StmtKind::TyAlias(TyAliasStmt {
            pub_: Pub::from(is_pub),
            alias_name: alias,
            ty,
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

pub fn parse_mod_decl(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let (name, _) = parser.expect_ident()?;
    parser.expect_recoverable(TokenKind::LBrace, "left brace");
    let mut body = vec![];
    while !parser.is_eof() && parser.peek().kind != TokenKind::RBrace {
        match parse_program(parser) {
            Ok(stmt) => body.push(stmt),
            Err(e) => {
                parser.synchronize(&[]);
                parser.diags.push(e)
            }
        }
    }
    parser.expect_recoverable(TokenKind::RBrace, "right brace");
    parser.expect_recoverable(TokenKind::Semi, "semicolon");

    Ok(Stmt {
        kind: Box::new(StmtKind::Mod(ModStmt {
            pub_: Pub::from(is_pub),
            body,
            name,
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

fn parse_struct_or_enum_fields(
    parser: &mut Parser,
) -> Result<Vec<EnumVariantOrStructProperty>, CodeError> {
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

    Ok(properties)
}

pub fn parse_struct_decl(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let (name, _) = parser.expect_ident()?;
    let generic_params = if parser.peek().kind == TokenKind::Less {
        Some(parse_generic_params(parser, true)?)
    } else {
        None
    };

    Ok(Stmt {
        kind: Box::new(StmtKind::Struct(StructStmt {
            name,
            properties: parse_struct_or_enum_fields(parser)?,
            generic_params,
            pub_: Pub::from(is_pub),
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

pub fn parse_enum_decl(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let (name, _) = parser.expect_ident()?;
    let generic_params = if parser.peek().kind == TokenKind::Less {
        Some(parse_generic_params(parser, true)?)
    } else {
        None
    };

    parser.expect_recoverable(TokenKind::LBrace, "left brace");
    let mut variants = vec![];
    while !parser.is_eof() && parser.peek().kind != TokenKind::RBrace {
        let (name, span) = parser.expect_ident()?;
        if parser.peek().kind == TokenKind::Comma {
            parser.advance_without_eof()?;
            variants.push(EnumVariant::NoValue(EnumVariantWithNoValue { name, span }));
            continue;
        }

        if parser.peek().kind == TokenKind::LBrace {
            variants.push(EnumVariant::StructLike(EnumVariantStructLike {
                name,
                properties: parse_struct_or_enum_fields(parser)?,
                span: span.merge(parser.previous().span),
            }));
        } else if parser.peek().kind == TokenKind::LParen {
            parser.advance_without_eof()?;
            let ty = parse_ty_path(parser, false)?;
            variants.push(EnumVariant::WithValue(EnumVariantWithValue {
                name,
                span,
                ty,
            }));
            parser.expect_recoverable(TokenKind::RParen, "right brace");
        }

        if parser.peek().kind != TokenKind::RBrace {
            parser.expect_recoverable(TokenKind::Comma, "comma");
        }
    }
    parser.expect_recoverable(TokenKind::RBrace, "right brace");

    Ok(Stmt {
        kind: Box::new(StmtKind::Enum(EnumStmt {
            pub_: Pub::from(is_pub),
            name,
            generic_params,
            variants,
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

#[derive(PartialEq)]
pub enum VarDeclType {
    Let,
    Const,
}

pub fn parse_var_decl(
    parser: &mut Parser,
    is_pub: bool,
    decl_type: VarDeclType,
) -> Result<Stmt, CodeError> {
    let start_sp = if is_pub {
        parser.previous().span
    } else {
        parser.current_span()
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
            span: start_sp.merge(parser.previous().span),
        }),
        VarDeclType::Const => {
            let span = start_sp.merge(parser.previous().span);

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
                span: start_sp.merge(parser.previous().span),
            })
        }
    }
}

pub fn parse_use(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
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
        span: start_sp.merge(parser.previous().span),
    })
}

pub fn parse_for(parser: &mut Parser) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let (ident, _) = parser.expect_ident()?;

    parser.expect_recoverable(TokenKind::In, "in");

    let iterable = parse_expr(parser)?;
    let body = parse_body(parser)?;

    Ok(Stmt {
        kind: Box::new(StmtKind::For(ForStmt {
            ident,
            iterable,
            body,
        })),
        span: start_sp.merge(parser.previous().span),
    })
}

pub fn parse_loop(parser: &mut Parser) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    Ok(Stmt {
        kind: Box::new(StmtKind::Expr(parse_body(parser)?)),
        span: start_sp.merge(parser.previous().span),
    })
}

pub fn parse_while(parser: &mut Parser) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let cond = parse_expr(parser)?;
    let body = parse_body(parser)?;

    Ok(Stmt {
        kind: Box::new(StmtKind::While(WhileStmt {
            cond,
            body
        })),
        span: start_sp.merge(parser.previous().span)
    })
}

pub fn parse_trait(parser: &mut Parser, is_pub: bool) -> Result<Stmt, CodeError> {
    let start_sp = parser.current_span();
    parser.advance_without_eof()?;

    let generic_params = if parser.peek().kind == TokenKind::Less {
        Some(parse_generic_params(parser, true)?)
    } else {
        None
    };

    let (name, _) = parser.expect_ident()?;
    let inherits_from = if parser.peek().kind == TokenKind::Less {
        parser.advance_without_eof()?;

        Some(parse_ty_path(parser, false)?)
    } else {
        None
    };

    parser.expect_recoverable(TokenKind::LBrace, "left brace");

    let mut items = vec![];
    while !parser.is_eof() && parser.peek().kind != TokenKind::RBrace {
        match match parser.peek().kind {
            TokenKind::Type => {
                let res = parse_type_alias(parser, true);
                if let Ok(_) = res {
                    parser.expect_recoverable(TokenKind::Semi, "semicolon");
                }

                res
            },
            TokenKind::Fn => parse_fn_decl(parser, true, true, false),
            _ => Err(CodeError::unexpected_token(parser.current_span()))
        } {
            Ok(stmt) => items.push(stmt),
            Err(e) => {
                parser.diags.push(e);
                parser.synchronize(&[TokenKind::Impl]);
            }
        }
    }

    parser.expect_recoverable(TokenKind::RBrace, "right brace");

    Ok(Stmt {
        kind: Box::new(StmtKind::Trait(TraitStmt {
            pub_: Pub::from(is_pub),
            generic_params,
            name,
            inherits_from,
            items,
        })),
        span: start_sp.merge(parser.previous().span)
    })
}
