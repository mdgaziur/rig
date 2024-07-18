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

use crate::stmt::VarDeclType;
use crate::ty::parse_ty_path;
use crate::{stmt, Parser};
use rig_ast::expr::{AssignExpr, BinExpr, BinOp, BodyExpr, ConditionalExpr, Expr, ExprKind, FnCallArg, FnCallArgKind, FnCallExpr, IndexExpr, LogicalExpr, LogicalOp, MemberAccessExpr, MemberAccessProp, NumberExpr, PathExpr, RangeExpr, StructExpr, StructExprProperty, TypeCastExpr, UnaryExpr, UnaryOp};
use rig_ast::stmt::{Stmt, StmtKind};
use rig_ast::token::{LexicalToken, TokenKind};
use rig_errors::CodeError;

pub fn parse_expr(parser: &mut Parser) -> Result<Expr, CodeError> {
    parse_typecast(parser)
}

fn parse_typecast(parser: &mut Parser) -> Result<Expr, CodeError> {
    let expr = parse_assignment(parser)?;

    Ok(if parser.peek().kind == TokenKind::As {
        parser.advance_without_eof()?;
        let typ = parse_ty_path(parser, true)?;

        if parser.peek().kind == TokenKind::LParen {
            parser.diags.push(CodeError::unexpected_token_with_note(
                parser.current_span(),
                "this looks like a possible function call. You may want to wrap \
                    the expression in parentheses before calling it",
            ));
        }

        Expr {
            span: expr.span.merge(parser.previous().span),
            kind: ExprKind::TypeCast(TypeCastExpr {
                expr: Box::new(expr),
                typ,
            }),
        }
    } else {
        expr
    })
}

fn parse_assignment(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.peek().span;
    let mut expr = parse_range(parser)?;

    match parser.peek().kind {
        TokenKind::Assign => {
            expr = Expr {
                kind: ExprKind::Assign(AssignExpr {
                    assignee: Box::new(expr),
                    value: Box::new(parse_expr(parser)?),
                }),
                span: start_span.merge(parser.previous().span),
            }
        }
        TokenKind::Greater => {
            if matches!(
                parser.try_peek_next(1),
                Some(&LexicalToken {
                    kind: TokenKind::Greater,
                    ..
                })
            ) && matches!(
                parser.try_peek_next(2),
                Some(&LexicalToken {
                    kind: TokenKind::Assign,
                    ..
                })
            ) {
                parser.advance_without_eof()?;
                parser.advance_without_eof()?;
                parser.advance_without_eof()?;

                let rhs = parse_expr(parser)?;
                expr = Expr {
                    span: start_span.merge(rhs.span),
                    kind: ExprKind::Assign(AssignExpr {
                        assignee: Box::new(expr.clone()),
                        value: Box::new(Expr {
                            span: rhs.span,
                            kind: ExprKind::Bin(BinExpr {
                                lhs: Box::new(expr),
                                op: BinOp::RShift,
                                rhs: Box::new(rhs),
                            }),
                        }),
                    }),
                }
            }
        }
        TokenKind::PlusEq
        | TokenKind::MinusEq
        | TokenKind::MulEq
        | TokenKind::DivEq
        | TokenKind::LShiftEq
        | TokenKind::AndEq
        | TokenKind::OrEq
        | TokenKind::XorEq
        | TokenKind::PowerEq => {
            let op = BinOp::from(parser.peek().kind);
            parser.advance_without_eof()?;
            let rhs = parse_expr(parser)?;
            expr = Expr {
                span: start_span.merge(rhs.span),
                kind: ExprKind::Assign(AssignExpr {
                    assignee: Box::new(expr.clone()),
                    value: Box::new(Expr {
                        span: rhs.span,
                        kind: ExprKind::Bin(BinExpr {
                            lhs: Box::new(expr),
                            op,
                            rhs: Box::new(rhs),
                        }),
                    }),
                }),
            }
        }
        _ => (),
    }

    Ok(expr)
}

fn parse_range(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_sp = parser.current_span();
    let mut expr = parse_logical_or(parser)?;

    if parser.peek().kind == TokenKind::RightArrow {
        parser.advance_without_eof()?;

        expr = Expr {
            kind: ExprKind::Range(Box::new(RangeExpr {
                from: expr,
                to: parse_logical_or(parser)?,
            })),
            span: start_sp.merge(parser.previous().span)
        }
    }

    Ok(expr)
}

fn parse_logical_or(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_logical_and(parser)?;

    if parser.peek().kind == TokenKind::LogicalAnd {
        parser.advance_without_eof()?;

        expr = Expr {
            kind: ExprKind::Logical(LogicalExpr {
                lhs: Box::new(expr),
                op: LogicalOp::And,
                rhs: Box::new(parse_logical_or(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_logical_and(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_equality(parser)?;

    if parser.peek().kind == TokenKind::LogicalAnd {
        parser.advance_without_eof()?;

        expr = Expr {
            kind: ExprKind::Logical(LogicalExpr {
                lhs: Box::new(expr),
                op: LogicalOp::And,
                rhs: Box::new(parse_logical_and(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_equality(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_comparison(parser)?;

    if matches!(parser.peek().kind, TokenKind::Eq | TokenKind::NotEq) {
        parser.advance_without_eof()?;

        expr = Expr {
            kind: ExprKind::Logical(LogicalExpr {
                lhs: Box::new(expr),
                op: LogicalOp::from(parser.previous().kind),
                rhs: Box::new(parse_equality(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_comparison(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_bitwise_or(parser)?;

    if parser.peek().kind == TokenKind::Greater
        && !matches!(
            parser.try_peek_next(1),
            Some(&LexicalToken {
                kind: TokenKind::Greater, // In case a "left shift assign" was found
                ..
            })
        )
        || matches!(parser.peek().kind, TokenKind::Less | TokenKind::LessEq)
    {
        parser.advance_without_eof()?;
        let op = if parser.peek().kind == TokenKind::Assign
            && parser.previous().kind == TokenKind::Greater
        {
            parser.advance_without_eof()?;
            LogicalOp::GreaterEq
        } else {
            LogicalOp::from(parser.previous().kind)
        };

        expr = Expr {
            kind: ExprKind::Logical(LogicalExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(parse_comparison(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_bitwise_or(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_bitwise_xor(parser)?;

    if parser.peek().kind == TokenKind::Or {
        parser.advance_without_eof()?;
        expr = Expr {
            kind: ExprKind::Bin(BinExpr {
                lhs: Box::new(expr),
                op: BinOp::from(parser.previous().kind),
                rhs: Box::new(parse_bitwise_or(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_bitwise_xor(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_bitwise_and(parser)?;

    if parser.peek().kind == TokenKind::Xor {
        parser.advance_without_eof()?;
        expr = Expr {
            kind: ExprKind::Bin(BinExpr {
                lhs: Box::new(expr),
                op: BinOp::from(parser.previous().kind),
                rhs: Box::new(parse_bitwise_xor(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_bitwise_and(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_bitwise_shift(parser)?;

    if parser.peek().kind == TokenKind::And {
        parser.advance_without_eof()?;
        expr = Expr {
            kind: ExprKind::Bin(BinExpr {
                lhs: Box::new(expr),
                op: BinOp::from(parser.previous().kind),
                rhs: Box::new(parse_bitwise_and(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_bitwise_shift(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_term(parser)?;

    if parser.peek().kind == TokenKind::Greater
        && matches!(
            parser.try_peek_next(1),
            Some(&LexicalToken {
                kind: TokenKind::Greater,
                ..
            })
        )
        && !matches!(
            parser.try_peek_next(2),
            Some(&LexicalToken {
                kind: TokenKind::Assign,
                ..
            })
        )
        || parser.peek().kind == TokenKind::LShift
    {
        parser.advance_without_eof()?;
        let op = if parser.peek().kind == TokenKind::Greater {
            parser.advance_without_eof()?;
            BinOp::RShift
        } else {
            BinOp::LShift
        };

        expr = Expr {
            kind: ExprKind::Bin(BinExpr {
                lhs: Box::new(expr),
                op,
                rhs: Box::new(parse_bitwise_shift(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_term(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_factor(parser)?;

    if parser.peek().kind == TokenKind::Plus || parser.peek().kind == TokenKind::Minus {
        parser.advance_without_eof()?;
        expr = Expr {
            kind: ExprKind::Bin(BinExpr {
                lhs: Box::new(expr),
                op: BinOp::from(parser.previous().kind),
                rhs: Box::new(parse_term(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_factor(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_power(parser)?;

    if parser.peek().kind == TokenKind::Mul || parser.peek().kind == TokenKind::Div {
        parser.advance_without_eof()?;
        expr = Expr {
            kind: ExprKind::Bin(BinExpr {
                lhs: Box::new(expr),
                op: BinOp::from(parser.previous().kind),
                rhs: Box::new(parse_factor(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_power(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    let mut expr = parse_unary(parser)?;

    if parser.peek().kind == TokenKind::Power {
        parser.advance_without_eof()?;
        expr = Expr {
            kind: ExprKind::Bin(BinExpr {
                lhs: Box::new(expr),
                op: BinOp::Power,
                rhs: Box::new(parse_power(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        };
    }

    Ok(expr)
}

fn parse_unary(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();

    if let Some(un_op) = match parser.peek().kind {
        TokenKind::Not => Some(UnaryOp::Not),
        TokenKind::Minus => Some(UnaryOp::Neg),
        TokenKind::BinaryNot => Some(UnaryOp::BitwiseNot),
        _ => None,
    } {
        parser.advance_without_eof()?;
        Ok(Expr {
            kind: ExprKind::Unary(UnaryExpr {
                op: un_op,
                value: Box::new(parse_unary(parser)?),
            }),
            span: start_span.merge(parser.previous().span),
        })
    } else {
        parse_index(parser)
    }
}

fn parse_index(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_sp = parser.current_span();
    let mut indexable = parse_fn_call(parser)?;

    if parser.peek().kind == TokenKind::LBracket {
        parser.advance_without_eof()?;
        let index = Box::new(parse_expr(parser)?);
        parser.expect_recoverable(TokenKind::RBracket, "right bracket");

        indexable = Expr {
            kind: ExprKind::Index(IndexExpr {
                indexable: Box::new(indexable),
                index,
            }),
            span: start_sp.merge(parser.previous().span),
        };
    }
    Ok(indexable)
}

fn parse_fn_call(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_sp = parser.current_span();
    let mut callable = parse_primary(parser)?;

    fn args(parser: &mut Parser) -> Result<Vec<FnCallArg>, CodeError> {
        let mut args = vec![];

        loop {
            if let TokenKind::Ident(arg_name) = parser.peek().kind
                && matches!(
                    parser.try_peek_next(1),
                    Some(&LexicalToken {
                        kind: TokenKind::Colon,
                        ..
                    })
                )
            {
                parser.advance_without_eof()?;
                parser.advance_without_eof()?;

                args.push(FnCallArg {
                    ty: FnCallArgKind::NotAnon(arg_name, parser.current_span()),
                    expr: Box::new(parse_expr(parser)?),
                });
            } else if parser.peek().kind != TokenKind::RParen {
                args.push(FnCallArg {
                    ty: FnCallArgKind::Anon,
                    expr: Box::new(parse_expr(parser)?),
                });
            }

            if parser.peek().kind == TokenKind::Comma {
                parser.advance_without_eof()?;
            } else {
                break;
            }
        }

        Ok(args)
    }

    loop {
        if parser.peek().kind == TokenKind::LParen {
            parser.advance();
            let args = args(parser)?;
            parser.expect_recoverable(TokenKind::RParen, "right parenthesis");

            callable = Expr {
                kind: ExprKind::FnCall(FnCallExpr {
                    callable: Box::new(callable),
                    args,
                }),
                span: start_sp.merge(parser.previous().span),
            }
        } else if parser.peek().kind == TokenKind::Dot {
            parser.advance_without_eof()?;
            let (ident, span) = parser.expect_ident()?;
            callable = Expr {
                kind: ExprKind::MemberAccess(MemberAccessExpr {
                    expr: Box::new(callable),
                    prop: MemberAccessProp { ident, span },
                }),
                span: start_sp.merge(parser.previous().span),
            }
        } else {
            break;
        }
    }

    Ok(callable)
}

fn parse_primary(parser: &mut Parser) -> Result<Expr, CodeError> {
    match parser.peek().kind {
        TokenKind::LParen => {
            parser.advance_without_eof()?;
            let expr = parse_expr(parser)?;
            parser.expect_recoverable(TokenKind::RParen, "right parenthesis");
            Ok(expr)
        }
        TokenKind::Ident(_) => {
            let start_span = parser.current_span();
            let ty_path = parse_ty_path(parser, true)?;
            let end_span = parser.previous().span;

            // NOTE: in case labels are added in the future, this will need some changes to properly
            //       detect Struct expressions
            // I have no clue how to fix this with `if_chain`. I am so sorry for the mess you're about to see :(
            if parser.peek().kind == TokenKind::LBrace
                && let Some(lookahead1) = parser.try_peek_next(1)
            {
                match lookahead1.kind {
                    TokenKind::Ident(_) => {
                        if let Some(lookahead2) = parser.try_peek_next(2)
                            && lookahead2.kind == TokenKind::Colon
                        {
                            parser.advance(); // consume LBrace

                            let mut values = vec![];
                            while !parser.is_eof() && parser.peek().kind != TokenKind::RBrace {
                                let (name, span) = parser.expect_ident()?;
                                parser.expect_recoverable(TokenKind::Colon, "colon");

                                let value = parse_expr(parser)?;
                                values.push(StructExprProperty {
                                    name,
                                    value,
                                    span: span.merge(parser.previous().span),
                                });

                                if parser.peek().kind != TokenKind::RBrace {
                                    parser.expect_recoverable(TokenKind::Comma, "comma");
                                }
                            }
                            parser.expect_recoverable(TokenKind::RBrace, "right brace");

                            Ok(Expr {
                                kind: ExprKind::Struct(StructExpr {
                                    path: ty_path,
                                    values,
                                }),
                                span: start_span.merge(parser.previous().span),
                            })
                        } else {
                            Ok(Expr {
                                kind: ExprKind::Path(PathExpr { path: ty_path }),
                                span: start_span.merge(end_span),
                            })
                        }
                    }
                    // We won't allow instantiating structs with no fields anyway. However, we want
                    // this to be parsed during parsing and reported later during the typechecking
                    // phase. To prevent syntax error in cases like `if x {}`, we check if we're
                    // parsing the condition of an if expr.
                    TokenKind::RBrace if parser.parsing_condition.is_empty() => {
                        parser.advance(); // consume LBrace
                        parser.advance(); // consume RBrace

                        Ok(Expr {
                            kind: ExprKind::Struct(StructExpr {
                                path: ty_path,
                                values: vec![],
                            }),
                            span: start_span.merge(parser.previous().span),
                        })
                    }
                    _ => Ok(Expr {
                        kind: ExprKind::Path(PathExpr { path: ty_path }),
                        span: start_span.merge(end_span),
                    }),
                }
            } else {
                Ok(Expr {
                    kind: ExprKind::Path(PathExpr { path: ty_path }),
                    span: start_span.merge(end_span),
                })
            }
        }
        TokenKind::LBracket => {
            let start_span = parser.current_span();

            let mut values = vec![];
            loop {
                values.push(parse_expr(parser)?);

                if parser.peek().kind == TokenKind::Comma {
                    parser.advance_without_eof()?;
                } else {
                    break;
                }
            }

            let (token, _) = parser.expect_recoverable(TokenKind::RBracket, "right bracket");
            let end_span = token.span;

            Ok(Expr {
                kind: ExprKind::Array(values),
                span: start_span.merge(end_span),
            })
        }
        TokenKind::Number { number, kind } => {
            parser.advance();
            Ok(Expr {
                kind: ExprKind::Number(NumberExpr {
                    value: number,
                    kind,
                }),
                span: parser.previous().span,
            })
        }
        TokenKind::True => {
            parser.advance();
            Ok(Expr {
                kind: ExprKind::Boolean(true),
                span: parser.previous().span,
            })
        }
        TokenKind::False => {
            parser.advance();
            Ok(Expr {
                kind: ExprKind::Boolean(false),
                span: parser.previous().span,
            })
        }
        TokenKind::String(string) => {
            parser.advance();
            Ok(Expr {
                kind: ExprKind::String(string),
                span: parser.previous().span,
            })
        }
        TokenKind::LBrace => parse_body(parser),
        TokenKind::If => parse_conditional(parser),
        _ => Err(CodeError::unexpected_token(parser.current_span())),
    }
}

pub fn parse_body(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.peek().span;
    parser.expect_recoverable(TokenKind::LBrace, "left brace");

    let mut stmts = vec![];
    let mut return_expr = None;

    while !parser.is_eof() && parser.peek().kind != TokenKind::RBrace {
        if parser.peek().kind == TokenKind::Semi {
            parser.advance();
            continue;
        }

        match match parser.peek().kind {
            TokenKind::Struct => stmt::parse_struct_decl(parser, false),
            TokenKind::Enum => stmt::parse_enum_decl(parser, false),
            TokenKind::Const => stmt::parse_var_decl(parser, false, VarDeclType::Const),
            TokenKind::Let => stmt::parse_var_decl(parser, false, VarDeclType::Let),
            TokenKind::Impl => stmt::parse_impl(parser),
            TokenKind::Mod => stmt::parse_mod_decl(parser, false),
            TokenKind::Use => stmt::parse_use(parser, false),
            TokenKind::For => stmt::parse_for(parser),
            TokenKind::Loop => stmt::parse_loop(parser),
            TokenKind::While => stmt::parse_while(parser),
            TokenKind::Fn => stmt::parse_fn_decl(parser, false, false),
            TokenKind::Trait => stmt::parse_trait(parser, false),
            TokenKind::Type => stmt::parse_type_alias(parser, false),
            _ => match parse_expr(parser) {
                Ok(expr) => {
                    if parser.peek().kind != TokenKind::RBrace
                        && !matches!(expr.kind, ExprKind::Conditional(..)) {
                        parser.expect_recoverable(TokenKind::Semi, "semicolon");
                    } else {
                        return_expr = Some(expr);
                        continue;
                    }

                    Ok(Stmt {
                        span: expr.span,
                        kind: Box::new(StmtKind::Expr(expr)),
                    })
                }
                Err(e) => Err(e),
            },
        } {
            Ok(stmt) => stmts.push(stmt),
            Err(e) => {
                parser.diags.push(e);
                parser.synchronize(&[]);
            }
        }
    }

    parser.expect_recoverable(TokenKind::RBrace, "right brace");

    Ok(Expr {
        kind: ExprKind::Body(Box::new(BodyExpr {
            stmts,
            expr: return_expr,
        })),
        span: start_span.merge(parser.previous().span),
    })
}

fn parse_conditional(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.current_span();
    parser.advance_without_eof()?;

    parser.parsing_condition.push(());
    let condition = parse_expr(parser)?;
    parser.parsing_condition.pop();

    let body = parse_body(parser)?;

    let else_ = if parser.peek().kind == TokenKind::Else {
        parser.advance();

        if parser.peek().kind == TokenKind::If {
            Some(parse_conditional(parser)?)
        } else {
            Some(parse_body(parser)?)
        }
    } else {
        None
    };

    Ok(Expr {
        kind: ExprKind::Conditional(Box::new(ConditionalExpr {
            condition,
            body,
            else_,
        })),
        span: start_span.merge(parser.previous().span),
    })
}
