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

use crate::ty::parse_ty_path;
use crate::Parser;
use rig_ast::expr::{AssignExpr, BinExpr, BinOp, Expr, ExprKind, FnCallArg, FnCallArgKind, FnCallExpr, IndexExpr, LogicalExpr, LogicalOp, MemberAccessExpr, MemberAccessProp, NumberExpr, PathExpr, UnaryExpr, UnaryOp};
use rig_ast::token::{LexicalToken, TokenKind};
use rig_errors::CodeError;

pub fn parse_expr(parser: &mut Parser) -> Result<Expr, CodeError> {
    let start_span = parser.peek().span;
    let mut expr = parse_logical_or(parser)?;

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
        && matches!(
            parser.try_peek_next(1),
            Some(&LexicalToken {
                kind: TokenKind::Assign,
                ..
            })
        )
        || matches!(parser.peek().kind, TokenKind::Less | TokenKind::LessEq)
    {
        parser.advance_without_eof()?;
        let op = if parser.peek().kind == TokenKind::Assign {
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
                op: BinOp::from(parser.peek().kind),
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
                op: BinOp::from(parser.peek().kind),
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
                op: BinOp::from(parser.peek().kind),
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
                op: BinOp::from(parser.peek().kind),
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
                    prop: MemberAccessProp {
                        ident,
                        span,
                    }
                }),
                span: start_sp.merge(parser.previous().span)
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

            Ok(Expr {
                kind: ExprKind::Path(PathExpr { path: ty_path }),
                span: start_span.merge(end_span),
            })
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
        _ => Err(CodeError::unexpected_token(parser.current_span())),
    }
}
