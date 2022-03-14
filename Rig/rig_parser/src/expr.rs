use crate::Parser;
use rig_ast::expr::Expr;
use rig_ast::op::{BinaryOperator, LogicalOperator, UnaryOperator};
use rig_ast::struct_field::StructExprField;
use rig_ast::token::TokenType;
use rig_error::{ErrorCode, ErrorType, RigError};
use rig_span::Span;
use std::str::FromStr;

pub fn expr(parser: &mut Parser) -> Result<Expr, RigError> {
    assignment(parser)
}

pub fn assignment(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let expr = logical_or(parser)?;

    match parser.peek().token_type {
        TokenType::PlusEquals
        | TokenType::LeftShiftEquals
        | TokenType::RightShiftEquals
        | TokenType::MinusEquals
        | TokenType::MultiplyEquals
        | TokenType::DivideEquals
        | TokenType::AndOpEquals
        | TokenType::OrOpEquals
        | TokenType::XorEquals
        | TokenType::ModulusEquals => {
            let op = BinaryOperator::from_assignequal(&parser.peek().lexeme).unwrap();
            parser.advance();
            let eq_span = parser.peek().span.clone();
            let rhs = Box::new(crate::expr::expr(parser)?);

            return match &expr {
                Expr::GetExpr { object, name, .. } => Ok(Expr::SetExpr {
                    object: object.clone(),
                    name: name.clone(),
                    value: Box::new(Expr::BinaryExpr {
                        lhs: Box::new(expr.clone()),
                        op,
                        rhs,
                        span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                    }),
                    span: Span::merge(sp_start, parser.peek().span.clone()),
                }),
                Expr::VariableExpr { name, .. } => Ok(Expr::AssignmentExpr {
                    value: Box::new(Expr::BinaryExpr {
                        lhs: Box::new(expr.clone()),
                        op,
                        rhs,
                        span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                    }),
                    name: name.clone(),
                    span: Span::merge(sp_start, parser.previous().span.clone()),
                }),
                _ => Err(RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0006,
                    "Invalid assignment",
                    eq_span,
                )),
            };
        }
        _ => (),
    }

    if parser.check(TokenType::Equal) {
        parser.advance();
        let eq_span = parser.previous().span.clone();
        let rhs = crate::expr::expr(parser)?;

        return match expr {
            Expr::GetExpr { object, name, span } => Ok(Expr::SetExpr {
                object,
                name,
                value: Box::from(rhs),
                span: Span::merge(span, parser.previous().span.clone()),
            }),
            Expr::VariableExpr { name, span } => Ok(Expr::AssignmentExpr {
                name,
                value: Box::new(rhs),
                span: Span::merge(span, parser.previous().span.clone()),
            }),
            _ => Err(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0006,
                "Invalid assignment",
                eq_span,
            )),
        };
    }

    Ok(expr)
}

pub fn logical_or(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = logical_and(parser)?;

    let mut op;

    loop {
        op = LogicalOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == LogicalOperator::Or {
                parser.advance();
                let rhs = logical_and(parser)?;

                expr = Expr::LogicalExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn logical_and(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = equality(parser)?;

    let mut op;

    loop {
        op = LogicalOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == LogicalOperator::And {
                parser.advance();
                let rhs = equality(parser)?;

                expr = Expr::LogicalExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn equality(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = comparison(parser)?;

    let mut op;

    loop {
        op = LogicalOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == LogicalOperator::Equal || op == LogicalOperator::NotEqual {
                parser.advance();
                let rhs = comparison(parser)?;

                expr = Expr::LogicalExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn comparison(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = bitwise_or(parser)?;

    let mut op;

    loop {
        op = LogicalOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == LogicalOperator::Less
                || op == LogicalOperator::LessEq
                || op == LogicalOperator::Greater
                || op == LogicalOperator::GreaterEq
            {
                parser.advance();
                let rhs = bitwise_or(parser)?;
                expr = Expr::LogicalExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn bitwise_or(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = bitwise_xor(parser)?;

    let mut op;

    loop {
        op = BinaryOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == BinaryOperator::Or {
                parser.advance();
                let rhs = bitwise_xor(parser)?;

                expr = Expr::BinaryExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn bitwise_xor(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = bitwise_and(parser)?;

    let mut op;

    loop {
        op = BinaryOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == BinaryOperator::Xor {
                parser.advance();
                let rhs = bitwise_and(parser)?;

                expr = Expr::BinaryExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn bitwise_and(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = bitwise_shift(parser)?;

    let mut op;

    loop {
        op = BinaryOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == BinaryOperator::And {
                parser.advance();
                let rhs = bitwise_shift(parser)?;

                expr = Expr::BinaryExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn bitwise_shift(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = term(parser)?;

    let mut op;

    loop {
        op = BinaryOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == BinaryOperator::LeftShift || op == BinaryOperator::RightShift {
                parser.advance();
                let rhs = term(parser)?;

                expr = Expr::BinaryExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn term(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = factor(parser)?;

    let mut op;

    loop {
        op = BinaryOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == BinaryOperator::Plus || op == BinaryOperator::Minus {
                parser.advance();
                let rhs = factor(parser)?;

                expr = Expr::BinaryExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn factor(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = unary(parser)?;

    let mut op;

    loop {
        op = BinaryOperator::from_str(&parser.peek().lexeme);
        if let Ok(op) = op.clone() {
            if op == BinaryOperator::Multiply
                || op == BinaryOperator::Divide
                || op == BinaryOperator::Modulus
            {
                parser.advance();
                let rhs = unary(parser)?;

                expr = Expr::BinaryExpr {
                    lhs: Box::new(expr),
                    op: op.clone(),
                    rhs: Box::new(rhs),
                    span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
                };
            } else {
                break;
            }
        } else {
            break;
        }
    }
    Ok(expr)
}

pub fn unary(parser: &mut Parser) -> Result<Expr, RigError> {
    let op = match UnaryOperator::from_str(&parser.peek().lexeme) {
        Ok(o) => o,
        Err(_) => return call(parser),
    };
    let sp_start = parser.peek().span.clone();
    parser.advance();

    Ok(Expr::UnaryExpr {
        op,
        rhs: Box::new(unary(parser)?),
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

pub fn call(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let mut expr = primary(parser)?;

    loop {
        if parser.peek().token_type == TokenType::LeftParen {
            parser.advance();
            let args = arguments(parser)?;

            parser.consume(TokenType::RightParen, "Expected `)` after argument list")?;
            expr = Expr::CallExpr {
                name: Box::new(expr),
                args,
                span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
            }
        } else if parser.peek().token_type == TokenType::Dot {
            parser.advance();
            let name = parser.consume(TokenType::Identifier, "Expected identifier")?;
            expr = Expr::GetExpr {
                name: name.lexeme.clone(),
                object: Box::new(expr),
                span: Span::merge(sp_start.clone(), parser.previous().span.clone()),
            }
        } else {
            break;
        }
    }

    Ok(expr)
}

pub fn arguments(parser: &mut Parser) -> Result<Vec<Expr>, RigError> {
    let mut args = Vec::new();
    if parser.peek().token_type == TokenType::RightParen {
        return Ok(args);
    }
    args.push(expr(parser)?);

    loop {
        if parser.peek().token_type != TokenType::Comma {
            break;
        }

        parser.advance();
        args.push(expr(parser)?);
    }

    Ok(args)
}

pub fn primary(parser: &mut Parser) -> Result<Expr, RigError> {
    match parser.peek().token_type {
        TokenType::Identifier => struct_(parser),
        TokenType::StringLiteral => {
            let ret = Ok(Expr::StringLiteralExpr {
                value: parser.peek().literal.clone(),
                span: parser.peek().span.clone(),
            });

            parser.advance();
            ret
        }
        TokenType::NumberLiteral => {
            if parser.peek().lexeme.contains('.') {
                let ret = Ok(Expr::FloatLiteralExpr {
                    value: parser.peek().literal.parse().expect(
                        "Lexer emitted invalid float literal or value is too big to store in f64",
                    ),
                    span: parser.peek().span.clone(),
                });

                parser.advance();
                ret
            } else {
                let ret = Ok(Expr::IntegerLiteralExpr {
                    value: parser.peek().literal.parse().expect(
                        "Lexer emitted invalid integer literal or value is too big to store in i64",
                    ),
                    span: parser.peek().span.clone(),
                });

                parser.advance();
                ret
            }
        }
        TokenType::Keyword => {
            let ret = match parser.peek().lexeme.as_str() {
                "true" => Ok(Expr::BooleanLiteralExpr {
                    value: true,
                    span: parser.peek().span.clone(),
                }),
                "false" => Ok(Expr::BooleanLiteralExpr {
                    value: false,
                    span: parser.peek().span.clone(),
                }),
                "null" => Ok(Expr::NullLiteralExpr {
                    span: parser.peek().span.clone(),
                }),
                "self" => Ok(Expr::SelfExpr {
                    span: parser.peek().span.clone(),
                }),
                _ => Err(RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0005,
                    "Expected `true`, `false`, `null` or `self`",
                    parser.peek().span.clone(),
                )),
            };

            if ret.is_ok() {
                parser.advance();
            }

            ret
        }
        TokenType::LeftParen => {
            let _sp_start = parser.peek().span.clone();
            parser.advance();
            let expr = Box::new(expr(parser)?);
            parser.consume(TokenType::RightParen, "Expected `)` after expression")?;

            Ok(Expr::GroupingExpr {
                expr,
                span: parser.previous().span.clone(),
            })
        }
        _ => Err(RigError::with_no_hint_and_notes(
            ErrorType::Hard,
            ErrorCode::E0005,
            "Expected primary expression",
            parser.peek().span.clone(),
        )),
    }
}

pub fn struct_(parser: &mut Parser) -> Result<Expr, RigError> {
    let sp_start = parser.peek().span.clone();
    let expr = path(parser)?;

    match expr {
        Expr::PathExpr { .. } | Expr::VariableExpr { .. } => {
            if parser.peek().token_type == TokenType::LeftBrace {
                let left_brace_pos = parser.pos;
                parser.advance();

                let mut vals = vec![];
                // this is here so that stuff like following actually parses without error
                //
                // fn main() {
                //     if X {
                //         print Y;
                //     }
                // }
                //
                if let Ok(val) = field_with_val(parser) {
                    vals.push(val);
                } else {
                    parser.set_position(left_brace_pos);
                    return Ok(expr);
                }

                if parser.peek().token_type == TokenType::RightBrace {
                    parser.advance();
                    return Ok(Expr::StructExpr {
                        name: Box::new(expr),
                        vals,
                        span: Span::merge(sp_start, parser.previous().span.clone()),
                    });
                }
                loop {
                    parser.consume(TokenType::Comma, "Expected comma before expression")?;
                    vals.push(field_with_val(parser)?);

                    if parser.peek().token_type == TokenType::RightBrace {
                        parser.advance();
                        break;
                    }
                }

                Ok(Expr::StructExpr {
                    name: Box::new(expr),
                    vals,
                    span: Span::merge(sp_start, parser.previous().span.clone()),
                })
            } else {
                Ok(expr)
            }
        }
        _ => Ok(expr),
    }
}

fn field_with_val(parser: &mut Parser) -> Result<StructExprField, RigError> {
    let field = parser
        .consume(TokenType::Identifier, "Expected field name")?
        .lexeme
        .clone();
    parser.consume(TokenType::Colon, "Expected `:` after field name")?;
    let val = assignment(parser)?;

    Ok(StructExprField { name: field, val })
}

pub fn path(parser: &mut Parser) -> Result<Expr, RigError> {
    let mut path = Vec::new();
    let start_span = parser.peek().span.clone();
    path.push(
        parser
            .consume(TokenType::Identifier, "Expected identifier")?
            .lexeme
            .clone(),
    );

    if parser.check(TokenType::Scope) {
        parser.advance();
        path.extend(parse_path(parser)?);
        let end_span = parser.peek().span.clone();

        Ok(Expr::PathExpr {
            path,
            span: Span::merge(start_span, end_span),
        })
    } else {
        Ok(Expr::VariableExpr {
            name: path[0].clone(),
            span: parser.peek().span.clone(),
        })
    }
}

fn parse_path(parser: &mut Parser) -> Result<Vec<String>, RigError> {
    let name = parser.consume(TokenType::Identifier, "Expected identifier")?;
    let mut path = vec![name.lexeme.clone()];

    if parser.check(TokenType::Scope) {
        parser.advance();
        path.extend(parse_path(parser)?);
    }

    Ok(path)
}
