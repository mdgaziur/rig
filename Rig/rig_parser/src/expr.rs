use rig_ast::expr::Expr;
use rig_ast::token::TokenType;
use rig_error::{ErrorType, RigError};
use rig_span::Span;
use crate::Parser;

pub fn expr(_parser: &mut Parser) -> Result<Expr, RigError> {
    Ok(Expr::IntegerLiteralExpr {
        span: Span::for_single_char("onga_bonga", 0, 0),
        value: 69420
    })
}

pub fn primary(parser: &mut Parser) -> Result<Expr, RigError> {
    match parser.peek().token_type {
        TokenType::Identifier => {
            path(parser)
        }
        TokenType::NumberLiteral => {
            if parser.peek().lexeme.contains('.') {
                Ok(Expr::FloatLiteralExpr {
                    value: parser.peek().lexeme.parse().expect(
                        "Lexer emitted invalid float literal or value is too big to store in f64",
                    ),
                    span: parser.peek().span.clone(),
                })
            } else {
                Ok(Expr::IntegerLiteralExpr {
                    value: parser.peek().lexeme.parse().expect(
                        "Lexer emitted invalid integer literal or value is too big to store in i64",
                    ),
                    span: parser.peek().span.clone(),
                })
            }
        }
        TokenType::Keyword => match parser.peek().lexeme.as_str() {
            "true" => Ok(Expr::IntegerLiteralExpr {
                value: 1,
                span: parser.peek().span.clone(),
            }),
            "false" | "null" => Ok(Expr::IntegerLiteralExpr {
                value: 0,
                span: parser.peek().span.clone(),
            }),
            _ => Err(RigError {
                error_type: ErrorType::Hard,
                error_code: String::from("E0005"),
                message: format!(
                    "Expected `true`/`false`/`null`, found `{}`",
                    &parser.peek().lexeme
                ),
                hint: None,
                span: parser.peek().span.clone(),
                file_path: parser.source_path.to_string(),
            }),
        },
        TokenType::LeftParen => {
            let expr = expr(parser)?;
            parser.consume(TokenType::RightParen, "Expected `)` after expression", None)?;

            Ok(expr)
        }
        _ => Err(RigError {
            error_type: ErrorType::Hard,
            error_code: String::from("E0005"),
            message: format!(
                "Expected primary expression`, found `{}`",
                &parser.peek().lexeme
            ),
            hint: None,
            span: parser.peek().span.clone(),
            file_path: parser.source_path.to_string(),
        }),
    }
}

pub fn path(parser: &mut Parser) -> Result<Expr, RigError> {
    let mut path = Vec::new();
    let start_span = parser.peek().span.clone();
    path.push(parser.consume(TokenType::Identifier, "Expected identifier", None)?.lexeme.clone());

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
            span: parser.peek().span.clone()
        })
    }
}

fn parse_path(parser: &mut Parser) -> Result<Vec<String>, RigError> {
    let name = parser.consume(TokenType::Identifier, "Expected identifier", None)?;
    let mut path = Vec::new();
    path.push(name.lexeme.clone());

    if parser.check(TokenType::Scope) {
        parser.advance();
        path.extend(parse_path(parser)?);
    }

    Ok(path)
}