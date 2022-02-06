use crate::expr::{expr, path};
use crate::{name_with_type, Parser};
use rig_ast::function_prototype::{Argument, Prototype};
use rig_ast::stmt::Stmt;
use rig_ast::token::TokenType;
use rig_ast::visibility::Visibility;
use rig_error::{ErrorType, RigError};
use rig_span::Span;

pub fn program(parser: &mut Parser) -> Result<Stmt, RigError> {
    match parser.peek().token_type {
        TokenType::Keyword => {
            if parser.peek().lexeme == "impl" {
                struct_impl(parser)
            } else if parser.peek().lexeme == "extern" {
                extern_block(parser)
            } else {
                visibility(parser)
            }
        }
        _ => Err(RigError {
            error_type: ErrorType::Hard,
            error_code: String::from("E0005"),
            message: format!(
                "Expected `pub`/`use`/`fn`/`struct`/`impl`/`let`, found `{}`",
                &parser.peek().lexeme
            ),
            hint: None,
            file_path: parser.source_path.to_string(),

            span: parser.peek().span.clone(),
        }),
    }
}

fn visibility(parser: &mut Parser) -> Result<Stmt, RigError> {
    let is_pub;

    if parser.peek().lexeme == "pub" {
        is_pub = true;
        parser.advance();
    } else {
        is_pub = false;
    }

    match parser.peek().token_type {
        TokenType::Keyword => match parser.peek().lexeme.as_str() {
            "use" => use_(parser, is_pub),
            "fn" => fn_(parser, is_pub),
            "struct" => struct_(parser, is_pub),
            "let" => let_(parser, is_pub),
            _ => Err(RigError {
                error_type: ErrorType::Hard,
                error_code: String::from("E0005"),
                message: format!(
                    "Expected `use`/`fn`/`struct`/`let`, found `{}`",
                    &parser.peek().lexeme
                ),
                hint: None,
                span: parser.peek().span.clone(),
                file_path: parser.source_path.to_string(),
            }),
        },
        _ => Err(RigError {
            error_type: ErrorType::Hard,
            error_code: String::from("E0005"),
            message: format!(
                "Expected `use`/`fn`/`struct`/`let`, found `{}`",
                &parser.peek().lexeme
            ),
            hint: None,
            span: parser.peek().span.clone(),
            file_path: parser.source_path.to_string(),
        }),
    }
}

fn struct_(_parser: &mut Parser, _visibility: bool) -> Result<Stmt, RigError> {
    todo!()
}

fn struct_impl(_parser: &mut Parser) -> Result<Stmt, RigError> {
    todo!()
}

fn extern_block(_parser: &mut Parser) -> Result<Stmt, RigError> {
    todo!()
}

fn use_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();

    parser.advance();
    let import_path = path(parser)?;

    let _ = parser.consume(TokenType::Semicolon, "Expected semicolon after path", None)?;

    Ok(Stmt::UseStmt {
        path: import_path,
        visibility: Visibility::from(visibility),
        span: Span::merge(sp_start, parser.peek().span.clone()),
    })
}

fn fn_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let prototype = prototype(parser, visibility)?;

    let _ = parser.consume(
        TokenType::LeftBrace,
        "Expected '{' after function prototype",
        None,
    )?;

    let body = Box::new(block_stmt(parser)?);

    Ok(Stmt::FnStmt {
        prototype,
        visibility: Visibility::from(visibility),
        body,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn block_stmt(parser: &mut Parser) -> Result<Stmt, RigError> {
    let start_sp = parser.previous().span.clone();
    let mut stmts = Vec::new();
    loop {
        if parser.check(TokenType::RightBrace) || parser.is_eof() {
            break;
        }
        stmts.push(Box::new(stmt(parser)?));
    }
    let _ = parser.consume(
        TokenType::RightBrace,
        "Expected `}` at the end of block statement",
        None,
    )?;

    Ok(Stmt::BlockStmt {
        exprs: stmts,
        span: Span::merge(start_sp, parser.previous().span.clone()),
    })
}

/// Parses valid statement inside blocks
fn stmt(parser: &mut Parser) -> Result<Stmt, RigError> {
    match parser.peek().token_type {
        TokenType::Keyword => {
            match parser.peek().lexeme.as_str() {
                "let" => let_(parser, false),
                "use" => use_(parser, false),
                "break" => break_(parser),
                "continue" => continue_(parser),
                _ => Err(RigError {
                    error_code: String::from("E0005"),
                    message: format!("Expected expression, `if`, `for`, `while`, \
                    `struct`, `impl`, `use`, `let`, `print`, `break`, `continue` \
                     or `extern`. But found: `{}`", parser.peek().lexeme),
                    error_type: ErrorType::Hard,
                    file_path: parser.source_path.to_string(),
                    hint: None,
                    span: parser.peek().span.clone(),
                })
            }
        },
        _ => expr_stmt(parser),
    }
}

fn break_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let brek = Ok(Stmt::BreakStmt {
        span: parser.peek().span.clone(),
    });
    parser.advance();
    let _ = parser.consume(TokenType::Semicolon, "Expected `;` after `break`", None)?;
    brek
}

fn continue_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let contnue = Ok(Stmt::ContinueStmt {
        span: parser.peek().span.clone(),
    });
    parser.advance();
    let _ = parser.consume(TokenType::Semicolon, "Expected `;` after `break`", None)?;
    contnue
}

fn expr_stmt(parser: &mut Parser) -> Result<Stmt, RigError> {
    let start_sp = parser.peek().span.clone();
    let expr = expr(parser)?;
    let _ = parser.consume(TokenType::Semicolon, "Expected `;` after expression", None)?;

    Ok(Stmt::ExprStmt {
        expr,
        span: Span::merge(start_sp, parser.previous().span.clone()),
    })
}

fn prototype(parser: &mut Parser, visibility: bool) -> Result<Prototype, RigError> {
    let name = parser
        .consume(
            TokenType::Identifier,
            "Expected identifier after 'fn' keyword",
            None,
        )?
        .lexeme
        .clone();
    let _ = parser.consume(
        TokenType::LeftParen,
        "Expected '(' after function name",
        None,
    );
    let mut args = Vec::new();

    if !parser.check(TokenType::RightParen) {
        loop {
            let name_with_ty = name_with_type(parser)?;
            args.push(Argument {
                name: name_with_ty.0.lexeme,
                type_: name_with_ty.1,
            });

            if !parser.check(TokenType::Comma) {
                break;
            }
            parser.advance();
        }
    }

    let _ = parser.consume(
        TokenType::RightParen,
        "Expected ')' after function argument list",
        None,
    )?;

    let mut return_ty = None;

    if parser.check(TokenType::Arrow) {
        parser.advance();
        return_ty = Some(path(parser)?);
    }

    Ok(Prototype {
        name,
        args,
        visibility: Visibility::from(visibility),
        return_ty,
    })
}

fn let_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.previous().span.clone();
    parser.advance();
    let name = parser
        .consume(TokenType::Identifier, "Expected name after `let`", None)?
        .lexeme
        .clone();
    let mut ty = None;

    if parser.peek().token_type == TokenType::Colon {
        parser.advance();
        ty = Some(path(parser)?);
    }

    let _ = parser.consume(TokenType::Equal, "Expected `=` after name", None)?;
    let value = expr(parser)?;
    let _ = parser.consume(
        TokenType::Semicolon,
        "Expected `;` after variable declaration",
        None,
    )?;

    Ok(Stmt::LetStmt {
        visibility: Visibility::from(visibility),
        name,
        value,
        ty,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}
