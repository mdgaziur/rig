use rig_ast::expr::Expr;
use crate::expr::{expr, path};
use crate::{name_with_type, Parser};
use rig_ast::function_prototype::{Argument, FnType, Prototype};
use rig_ast::stmt::Stmt;
use rig_ast::struct_field::StructField;
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

fn struct_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let name = parser
        .consume(
            TokenType::Identifier,
            "Expected struct name after `struct`",
            None,
        )?
        .lexeme
        .clone();
    let mut fields = Vec::new();

    parser.consume(TokenType::LeftBrace, "Expected `{` after struct name", None)?;

    while parser.peek().token_type != TokenType::RightBrace && !parser.is_eof() {
        let vis;
        if parser.peek().lexeme == "pub" {
            parser.advance();
            vis = Visibility::Pub;
        } else {
            vis = Visibility::NotPub;
        }

        let name_w_ty = name_with_type(parser)?;

        fields.push(StructField {
            visibility: vis,
            name: name_w_ty.0.lexeme,
            ty: name_w_ty.1,
        });

        if parser.peek().token_type != TokenType::Comma {
            break;
        }
        parser.advance(); // eat comma
    }

    parser.consume(
        TokenType::RightBrace,
        "Expected `}` after struct field list",
        None,
    )?;

    Ok(Stmt::StructStmt {
        visibility: Visibility::from(visibility),
        name,
        fields,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn struct_impl(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let struct_name = parser.consume(TokenType::Identifier, "Expected struct name after `impl`", None)?
        .lexeme
        .clone();
    let mut methods = Vec::new();

    parser.consume(TokenType::LeftBrace, "Expected `{` after struct name", None)?;

    while parser.peek().token_type != TokenType::RightBrace || parser.is_eof() {
        methods.push(Box::new(struct_fn(parser)?));
    }

    parser.consume(TokenType::RightBrace, "Expected `}` after struct methods", None)?;

    Ok(Stmt::ImplStmt {
        struct_name,
        methods,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn struct_fn(parser: &mut Parser) -> Result<Stmt, RigError> {
    let keyword = parser.consume(TokenType::Keyword, "Expected keyword `fn` or `pub` inside struct impl", None)?
        .lexeme
        .clone();
    let sp_start = parser.peek().span.clone();
    let visibility;

    if keyword == "pub" {
        visibility = Visibility::Pub;
        let keyword = parser.consume(TokenType::Keyword, "Expected keyword `fn` after `pub`", None)?
            .lexeme
            .clone();

        if keyword != "fn" {
            return Err(RigError {
                error_type: ErrorType::Hard,
                error_code: String::from("E0005"),
                message: String::from("Expected keyword `fn` after `pub`"),
                span: parser.previous().span.clone(),
                hint: None,
                file_path: parser.source_path.to_string()
            })
        }
    } else if keyword != "fn" {
        return Err(RigError {
            error_type: ErrorType::Hard,
            error_code: String::from("E0005"),
            message: String::from("Expected keyword `fn`"),
            span: parser.previous().span.clone(),
            hint: None,
            file_path: parser.source_path.to_string()
        })
    } else {
        visibility = Visibility::NotPub;
    }

    let method_name = parser.consume(TokenType::Identifier, "Expected name after `fn`", None)?
        .lexeme
        .clone();
    let mut args = Vec::new();

    parser.consume(TokenType::LeftParen, "Expected `(` after method name", None)?;

    let fn_type;

    if parser.peek().token_type == TokenType::Keyword && parser.peek().lexeme == "self" {
        fn_type = FnType::Method;
        parser.advance();
    } else {
        let arg = name_with_type(parser)?;

        args.push(Argument {
            name: arg.0.lexeme.clone(),
            type_: arg.1,
        });
        fn_type = FnType::Fn;
    }

    while parser.peek().token_type != TokenType::RightParen && !parser.is_eof() {
        parser.consume(TokenType::Comma, "Expected `,` before argument", None)?;
        let arg = name_with_type(parser)?;

        args.push(Argument {
            name: arg.0.lexeme.clone(),
            type_: arg.1,
        });
    }

    parser.consume(TokenType::RightParen, "Expected `)` after argument list", None)?;

    let return_ty;
    if parser.peek().token_type == TokenType::Arrow {
        parser.advance();
        return_ty = Some(path(parser)?);
    } else {
        return_ty = None;
    }

    let prototype = Prototype {
        name: method_name,
        visibility: visibility.clone(),
        return_ty,
        args,
        fn_type,
    };

    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement", None)?;

    let body = Box::new(block_stmt(parser)?);

    Ok(Stmt::FnStmt {
        prototype,
        body,
        visibility,
        span: Span::merge(sp_start, parser.previous().span.clone())
    })
}

fn extern_block(_parser: &mut Parser) -> Result<Stmt, RigError> {
    todo!()
}

fn use_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();

    parser.advance();
    let import_path = path(parser)?;

    parser.consume(TokenType::Semicolon, "Expected semicolon after path", None)?;

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

    parser.consume(
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
        let stmt = stmt(parser);
        if let Err(e) = stmt {
            parser.synchronize();
            e.print(parser.source);
            parser.has_error_inside_block_stmt = true;
            continue;
        } else if let Ok(stmt) = stmt {
            stmts.push(Box::new(stmt));
        }
    }
    parser.consume(
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
        TokenType::Keyword => match parser.peek().lexeme.as_str() {
            "let" => let_(parser, false),
            "use" => use_(parser, false),
            "struct" => struct_(parser, false),
            "impl" => struct_impl(parser),
            "while" => while_(parser),
            "if" => conditional_(parser),
            "for" => for_(parser),
            "loop" => loop_(parser),
            "break" => break_(parser),
            "continue" => continue_(parser),
            "print" => print(parser),
            "return" => return_(parser),
            _ => Err(RigError {
                error_code: String::from("E0005"),
                message: format!(
                    "Expected expression, `if`, `for`, `while`, \
                    `struct`, `impl`, `use`, `let`, `print`, `break`, `continue` \
                     or `extern`. But found: `{}`",
                    parser.peek().lexeme
                ),
                error_type: ErrorType::Hard,
                file_path: parser.source_path.to_string(),
                hint: None,
                span: parser.peek().span.clone(),
            }),
        },
        TokenType::LeftBrace => {
            parser.advance();
            block_stmt(parser)
        }
        _ => expr_stmt(parser),
    }
}

fn while_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    parser.consume(TokenType::LeftParen, "Expected `(`", None)?;
    let condition = expr(parser)?;
    parser.consume(TokenType::RightParen, "Expected `)`", None)?;
    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement", None)?;
    let body = Box::new(block_stmt(parser)?);

    Ok(Stmt::WhileStmt {
        condition,
        body,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn conditional_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    parser.consume(TokenType::LeftParen, "Expected `(`", None)?;
    let condition = expr(parser)?;
    parser.consume(TokenType::RightParen, "Expected `)`", None)?;
    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement", None)?;
    let body = Box::new(block_stmt(parser)?);
    let else_branch;

    if parser.peek().lexeme == "else" {
        parser.advance();
        if parser.peek().lexeme == "if" {
            else_branch = Some(Box::new(conditional_(parser)?));
        } else {
            let sp_start = parser.previous().span.clone();
            parser.consume(TokenType::LeftBrace, "Expected `{` before block statement", None)?;
            let body = Box::new(block_stmt(parser)?);
            else_branch = Some(Box::new(Stmt::IfStmt {
                condition: Expr::BooleanLiteralExpr {
                    value: true,
                    span: parser.previous().span.clone(),
                },
                body,
                else_branch: None,
                span: Span::merge(sp_start, parser.previous().span.clone()),
            }));
        }
    } else {
        else_branch = None;
    }

    Ok(Stmt::IfStmt {
        condition,
        body,
        else_branch,
        span: Span::merge(sp_start, parser.previous().span.clone())
    })
}

fn for_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    parser.consume(TokenType::LeftParen, "Expected `(`", None)?;
    let var = parser.consume(TokenType::Identifier, "Expected variable name after `for`", None)?
        .lexeme
        .clone();

    let in_ = parser.consume(TokenType::Keyword, "Expected `in` after variable name", None)?
        .lexeme
        .clone();
    if in_ != "in" {
        return Err(RigError {
            error_type: ErrorType::Hard,
            error_code: String::from("E0005"),
            message: String::from("Expected keyword `in`"),
            span: parser.previous().span.clone(),
            hint: None,
            file_path: parser.source_path.to_string()
        })
    }

    let iterable = expr(parser)?;
    parser.consume(TokenType::RightParen, "Expected `)`", None)?;

    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement", None)?;

    let body = Box::new(block_stmt(parser)?);

    Ok(Stmt::ForStmt {
        var,
        iterable,
        body,
        span: Span::merge(sp_start, parser.previous().span.clone())
    })
}

fn loop_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();

    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement", None)?;
    let body = Box::new(block_stmt(parser)?);

    Ok(Stmt::WhileStmt {
        condition: Expr::BooleanLiteralExpr {
            value: true,
            span: sp_start.clone(),
        },
        body,
        span: Span::merge(sp_start, parser.previous().span.clone())
    })
}

fn return_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let expr = expr(parser)?;
    parser.consume(TokenType::Semicolon, "Expected `;` after expression", None)?;

    Ok(Stmt::ReturnStmt {
        expr,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn print(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let expr = expr(parser)?;
    parser.consume(TokenType::Semicolon, "Expected `;` after expression", None)?;

    Ok(Stmt::PrintStmt {
        expr,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn break_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let brek = Ok(Stmt::BreakStmt {
        span: parser.peek().span.clone(),
    });
    parser.advance();
    parser.consume(TokenType::Semicolon, "Expected `;` after `break`", None)?;
    brek
}

fn continue_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let contnue = Ok(Stmt::ContinueStmt {
        span: parser.peek().span.clone(),
    });
    parser.advance();
    parser.consume(TokenType::Semicolon, "Expected `;` after `break`", None)?;
    contnue
}

fn expr_stmt(parser: &mut Parser) -> Result<Stmt, RigError> {
    let start_sp = parser.peek().span.clone();
    let expr = expr(parser)?;
    parser.consume(TokenType::Semicolon, "Expected `;` after expression", None)?;

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
    parser.consume(
        TokenType::LeftParen,
        "Expected '(' after function name",
        None,
    )?;
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

    parser.consume(
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
        fn_type: FnType::Fn
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

    parser.consume(TokenType::Equal, "Expected `=` after name", None)?;
    let value = expr(parser)?;
    parser.consume(
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
