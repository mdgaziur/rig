use rig_ast::enum_variant::{EnumVariant, EnumVariantField};
use crate::expr::{expr, path, primary};
use crate::{name_with_type, Parser};
use rig_ast::expr::Expr;
use rig_ast::function_prototype::{Argument, FnType, Prototype};
use rig_ast::match_arms::MatchArm;
use rig_ast::stmt::Stmt;
use rig_ast::struct_field::StructField;
use rig_ast::token::TokenType;
use rig_ast::visibility::Visibility;
use rig_error::{ErrorCode, ErrorType, RigError};
use rig_span::Span;

pub fn program(parser: &mut Parser) -> Result<Stmt, RigError> {
    match parser.peek().token_type {
        TokenType::Keyword => match parser.peek().lexeme.as_str() {
            "impl" => struct_impl(parser),
            "extern" => extern_block(parser),
            _ => visibility(parser),
        },
        _ => Err(RigError::with_no_hint_and_notes(
            ErrorType::Hard,
            ErrorCode::E0005,
            "Expected `pub`, `use`, `fn`, `struct`, `impl`, `enum` or `let`",
            parser.peek().span.clone(),
        )),
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
            "mod" => mod_(parser, is_pub),
            "let" => let_(parser, is_pub),
            "enum" => enum_(parser, is_pub),
            _ => Err(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0005,
                "Expected `use`, `fn`, `struct`, `mod`, `enum` or `let`",
                parser.peek().span.clone(),
            )),
        },
        _ => Err(RigError::with_no_hint_and_notes(
            ErrorType::Hard,
            ErrorCode::E0005,
            "Expected `use`, `fn`, `struct`, `mod`, `enum` or `let`",
            parser.peek().span.clone(),
        )),
    }
}

fn enum_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();

    let name = parser
        .consume(TokenType::Identifier, "Expected enum name after `enum`")?
        .lexeme
        .clone();

    parser.consume(TokenType::LeftBrace, "Expected `{` after enum name")?;

    let mut variants = Vec::new();
    variants.push(parse_enum_variant(parser)?);

    while !parser.check(TokenType::RightBrace) && !parser.is_eof() {
        parser.consume(TokenType::Comma, "Expected comma before enum variant")?;
        variants.push(parse_enum_variant(parser)?);
    }

    parser.consume(TokenType::RightBrace, "Expected `}` after enum declaration")?;

    Ok(Stmt::EnumStmt {
        name,
        variants,
        visibility: Visibility::from(visibility),
        span: Span::merge(sp_start, parser.previous().span.clone())
    })
}

fn parse_enum_variant(parser: &mut Parser) -> Result<EnumVariant, RigError> {
    let name = parser.peek().lexeme.clone();
    parser.advance();

    if parser.check(TokenType::LeftBrace) {
        parser.advance();
        let mut fields = Vec::new();
        let field = name_with_type(parser)?;

        fields.push(EnumVariantField {
            name: field.0.lexeme,
            ty: field.1,
        });

        while !parser.check(TokenType::RightBrace) && !parser.is_eof() {
            parser.consume(TokenType::Comma, "Expected comma before enum variant field")?;
            let field = name_with_type(parser)?;

            fields.push(EnumVariantField {
                name: field.0.lexeme,
                ty: field.1,
            })
        }
        parser.consume(TokenType::RightBrace, "Expected `}` after enum variant")?;

        Ok(EnumVariant {
            name,
            fields: Some(fields),
        })
    } else {
        Ok(EnumVariant {
            name,
            fields: None,
        })
    }
}

fn struct_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let name = parser
        .consume(TokenType::Identifier, "Expected struct name after `struct`")?
        .lexeme
        .clone();
    let mut fields = Vec::new();

    parser.consume(TokenType::LeftBrace, "Expected `{` after struct name")?;

    while parser.peek().token_type != TokenType::RightBrace && !parser.is_eof() {
        let vis = if parser.peek().lexeme == "pub" {
            parser.advance();
            Visibility::Pub
        } else {
            Visibility::NotPub
        };

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
    let struct_name = parser
        .consume(TokenType::Identifier, "Expected struct name after `impl`")?
        .lexeme
        .clone();
    let mut methods = Vec::new();

    parser.consume(TokenType::LeftBrace, "Expected `{` after struct name")?;

    while parser.peek().token_type != TokenType::RightBrace || parser.is_eof() {
        methods.push(Box::new(struct_fn(parser)?));
    }

    parser.consume(TokenType::RightBrace, "Expected `}` after struct methods")?;

    Ok(Stmt::ImplStmt {
        struct_name,
        methods,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn struct_fn(parser: &mut Parser) -> Result<Stmt, RigError> {
    let keyword = parser
        .consume(
            TokenType::Keyword,
            "Expected keyword `fn` or `pub` inside struct impl",
        )?
        .lexeme
        .clone();
    let sp_start = parser.peek().span.clone();
    let visibility;

    if keyword == "pub" {
        visibility = Visibility::Pub;
        let keyword = parser
            .consume(TokenType::Keyword, "Expected keyword `fn` after `pub`")?
            .lexeme
            .clone();

        if keyword != "fn" {
            return Err(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0005,
                "Expected `fn`",
                parser.previous().span.clone(),
            ));
        }
    } else if keyword != "fn" {
        return Err(RigError::with_no_hint_and_notes(
            ErrorType::Hard,
            ErrorCode::E0005,
            "Expected `fn`",
            parser.previous().span.clone(),
        ));
    } else {
        visibility = Visibility::NotPub;
    }

    let method_name = parser
        .consume(TokenType::Identifier, "Expected name after `fn`")?
        .lexeme
        .clone();
    let mut args = Vec::new();

    parser.consume(TokenType::LeftParen, "Expected `(` after method name")?;

    let fn_type;

    if parser.peek().token_type == TokenType::Keyword && parser.peek().lexeme == "self" {
        fn_type = FnType::Method;
        parser.advance();
    } else if parser.peek().token_type != TokenType::RightParen {
        let arg = name_with_type(parser)?;

        args.push(Argument {
            name: arg.0.lexeme.clone(),
            type_: arg.1,
        });
        fn_type = FnType::Fn;
    } else {
        fn_type = FnType::Fn;
    }

    while parser.peek().token_type != TokenType::RightParen && !parser.is_eof() {
        parser.consume(TokenType::Comma, "Expected `,` before argument")?;
        let arg = name_with_type(parser)?;

        args.push(Argument {
            name: arg.0.lexeme.clone(),
            type_: arg.1,
        });
    }

    parser.consume(TokenType::RightParen, "Expected `)` after argument list")?;

    let return_ty = if parser.peek().token_type == TokenType::Arrow {
        parser.advance();
        Some(path(parser)?)
    } else {
        None
    };

    let prototype = Prototype {
        name: method_name,
        visibility: visibility.clone(),
        return_ty,
        args,
        fn_type,
    };

    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement")?;

    let body = Box::new(block_stmt(parser)?);

    Ok(Stmt::FnStmt {
        prototype,
        body,
        visibility,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn extern_block(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();

    let mut prototypes = Vec::new();

    parser.consume(TokenType::LeftBrace, "Expected `{` after `extern`")?;

    while parser.peek().token_type != TokenType::RightBrace && !parser.is_eof() {
        let vis;
        if parser.peek().lexeme == "pub" {
            vis = true;
            parser.advance();

            if parser.peek().lexeme != "fn" {
                return Err(RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0005,
                    "Expected `fn`",
                    parser.peek().span.clone(),
                ));
            }
        } else if parser.peek().lexeme != "fn" {
            return Err(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0005,
                "Expected `fn` or `pub`",
                parser.peek().span.clone(),
            ));
        } else {
            vis = false;
        }
        parser.advance();

        prototypes.push(prototype(parser, vis)?);
        parser.consume(TokenType::Semicolon, "Expected `;` after prototype")?;
    }

    parser.consume(TokenType::RightBrace, "Expected `}` after `extern` body")?;

    Ok(Stmt::ExternStmt {
        prototypes,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn use_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();

    parser.advance();
    let import_path = path(parser)?;

    parser.consume(TokenType::Semicolon, "Expected semicolon after path")?;

    Ok(Stmt::UseStmt {
        path: import_path,
        visibility: Visibility::from(visibility),
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn fn_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let prototype = prototype(parser, visibility)?;

    parser.consume(
        TokenType::LeftBrace,
        "Expected '{' after function prototype",
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

        let statement = if parser.peek().lexeme == "continue" || parser.peek().lexeme == "break" {
            Err(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0005,
                &format!("Found `{}` outside a loop", parser.peek().lexeme),
                parser.peek().span.clone(),
            ))
        } else {
            stmt(parser, "")
        };

        if let Err(e) = statement {
            parser.block_stmt_errs.push(e);
            parser.synchronize();
        } else if let Ok(stmt) = statement {
            stmts.push(Box::new(stmt));
        }
    }
    parser.consume(
        TokenType::RightBrace,
        "Expected `}` at the end of block statement",
    )?;

    Ok(Stmt::BlockStmt {
        exprs: stmts,
        span: Span::merge(start_sp, parser.previous().span.clone()),
    })
}

fn loop_body(parser: &mut Parser) -> Result<Stmt, RigError> {
    let start_sp = parser.previous().span.clone();
    let mut stmts = Vec::new();

    loop {
        if parser.check(TokenType::RightBrace) || parser.is_eof() {
            break;
        }

        let stmt = match parser.peek().token_type {
            TokenType::Keyword => match parser.peek().lexeme.as_str() {
                "continue" => continue_(parser),
                "break" => break_(parser),
                _ => stmt(parser, "`continue`, `break`"),
            },
            _ => expr_stmt(parser),
        };

        if let Err(e) = stmt {
            parser.block_stmt_errs.push(e);
            parser.synchronize();
        } else if let Ok(stmt) = stmt {
            stmts.push(Box::new(stmt));
        }
    }
    parser.consume(
        TokenType::RightBrace,
        "Expected `}` at the end of block statement",
    )?;

    Ok(Stmt::BlockStmt {
        exprs: stmts,
        span: Span::merge(start_sp, parser.previous().span.clone()),
    })
}

/// Parses valid statement inside blocks
fn stmt(parser: &mut Parser, extra_expectations: &str) -> Result<Stmt, RigError> {
    match parser.peek().token_type {
        TokenType::Keyword => match parser.peek().lexeme.as_str() {
            "let" => let_(parser, false),
            "use" => use_(parser, false),
            "mod" => mod_(parser, false),
            "struct" => struct_(parser, false),
            "extern" => extern_block(parser),
            "impl" => struct_impl(parser),
            "while" => while_(parser),
            "if" => conditional_(parser),
            "for" => for_(parser),
            "loop" => loop_(parser),
            "print" => print(parser),
            "return" => return_(parser),
            "enum" => enum_(parser, false),
            "match" => match_(parser),
            _ => Err(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0005,
                &format!("Expected `let`, `use`, `mod`, `struct`, `extern`, `impl`, \
                                 `while`, `if`, `for`, `loop`, `print`, `return`, `enum`, `match`, {extra_expectations}"),
                parser.peek().span.clone(),
            )),
        },
        TokenType::LeftBrace => {
            parser.advance();
            block_stmt(parser)
        }
        _ => expr_stmt(parser),
    }
}

fn match_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();

    let matched = path(parser)?;
    let mut arms = Vec::new();

    parser.consume(TokenType::LeftBrace, "Expected `{`")?;

    while !parser.check(TokenType::RightBrace) && !parser.is_eof() {
        arms.push(parse_arm(parser)?);
    }

    parser.consume(TokenType::RightBrace, "Expected `}`")?;

    Ok(Stmt::MatchStmt {
        matched,
        arms,
        span: Span::merge(sp_start, parser.previous().span.clone())
    })
}

fn parse_arm(parser: &mut Parser) -> Result<MatchArm, RigError> {
    let match_ = primary(parser)?;

    parser.consume(TokenType::FatArrow, "Expected `=>`")?;
    parser.consume(TokenType::LeftBrace, "Expected `}`")?;

    let body = block_stmt(parser)?;

    Ok(MatchArm {
        match_,
        body,
    })
}

fn mod_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();

    let mod_name = parser
        .consume(TokenType::Identifier, "Expected module name")?
        .lexeme
        .clone();

    let body = if parser.peek().token_type == TokenType::LeftBrace {
        parser.advance();
        let mut stmts = Vec::new();
        while parser.peek().token_type != TokenType::RightBrace && !parser.is_eof() {
            stmts.push(program(parser)?);
        }

        parser.consume(TokenType::RightBrace, "Expected `}`")?;
        Some(stmts)
    } else {
        parser.consume(TokenType::Semicolon, "Expected `;`")?;
        None
    };

    Ok(Stmt::ModStmt {
        name: mod_name,
        body,
        visibility: Visibility::from(visibility),
        span: Span::merge(sp_start, parser.peek().span.clone()),
    })
}

fn while_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let condition = expr(parser)?;
    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement")?;
    let body = Box::new(loop_body(parser)?);

    Ok(Stmt::WhileStmt {
        condition,
        body,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn conditional_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let condition = expr(parser)?;
    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement")?;
    let body = Box::new(block_stmt(parser)?);
    let else_branch;

    if parser.peek().lexeme == "else" {
        parser.advance();
        if parser.peek().lexeme == "if" {
            else_branch = Some(Box::new(conditional_(parser)?));
        } else {
            let sp_start = parser.previous().span.clone();
            parser.consume(TokenType::LeftBrace, "Expected `{` before block statement")?;
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
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn for_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let var = parser
        .consume(TokenType::Identifier, "Expected variable name after `for`")?
        .lexeme
        .clone();

    let in_ = parser
        .consume(TokenType::Keyword, "Expected `in` after variable name")?
        .lexeme
        .clone();
    if in_ != "in" {
        return Err(RigError::with_no_hint_and_notes(
            ErrorType::Hard,
            ErrorCode::E0005,
            "Expected `in`",
            parser.previous().span.clone(),
        ));
    }

    let iterable = expr(parser)?;

    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement")?;

    let body = Box::new(loop_body(parser)?);

    Ok(Stmt::ForStmt {
        var,
        iterable,
        body,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn loop_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();

    parser.consume(TokenType::LeftBrace, "Expected `{` before block statement")?;
    let body = Box::new(loop_body(parser)?);

    Ok(Stmt::WhileStmt {
        condition: Expr::BooleanLiteralExpr {
            value: true,
            span: sp_start.clone(),
        },
        body,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn return_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let expr = expr(parser)?;
    parser.consume(TokenType::Semicolon, "Expected `;` after expression")?;

    Ok(Stmt::ReturnStmt {
        expr,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn print(parser: &mut Parser) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let expr = expr(parser)?;
    parser.consume(TokenType::Semicolon, "Expected `;` after expression")?;

    Ok(Stmt::PrintStmt {
        expr,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}

fn break_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let break_span = parser.peek().span.clone();
    parser.advance();

    Ok(Stmt::BreakStmt {
        span: Span::merge(break_span,
                          parser.consume(TokenType::Semicolon, "Expected `;` after `break`")?
                              .span.clone()),
    })
}

fn continue_(parser: &mut Parser) -> Result<Stmt, RigError> {
    let continue_span = parser.peek().span.clone();
    parser.advance();

    Ok(Stmt::ContinueStmt {
        span: Span::merge(continue_span,
                          parser.consume(TokenType::Semicolon, "Expected `;` after `continue`")?
                              .span.clone()),
    })
}

fn expr_stmt(parser: &mut Parser) -> Result<Stmt, RigError> {
    let start_sp = parser.peek().span.clone();
    let expr = expr(parser)?;
    parser.consume(TokenType::Semicolon, "Expected `;` after expression")?;

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
        )?
        .lexeme
        .clone();
    parser.consume(TokenType::LeftParen, "Expected '(' after function name")?;
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
        fn_type: FnType::Fn,
    })
}

fn let_(parser: &mut Parser, visibility: bool) -> Result<Stmt, RigError> {
    let sp_start = parser.peek().span.clone();
    parser.advance();
    let name = parser
        .consume(TokenType::Identifier, "Expected name after `let`")?
        .lexeme
        .clone();
    let mut ty = None;

    if parser.peek().token_type == TokenType::Colon {
        parser.advance();
        ty = Some(path(parser)?);
    }

    parser.consume(TokenType::Equal, "Expected `=` after name")?;
    let value = expr(parser)?;
    parser.consume(
        TokenType::Semicolon,
        "Expected `;` after variable declaration",
    )?;

    Ok(Stmt::LetStmt {
        visibility: Visibility::from(visibility),
        name,
        value,
        ty,
        span: Span::merge(sp_start, parser.previous().span.clone()),
    })
}
