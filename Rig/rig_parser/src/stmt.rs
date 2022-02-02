use rig_ast::stmt::Stmt;
use rig_ast::token::TokenType;
use rig_ast::visibility::Visibility;
use rig_error::{ErrorType, RigError};
use rig_span::Span;
use crate::expr::path;
use crate::Parser;

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

fn fn_(_parser: &mut Parser, _visibility: bool) -> Result<Stmt, RigError> {
    todo!()
}

fn let_(_parser: &mut Parser, _visibility: bool) -> Result<Stmt, RigError> {
    todo!()
}