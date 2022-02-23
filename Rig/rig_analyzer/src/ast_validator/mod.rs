use rig_ast::stmt::Stmt;
use rig_error::{ErrorCode, ErrorType, Note, RigError};
use rig_span::Span;

pub fn validate_ast(ast: &[Stmt]) -> Vec<RigError> {
    let mut warnings = Vec::new();

    for node in ast {
        match node {
            Stmt::FnStmt { body, .. } => {
                let res = validate_fn(body);
                warnings.extend(res);
            }
            _ => (),
        }
    }

    warnings
}

fn validate_fn(body: &Stmt) -> Vec<RigError> {
    let mut warnings = Vec::new();
    let mut return_span = None;
    let mut unreachable_span: Option<Span> = None;

    match body {
        Stmt::BlockStmt { exprs, .. } => {
            for expr_stmt in exprs {
                match &**expr_stmt {
                    Stmt::ReturnStmt { span, .. } => return_span = Some(span),
                    _ => {
                        if let Some(_) = return_span {
                            if let Some(prev_span) = &unreachable_span {
                                unreachable_span =
                                    Some(Span::merge(prev_span.clone(), expr_stmt.get_span()));
                            } else {
                                unreachable_span = Some(expr_stmt.get_span());
                            }
                        }
                    }
                }
            }
        }
        _ => panic!("encountered parser bug: function contains Stmt other than BlockStmt as body"),
    }

    if let Some(return_span) = return_span {
        if let Some(unreachable_span) = unreachable_span {
            warnings.push(RigError {
                error_type: ErrorType::Soft,
                error_code: ErrorCode::E0007,
                message: String::from("Unreachable code"),
                span: unreachable_span.clone(),
                hint: None,
                hint_span: None,
                notes: vec![Note {
                    message: String::from("Function returns here"),
                    span: return_span.clone(),
                }],
            })
        }
    }
    warnings
}
