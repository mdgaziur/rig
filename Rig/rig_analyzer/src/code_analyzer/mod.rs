use rig_ast::stmt::Stmt;
use rig_error::{ErrorCode, ErrorType, Note, RigError};
use rig_span::Span;

pub fn analyze_code(ast: &[Stmt]) -> Vec<RigError> {
    let mut warnings = Vec::new();

    for node in ast {
        match node {
            Stmt::FnStmt { body, .. } => warnings.extend(analyze_block(body)),
            Stmt::ImplStmt { methods, .. } => {
                for method in methods {
                    match &**method {
                        Stmt::FnStmt { body, .. } => warnings.extend(analyze_block(body)),
                        _ => unreachable!(),
                    }
                }
            }
            _ => (),
        }
    }

    warnings
}

fn analyze_block(body: &Stmt) -> Vec<RigError> {
    let mut warnings = Vec::new();
    let mut note_span = None;
    let mut message = String::new();
    let mut unreachable_span: Option<Span> = None;

    match body {
        Stmt::BlockStmt { exprs, .. } => {
            for expr_stmt in exprs {
                if note_span.is_some() {
                    if let Some(prev_span) = &unreachable_span {
                        unreachable_span =
                            Some(Span::merge(prev_span.clone(), expr_stmt.get_span()));
                    } else {
                        unreachable_span = Some(expr_stmt.get_span());
                    }
                } else {
                    match &**expr_stmt {
                        Stmt::IfStmt { .. } => {
                            warnings.extend(analyze_if_stmt(expr_stmt));
                        }
                        Stmt::WhileStmt { body, .. } | Stmt::ForStmt { body, .. } => {
                            warnings.extend(analyze_block(body))
                        }
                        Stmt::ReturnStmt { span, .. } => {
                            message = String::from("Code returns here");
                            note_span = Some(span.clone());
                        }
                        Stmt::BreakStmt { span } => {
                            message = String::from("Loop stops here");
                            note_span = Some(span.clone());
                        }
                        Stmt::ContinueStmt { span } => {
                            message = String::from("Loop ignores code after this");
                            note_span = Some(span.clone());
                        }
                        _ => (),
                    }
                }
            }
        }
        _ => unreachable!(),
    }

    if let Some(return_span) = note_span {
        if let Some(unreachable_span) = unreachable_span {
            warnings.push(RigError {
                error_type: ErrorType::Soft,
                error_code: ErrorCode::E0007,
                message: String::from("Unreachable code"),
                span: unreachable_span,
                hint: None,
                hint_span: None,
                notes: vec![Note {
                    message,
                    span: return_span,
                }],
            })
        }
    }
    warnings
}

fn analyze_if_stmt(stmt: &Stmt) -> Vec<RigError> {
    let mut warnings = Vec::new();

    match stmt {
        Stmt::IfStmt {
            body, else_branch, ..
        } => {
            warnings.extend(analyze_block(body));

            if let Some(branch) = else_branch {
                warnings.extend(analyze_if_stmt(branch));
            }
        }
        _ => unreachable!(),
    }

    warnings
}
