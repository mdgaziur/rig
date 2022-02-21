use rig_ast::stmt::Stmt;
use rig_error::{ErrorCode, ErrorType, RigError};

pub fn validate_ast(ast: &[Stmt], file_path: &str) -> Vec<RigError> {
    let mut warnings = Vec::new();

    for node in ast {
        match node {
            Stmt::FnStmt { body, .. } => {
                let res = validate_fn(body, file_path);
                warnings.extend(res);
            }
            _ => (),
        }
    }

    warnings
}

fn validate_fn(body: &Stmt, file_path: &str) -> Vec<RigError> {
    let mut warnings = Vec::new();
    let mut encountered_return = false;

    match body {
        Stmt::BlockStmt { exprs, .. } => {
            for expr_stmt in exprs {
                match &**expr_stmt {
                    Stmt::ReturnStmt { .. } => encountered_return = true,
                    _ => {
                        if encountered_return {
                            warnings.push(RigError {
                                error_code: ErrorCode::E0007,
                                error_type: ErrorType::Soft,
                                file_path: file_path.to_string(),
                                message: String::from("This part will never execute because the function always returns before coming to this part"),
                                hint: None,
                                span: expr_stmt.get_span(),
                            })
                        }
                    }
                }
            }
        }
        _ => panic!("encountered parser bug: function contains Stmt other than BlockStmt as body"),
    }

    warnings
}
