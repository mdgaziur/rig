use crate::import;

use rig_ast::stmt::Stmt;

use rig_error::RigError;

use rig_project::Project;
use rig_session::Session;

use rig_types::checked_stmt::CheckedStmt;
use rig_types::{ModuleId, ScopeId};
use std::collections::HashMap;

pub fn typecheck_statement(
    project: &mut Project,
    session: &Session,
    typechecker_errors: &mut HashMap<ModuleId, Vec<RigError>>,
    module_id: ModuleId,
    scope_id: ScopeId,
    stmt: &Stmt,
) -> (Option<CheckedStmt>, Vec<(ModuleId, RigError)>) {
    match stmt {
        Stmt::UseStmt {
            path,
            visibility,
            span,
        } => import::check_use_stmt(
            project,
            session,
            typechecker_errors,
            module_id,
            scope_id,
            path,
            *visibility,
            span,
        ),
        _ => todo!(),
    }
}
