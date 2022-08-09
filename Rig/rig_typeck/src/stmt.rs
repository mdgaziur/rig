use crate::import;

use rig_ast::stmt::Stmt;

use rig_error::{ErrorCode, ErrorType, RigError};

use rig_project::Project;
use rig_session::Session;

use rig_ast::expr::Expr;
use rig_ast::struct_field::StructField;
use rig_ast::visibility::Visibility;
use rig_span::Span;
use rig_types::checked_stmt::CheckedStmt;
use rig_types::{ModuleId, ScopeId, StructFieldType, StructType, Type, TypeId, TypeIdOrModuleId};
use rig_utils::bug;
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
        Stmt::StructStmt {
            visibility,
            name,
            fields,
            span,
        } => check_struct(
            project,
            session,
            typechecker_errors,
            module_id,
            scope_id,
            *visibility,
            name,
            fields,
            span,
        ),
        _ => todo!(),
    }
}

fn check_struct(
    project: &mut Project,
    _session: &Session,
    _typechecker_errors: &mut HashMap<ModuleId, Vec<RigError>>,
    module_id: ModuleId,
    scope_id: ScopeId,
    visibility: Visibility,
    name: &str,
    fields: &[StructField],
    span: &Span,
) -> (Option<CheckedStmt>, Vec<(ModuleId, RigError)>) {
    let mut errors = vec![];

    // first check if it's already declared in the module
    let module = project.get_module(module_id);
    if let Some((_, struct_id)) = module.get_scope(scope_id).find_struct(name) {
        // do not allow multiple definition of struct with same name in same module
        if struct_id.get_module_id() == module_id {
            let resolved_struct = project.resolve_struct(struct_id);

            errors.push((
                module_id,
                RigError::with_hint(
                    ErrorType::Hard,
                    ErrorCode::E0008,
                    "Redefinition of struct",
                    span.clone(),
                    "The other one is defined here",
                    resolved_struct.span.clone(),
                ),
            ));
        }
    }

    let mut typecked_fields = vec![];
    for field in fields {
        let ty_path;
        let ty_path_span;

        match &field.ty {
            Expr::PathExpr { path, span } => {
                ty_path = path.clone(); // FIXME: don't clone
                ty_path_span = span;
            }
            Expr::VariableExpr { name, span } => {
                ty_path = vec![name.clone()]; // FIXME: don't clone
                ty_path_span = span;
            }
            expr => bug!(expr, "Unexpected expression in type of struct field"),
        };

        let ty = match module.try_resolve(&project.modules, &ty_path) {
            Ok(ty) => ty,
            Err(error) => {
                errors.push((
                    module_id,
                    RigError::with_no_hint_and_notes(
                        ErrorType::Hard,
                        error.to_error_code(),
                        &error.to_string(),
                        ty_path_span.clone(),
                    ),
                ));

                return (None, errors);
            }
        };

        let ty = match ty {
            TypeIdOrModuleId::TypeId(ty, vis) => {
                if vis == Visibility::NotPub
                    && field.visibility == Visibility::Pub
                    && visibility == Visibility::Pub
                {
                    errors.push((
                        module_id,
                        RigError::with_no_hint_and_notes(
                            ErrorType::Hard,
                            ErrorCode::E0014,
                            "Cannot leak private type",
                            ty_path_span.clone(),
                        ),
                    ));

                    return (None, errors);
                } else {
                    ty
                }
            }
            TypeIdOrModuleId::ModuleId(_, _) => {
                errors.push((
                    module_id,
                    RigError::with_no_hint_and_notes(
                        ErrorType::Hard,
                        ErrorCode::E0014,
                        "Expected type, found module",
                        ty_path_span.clone(),
                    ),
                ));

                return (None, errors);
            }
        };

        typecked_fields.push(StructFieldType {
            name: field.name.clone(),
            ty,
            span: span.clone(),
        })
    }

    let mut location = module.location.clone();
    location.push(name.to_string());

    let struct_type = StructType {
        location,
        visibility,
        span: span.clone(),
        fields: typecked_fields,
        helpers: vec![],
        methods: vec![],
    };

    let module = project.get_module_mut(module_id);
    module.structs.push(struct_type);

    module.get_scope_mut(scope_id).insert_type(
        name,
        TypeId(
            scope_id,
            module.structs.len() - 1,
            visibility == Visibility::Pub,
            Type::Struct,
        ),
    );

    (None, errors)
}
