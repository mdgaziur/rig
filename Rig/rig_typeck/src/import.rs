use crate::typeck_module;
use rig_ast::expr::Expr;
use rig_ast::visibility::Visibility;
use rig_error::{ErrorCode, ErrorType, RigError};
use rig_project::parsed_module::ParsedModule;
use rig_project::Project;
use rig_session::Session;
use rig_span::Span;
use rig_types::checked_stmt::CheckedStmt;
use rig_types::{Import, ImportedModuleId, Module, ModuleId, ScopeId, TypeIdOrModuleId};
use rig_utils::bug;
use std::collections::HashMap;

// TODO: handle modules inside subdirectories
pub fn check_use_stmt(
    project: &mut Project,
    session: &Session,
    typechecker_errors: &mut HashMap<ModuleId, Vec<RigError>>,
    current_module_id: ModuleId,
    scope_id: ScopeId,
    path: &Expr,
    visibility: Visibility,
    _span: &Span,
) -> (Option<CheckedStmt>, Vec<(ModuleId, RigError)>) {
    let mut errors = vec![];

    let (import_path, import_path_span) = match path {
        Expr::PathExpr { path, span } => (path, span),
        expr => bug!(
            expr,
            "Unexpected expression in use statement(expected path expression)"
        ),
    };

    let mut resolved_import = None;

    // Check if any module exists with same name
    for module in &project.modules {
        if import_path[0] == module.location.last().unwrap().as_str() {
            resolved_import = Some(Import::Module(
                ImportedModuleId(current_module_id, module.id),
                visibility,
            ));
        }
    }
    if let None = resolved_import {
        for search_path in &session.search_paths {
            if let Ok(dir) = search_path.read_dir() {
                for entry in dir {
                    if let Ok(entry) = entry {
                        let file_path = entry.path();

                        if file_path.is_file() {
                            if file_path.file_name().unwrap().to_str().unwrap()
                                == import_path[0].clone() + ".rig"
                            {
                                if let Ok(file_content) = std::fs::read_to_string(&file_path) {
                                    let parsed_module = ParsedModule::new(file_path, file_content);

                                    if parsed_module.has_lexer_errors() {
                                        parsed_module.print_parser_errors();
                                        std::process::exit(1);
                                    }

                                    parsed_module.print_parser_errors();
                                    let module_id = project.insert_module(Module::new(
                                        parsed_module.absolute_path.clone(),
                                        vec![parsed_module.module_name.clone()],
                                        parsed_module.file_content.clone(),
                                        parsed_module.ast.clone(),
                                    ));
                                    let new_scope = project.get_module_mut(module_id).new_scope();

                                    typeck_module(
                                        project,
                                        session,
                                        typechecker_errors,
                                        &parsed_module,
                                        module_id,
                                        new_scope,
                                    );

                                    resolved_import = Some(Import::Module(
                                        ImportedModuleId(current_module_id, module_id),
                                        visibility,
                                    ));
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if let Some(resolved_import) = resolved_import {
        let (imported_module_id, module_visibility) = match resolved_import {
            Import::Module(id, vis) => (id, vis),
            _ => unreachable!(),
        };

        if import_path.len() == 1 {
            let current_module = project.get_module_mut(current_module_id);

            current_module
                .imports
                .insert(import_path.last().unwrap().clone(), resolved_import);
            current_module
                .scopes
                .last_mut()
                .unwrap()
                .insert_import(module_visibility, imported_module_id);
        } else {
            let imported_module = project.get_module(imported_module_id.get_id());
            let resolved =
                imported_module.try_resolve(&project.modules, &import_path[1..import_path.len()]);

            if let Ok(resolved) = resolved {
                match resolved {
                    TypeIdOrModuleId::TypeId(id, vis) => {
                        if vis == Visibility::NotPub && id.get_module_id() != current_module_id {
                            errors.push((
                                current_module_id,
                                RigError::with_no_hint_and_notes(
                                    ErrorType::Hard,
                                    ErrorCode::E0010,
                                    "Cannot import private type",
                                    import_path_span.clone(),
                                ),
                            ));
                        } else {
                            let current_module = project.get_module_mut(current_module_id);

                            current_module.imports.insert(
                                import_path.last().unwrap().clone(),
                                Import::TypeId(id, vis),
                            );
                            current_module
                                .get_scope_mut(scope_id)
                                .insert_type(import_path.last().unwrap(), id);
                        }
                    }
                    TypeIdOrModuleId::ModuleId(id, vis) => {
                        if vis == Visibility::NotPub && id.get_importer() != current_module_id {
                            errors.push((
                                current_module_id,
                                RigError::with_no_hint_and_notes(
                                    ErrorType::Hard,
                                    ErrorCode::E0010,
                                    "Cannot import private module",
                                    import_path_span.clone(),
                                ),
                            ));
                        } else {
                            let current_module = project.get_module_mut(current_module_id);

                            current_module
                                .get_scope_mut(scope_id)
                                .insert_import(module_visibility, imported_module_id);
                        }
                    }
                }
            } else if let Err(e) = resolved {
                errors.push((
                    current_module_id,
                    RigError::with_no_hint_and_notes(
                        ErrorType::Hard,
                        e.to_error_code(),
                        &e.to_string(),
                        import_path_span.clone(),
                    ),
                ));
            }
        }
    } else {
        errors.push((
            current_module_id,
            RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0009,
                "Couldn't resolve import",
                import_path_span.clone(),
            ),
        ));
    }

    (None, errors)
}
