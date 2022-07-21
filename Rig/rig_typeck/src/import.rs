use crate::typeck_module;
use rig_ast::expr::Expr;
use rig_ast::visibility::Visibility;
use rig_error::{ErrorCode, ErrorType, RigError};
use rig_project::parsed_module::ParsedModule;
use rig_project::Project;
use rig_session::Session;
use rig_span::Span;
use rig_types::checked_stmt::CheckedStmt;
use rig_types::{Import, Module, ModuleId, ScopeId, TypeIdOrModuleId};
use rig_utils::bug;
use std::collections::HashMap;

// TODO: handle modules inside subdirectories
pub fn check_use_stmt(
    project: &mut Project,
    session: &Session,
    typechecker_errors: &mut HashMap<ModuleId, Vec<RigError>>,
    module_id: ModuleId,
    scope_id: ScopeId,
    path: &Expr,
    visibility: Visibility,
    _span: &Span,
) -> (Option<CheckedStmt>, Vec<(ModuleId, RigError)>) {
    let mut errs = Vec::new();
    let (path, span) = match path {
        Expr::PathExpr { path, span } => (path, span),
        expr => bug!(expr, "Unexpected expression in `use` statement"),
    };

    let mut resolved_module = None;
    let mut resolved_typeid = None;
    // check if any module exists with same name in project
    for module in &project.modules {
        if path[0] == module.location.last().unwrap().as_str() {
            resolved_module = Some(module.id);
        }
    }
    if let Some(module_id) = resolved_module {
        if path.len() == 1 {
            let module = project.get_module_mut(module_id);

            module.imports.insert(
                path.last().unwrap().clone(),
                Import::Module(module_id, visibility),
            );
        }
    } else {
        // search for module in search path
        for search_path in &session.search_paths {
            if let Ok(dir) = search_path.read_dir() {
                for entry in dir {
                    if let Ok(entry) = entry {
                        let file_path = entry.path();

                        if file_path.is_file() {
                            if file_path.file_name().unwrap().to_str().unwrap()
                                == path[0].clone() + ".rig"
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

                                    typeck_module(
                                        project,
                                        session,
                                        typechecker_errors,
                                        &parsed_module,
                                        module_id,
                                        scope_id,
                                    );

                                    resolved_module = Some(module_id);
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    if let Some(resolved_module) = resolved_module {
        if path.len() > 1 {
            let module = project.get_module(resolved_module);
            let resolved_ty = module.try_import(&project.modules, &path[1..path.len()]);

            if let Ok(type_id) = resolved_ty {
                resolved_typeid = Some(type_id);
            } else if let Err(error) = resolved_ty {
                errs.push((
                    module_id,
                    RigError::with_no_hint_and_notes(
                        ErrorType::Hard,
                        error.to_error_code(),
                        &error.to_string(),
                        span.clone(),
                    ),
                ));
            }
        } else {
            let module = project.get_module_mut(module_id);
            module.imports.insert(
                path.last().unwrap().clone(),
                Import::Module(resolved_module, visibility),
            );
        }
    } else {
        errs.push((
            module_id,
            RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0009,
                "Failed to import module",
                span.clone(),
            ),
        ));
    }

    if let Some(type_id) = resolved_typeid {
        let module = project.get_module_mut(module_id);
        module.imports.insert(
            path.last().unwrap().clone(),
            match type_id {
                TypeIdOrModuleId::TypeId(id, Visibility::Pub) => Import::TypeId(id, visibility),
                TypeIdOrModuleId::ModuleId(id, Visibility::Pub) => Import::Module(id, visibility),
                t => bug!(t, "Resolver should return nothing if module is private"),
            },
        );
    }

    (None, errs)
}
