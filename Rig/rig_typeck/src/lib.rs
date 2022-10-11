mod expr;
mod import;
mod stmt;

use crate::stmt::typecheck_statement;

use rig_error::RigError;
use rig_project::parsed_module::ParsedModule;
use rig_project::Project;
use rig_session::Session;

use rig_types::{Module, ModuleId, ScopeId};
use std::collections::HashMap;

#[derive(Debug)]
pub struct TypeChecker<'tcx> {
    /// Entry point module
    pub entry_point: ParsedModule,

    /// Project
    pub project: Project,

    /// Errors while parsing modules
    pub typechecking_errors: HashMap<ModuleId, Vec<RigError>>,

    /// Session info
    pub session: &'tcx Session,
}

impl<'tcx> TypeChecker<'tcx> {
    pub fn new(parsed_module: ParsedModule, session: &'tcx Session) -> Self {
        // FIXME(mdgaziur): do something other than cloning
        let module = Module::new(
            parsed_module.absolute_path.clone(),
            vec![parsed_module.module_name.clone()],
            parsed_module.file_content.clone(),
            parsed_module.ast.clone(),
        );

        let project = Project {
            modules: vec![module],
            entry_point: ModuleId(0), // entry points are always supposed to be the first module
        };

        Self {
            entry_point: parsed_module,
            project,
            typechecking_errors: HashMap::new(),
            session,
        }
    }

    pub fn do_typechecking(&mut self) {
        let scope_id = self.project.get_entry_point_mut().new_scope();

        typeck_module(
            &mut self.project,
            &self.session,
            &mut self.typechecking_errors,
            &self.entry_point,
            ModuleId(0),
            scope_id,
        );
    }

    pub fn print_errors(&self) {
        for (module_id, errors) in &self.typechecking_errors {
            let module = self.project.get_module(*module_id);

            for error in errors {
                error.print(&module.source_code);
            }
        }
    }
}

fn typeck_module(
    project: &mut Project,
    session: &Session,
    typechecker_errors: &mut HashMap<ModuleId, Vec<RigError>>,
    parsed_module: &ParsedModule,
    id: ModuleId,
    scope_id: ScopeId,
) {
    let module_id;
    if !project.has_module(ModuleId(id.0)) {
        // FIXME(mdgaziur): do something other than cloning

        module_id = project.insert_module(Module::new(
            parsed_module.absolute_path.clone(),
            vec![parsed_module.module_name.clone()],
            parsed_module.file_content.clone(),
            parsed_module.ast.clone(),
        ));
    } else {
        module_id = ModuleId(id.0);
    }

    for node in &parsed_module.ast {
        let (_, errors) = typecheck_statement(
            project,
            session,
            typechecker_errors,
            module_id,
            scope_id,
            node,
        );

        extend_errors(typechecker_errors, errors);
    }
}

fn extend_errors(
    typechecker_errors: &mut HashMap<ModuleId, Vec<RigError>>,
    new_errors: Vec<(ModuleId, RigError)>,
) {
    for (module_id, error) in new_errors {
        if let Some(mod_errors) = typechecker_errors.get_mut(&module_id) {
            mod_errors.push(error);
        } else {
            typechecker_errors.insert(module_id, vec![error]);
        }
    }
}
