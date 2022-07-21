pub mod parsed_module;

use rig_types::{Module, ModuleId};

#[derive(Debug, Clone)]
pub struct Project {
    pub modules: Vec<Module>,
    pub entry_point: ModuleId,
}

impl Project {
    pub fn new() -> Self {
        Project {
            modules: vec![],
            entry_point: ModuleId(0),
        }
    }

    pub fn insert_module(&mut self, mut module: Module) -> ModuleId {
        module.id = ModuleId(self.modules.len());
        self.modules.push(module);

        ModuleId(self.modules.len() - 1)
    }

    pub fn get_module(&self, module_id: ModuleId) -> &Module {
        self.modules.get(module_id.0).unwrap()
    }

    pub fn get_module_mut(&mut self, module_id: ModuleId) -> &mut Module {
        self.modules.get_mut(module_id.0).unwrap()
    }

    pub fn get_entry_point(&self) -> &Module {
        self.modules.get(0).unwrap()
    }

    pub fn get_entry_point_mut(&mut self) -> &mut Module {
        self.modules.get_mut(0).unwrap()
    }

    pub fn has_module(&self, id: ModuleId) -> bool {
        self.modules.len() > id.0
    }
}
