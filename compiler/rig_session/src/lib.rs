pub mod module;

use crate::module::Module;
use rig_intern::{intern, InternedString};
use std::collections::HashMap;
use std::fs;
use owo_colors::OwoColorize;

#[derive(Debug, Clone)]
pub struct Session {
    modules: HashMap<InternedString, Module>,
    pub debug: bool,
    pub debug_pretty: bool,
}

impl Session {
    pub fn new(debug: bool, debug_pretty: bool) -> Self {
        Self {
            modules: HashMap::new(),
            debug,
            debug_pretty
        }
    }

    pub fn get_file_content(&self, file_path: InternedString) -> InternedString {
        self.modules[&file_path].file_content
    }

    pub fn create_module(&mut self, file_path: &str) -> Result<(), String> {
        let file_content = match fs::read_to_string(file_path) {
            Ok(file_content) => file_content,
            Err(e) => return Err(format!("Failed to open file at `{file_path}: {e}"))
        };

        self.modules.insert(intern!(file_path), Module::new(file_path, &file_content));

        Ok(())
    }

    pub fn get_module(&self, module_path: InternedString) -> Option<&Module> {
        self.modules.get(&module_path)
    }

    pub fn get_module_mut(&mut self, module_path: InternedString) -> Option<&mut Module> {
        self.modules.get_mut(&module_path)
    }
}
