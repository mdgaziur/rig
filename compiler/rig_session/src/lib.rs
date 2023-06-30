use rig_intern::{InternedString, INTERNER};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Session {
    processed_files: HashMap<InternedString, InternedString>,
}

impl Session {
    pub fn new() -> Self {
        Self {
            processed_files: HashMap::new(),
        }
    }

    pub fn get_file_content(&self, file_path: InternedString) -> InternedString {
        self.processed_files[&file_path]
    }

    pub fn insert_file(
        &mut self,
        file_path: impl ToString,
        file_content: impl ToString,
    ) -> InternedString {
        let mut interner = INTERNER.get().unwrap().write();

        let file_name_interned = interner.intern(file_path.to_string());
        let file_content_interned = interner.intern(file_content.to_string());
        self.processed_files
            .insert(file_name_interned, file_content_interned);

        file_content_interned
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}
