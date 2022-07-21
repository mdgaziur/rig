use crate::OutputType;
use colored::Colorize;
use std::env::current_dir;

use rig_project::parsed_module::ParsedModule;
use rig_session::{DebugInfo, Session};
use rig_typeck::TypeChecker;
use std::path::PathBuf;

pub fn run(file_name: String, _unpretty: Option<OutputType>, _reconstruct_from_ast: bool) {
    let file_content = match std::fs::read_to_string(&file_name) {
        Ok(f) => f,
        Err(e) => {
            eprintln!(
                "{}",
                format!("Failed to open file \"{}\": {}", file_name, e).red()
            );
            return;
        }
    };

    let parsed_module = ParsedModule::new(
        PathBuf::from(file_name).canonicalize().unwrap(),
        file_content,
    );

    if parsed_module.has_lexer_errors() {
        parsed_module.print_lexer_errors();
        std::process::exit(1);
    }

    parsed_module.print_parser_errors();

    let session = Session {
        search_paths: vec![current_dir().unwrap()],
        debug: DebugInfo::None,
    };
    let mut type_checker = TypeChecker::new(parsed_module, &session);
    type_checker.do_typechecking();
    type_checker.print_errors();
}
