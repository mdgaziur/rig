use parking_lot::lock_api::RwLock;

use rig_intern::{intern, Interner, INTERNER};

use rig_lexer::Lexer;
use rig_session::Session;

use std::env::args;
use std::fs;

fn main() {
    INTERNER.init_once(|| RwLock::new(Interner::new()));

    let file_path = args().nth(1).unwrap();
    let file_content = fs::read_to_string(&file_path).unwrap().replace("\t", "   ");
    let mut session = Session::new();
    let mut lexer = Lexer::new(file_content.chars(), intern!(&file_path));
    let mut lexer_results = vec![];
    while !lexer.is_eof() {
        lexer_results.push(lexer.lex_once());
        lexer.advance();
    }

    session.insert_file(&file_path, &file_content);
    for lexer_result in lexer_results {
        if let Ok(tok) = lexer_result {
            dbg!(tok);
        }
        if let Err(err) = lexer_result {
            err.display(&session);
        }
    }
}
