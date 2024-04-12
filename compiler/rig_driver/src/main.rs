use clap::Parser as ClapParser;
use parking_lot::lock_api::RwLock;
use rig_intern::{intern, Interner, INTERNER};
use rig_lexer::Lexer;
use rig_session::Session;
use rig_parser::Parser;
use std::fs;

#[derive(ClapParser)]
struct Args {
    /// Path to source file
    source: String,

    #[arg(long)]
    /// Enables debug outputs
    debug: bool,
}

fn main() {
    INTERNER.init_once(|| RwLock::new(Interner::new()));

    let args = Args::parse();

    let file_path = &args.source;
    let file_content = fs::read_to_string(&file_path).unwrap();
    let mut session = Session::new();
    session.insert_file(&file_path, file_content);
    let file_content_temp = session.get_file_content(intern!(file_path)).get();

    let mut lexer = Lexer::new(file_content_temp.chars(), intern!(file_path));
    let lexer_results = lexer.lex();
    let mut tokens = vec![];
    for lexer_result in lexer_results {
        if let Ok(tok) = lexer_result {
            tokens.push(tok);
        }
        if let Err(err) = lexer_result {
            err.display(&session);
        }
    }

    let mut parser = Parser::new(intern!(file_path), &tokens);
    let ast = parser.parse();
    for diag in parser.get_diags() {
        diag.display(&session);
    }

    if args.debug {
        eprintln!("\x1b[033m\x1b[1m[Start Debug]\n\
        File: {}:{}:{}\n\
        Generated AST from source file `{file_path}`:", file!(), line!(), column!());
        eprintln!("{:#?}\n[End Debug]\x1b[0m", ast);
    }
}
