#![feature(panic_info_message)]
#![feature(panic_backtrace_config)]

use clap::Parser as ClapParser;
use parking_lot::lock_api::RwLock;
use rig_errors::display_compiler_error;
use rig_intern::{intern, Interner, INTERNER};
use rig_lexer::Lexer;
use rig_parser::Parser;
use rig_session::Session;
use std::backtrace::Backtrace;
use std::fs;
use std::panic::{set_backtrace_style, set_hook, BacktraceStyle};

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

    set_backtrace_style(BacktraceStyle::Full);
    set_hook(Box::new(|pi| {
        display_compiler_error("Fatal Internal compiler error!");

        if let Some(msg) = pi.message() {
            display_compiler_error(format!("Message: {msg}"));
        }

        if let Some(location) = pi.location() {
            display_compiler_error(format!("Location: {location}"));
        }

        if let Some(payload) = pi.payload().downcast_ref::<&str>() {
            display_compiler_error(format!("Payload: {payload}"));
        }

        display_compiler_error(format!("Backtrace:\n{}", Backtrace::force_capture()));
        display_compiler_error("Please submit a bug report at: https://github.com/mdgaziur/riglang")
    }));

    let args = Args::parse();

    let file_path = &args.source;
    let file_content = fs::read_to_string(file_path).unwrap();
    let mut session = Session::new();
    session.insert_file(file_path, file_content);
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

    let mut parser = Parser::new(&tokens);
    let ast = parser.parse();

    if args.debug {
        eprintln!(
            "\x1b[033m\x1b[1m[Start Debug]\n\
        File: {}:{}:{}\n\
        Generated AST from source file `{file_path}`:",
            file!(),
            line!(),
            column!()
        );
        eprintln!("{:#?}\n[End Debug]\x1b[0m", &ast);
    }

    for diag in parser.get_diags() {
        diag.display(&session);
    }
}
