#![feature(panic_info_message)]
mod run;

use crate::run::run;
use clap::{Parser, Subcommand};
use std::panic;
use std::str::FromStr;

#[derive(Parser)]
#[clap(author, version, about, long_about = None)]
struct RigCLI {
    #[clap(subcommand)]
    subcommands: Subcommands,
}

#[derive(Subcommand)]
enum Subcommands {
    Run {
        /// File to execute
        file: String,

        /// Output generated unpretty stuff
        #[clap(short, long, name = "type")]
        unpretty: Option<OutputType>,

        /// Reconstruct source code from ast(debugging purpose)
        #[clap(short, long)]
        reconstruct_from_ast: bool,
    },
    Explain {
        /// Error code to explain
        error_code: String,
    },
}

#[derive(PartialEq)]
pub enum OutputType {
    Ast,
    LexicalTokens,
    Bytecode,
}

impl FromStr for OutputType {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "ast" => Ok(Self::Ast),
            "lexical-tokens" => Ok(Self::LexicalTokens),
            "bytecode" => Ok(Self::Bytecode),
            _ => Err(format!(
                "Unknown type '{}'. Expected one of 'ast', 'lexical-tokens', 'bytecode'.",
                s
            )),
        }
    }
}

fn main() {
    std::env::set_var("RUST_BACKTRACE", "full");
    panic::set_hook(Box::new(|pi| {
        eprintln!("Internal compiler error: Compiler panicked\n");
        eprintln!("Backtrace:\n{:?}", backtrace::Backtrace::new());

        if let Some(message) = pi.message() {
            eprintln!("Panic message:\n\n{}\n", message);
        }
        if let Some(location) = pi.location() {
            eprintln!("Location: {}", location);
        }
    }));

    let cli: RigCLI = RigCLI::parse();

    match cli.subcommands {
        Subcommands::Run {
            file,
            unpretty,
            reconstruct_from_ast,
        } => run(file, unpretty, reconstruct_from_ast),
        Subcommands::Explain { error_code: _ } => {}
    }
}
