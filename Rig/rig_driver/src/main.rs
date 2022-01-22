mod run;

use crate::run::run;
use clap::{Parser, Subcommand};
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

        /// Output AST and exit
        #[clap(short, long, name = "type")]
        unpretty: Option<OutputType>,
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
    let cli: RigCLI = RigCLI::parse();

    match cli.subcommands {
        Subcommands::Run { file, unpretty } => run(file, unpretty),
        Subcommands::Explain { error_code: _ } => {}
    }
}
