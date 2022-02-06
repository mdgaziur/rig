use crate::OutputType;
use colored::Colorize;
use rig_lexer::Lexer;
use rig_parser::{parse, Parser};

pub fn run(file_name: String, unpretty: Option<OutputType>, reconstruct_from_ast: bool) {
    let file = match std::fs::read_to_string(&file_name) {
        Ok(f) => f,
        Err(e) => {
            eprintln!(
                "{}",
                format!("Failed to open file \"{}\": {}", file_name, e).red()
            );
            return;
        }
    };

    let mut lexer = Lexer::new(&file, &file_name);
    let tokens = lexer.lex();

    if unpretty == Some(OutputType::LexicalTokens) {
        println!("{:#?}", tokens.0);
    }

    if !tokens.1.is_empty() {
        for err in tokens.1 {
            err.print(&file);
        }
        return;
    }

    let mut parser = Parser::new(&file_name, &tokens.0);
    let ast = parse(&mut parser, &file);

    if unpretty == Some(OutputType::Ast) {
        println!("{:#?}", ast.0);
    }

    if reconstruct_from_ast {
        for node in ast.0 {
            println!("{}", node.to_string(0));
        }
    }
}
