use crate::OutputType;
use colored::Colorize;
use rig_lexer::Lexer;

pub fn run(file_name: String, unpretty: Option<OutputType>) {
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
            eprintln!("{}", err);
        }
        return;
    }
}
