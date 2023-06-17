use rig_intern::{intern, Interner, INTERNER};
use rig_lexer::Lexer;
use std::io::stdin;
use std::sync::RwLock;

fn main() {
    INTERNER.set(RwLock::new(Interner::new())).unwrap();

    loop {
        let mut line = String::new();
        stdin().read_line(&mut line).unwrap();

        let mut lexer = Lexer::new(line.chars(), intern!("<stdin>"));
        let mut tokens = vec![];
        while !lexer.is_eof() {
            tokens.push(lexer.lex_once());
            lexer.advance();
        }
        dbg!(tokens);
    }
}
