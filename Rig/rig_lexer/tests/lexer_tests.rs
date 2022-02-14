use rig_ast::token::{Token, TokenType};
use rig_lexer::Lexer;
use rig_span::Span;

#[test]
fn test_single_char() {
    let mut lexer = Lexer::new(";", "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print(";");
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::Semicolon,
                lexeme: ";".to_string(),
                literal: ";".to_string(),
                span: Span::for_single_char("<test>", 1, 0)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 1, 1)
            }
        ]
    );
}

#[test]
fn test_plus_single_char() {
    let mut lexer = Lexer::new("+", "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print("+");
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_string(),
                literal: "+".to_string(),
                span: Span::for_single_char("<test>", 1, 0)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 1, 1)
            }
        ]
    );
}

#[test]
fn test_plus_equals_double_char() {
    let mut lexer = Lexer::new("+", "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print("+=");
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_string(),
                literal: "+".to_string(),
                span: Span::for_single_char("<test>", 1, 0),
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 1, 1)
            }
        ]
    );
}

#[test]
fn test_less_than_double_char() {
    let mut lexer = Lexer::new("<", "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print("<");
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::LessThan,
                lexeme: "<".to_string(),
                literal: "<".to_string(),
                span: Span::for_single_char("<test>", 1, 0)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 1, 1)
            }
        ]
    );
}

#[test]
fn test_less_than_equals_triple_char() {
    let mut lexer = Lexer::new("<=", "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print("<=");
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::LessThanOrEquals,
                lexeme: "<=".to_string(),
                literal: "<=".to_string(),
                span: Span::for_single_line("<test>", 1, 0, 1)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 1, 2)
            }
        ]
    );
}

#[test]
fn test_left_shift_triple_char() {
    let mut lexer = Lexer::new("<<", "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print("<<");
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::LeftShift,
                lexeme: "<<".to_string(),
                literal: "<<".to_string(),
                span: Span::for_single_line("<test>", 1, 0, 1)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 1, 2)
            }
        ]
    );
}

#[test]
fn test_single_line_string() {
    let file_content = "\"Hello World\"";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print(file_content);
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::StringLiteral,
                lexeme: "\"Hello World\"".to_string(),
                literal: "Hello World".to_string(),
                span: Span::for_single_line("<test>", 1, 0, 12)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 1, 13)
            }
        ]
    );
}

#[test]
fn test_double_line_string() {
    let file_content = "\"Hello\nWorld\"";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print(file_content);
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::StringLiteral,
                lexeme: "\"Hello\nWorld\"".to_string(),
                literal: "Hello\nWorld".to_string(),
                span: Span {
                    file_name: "<test>".to_string(),
                    starting_line: 1,
                    starting_line_offset: 0,
                    starting_line_end_offset: 5,
                    ending_line: 2,
                    ending_line_offset: 0,
                    ending_line_end_offset: 5,
                }
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 2, 6)
            }
        ]
    );
}

#[test]
fn test_multi_line_string() {
    let file_content = "\"Hello\nWorld\nfrom\nRig!\"";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print(file_content);
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::StringLiteral,
                lexeme: "\"Hello\nWorld\nfrom\nRig!\"".to_string(),
                literal: "Hello\nWorld\nfrom\nRig!".to_string(),
                span: Span {
                    file_name: "<test>".to_string(),
                    starting_line: 1,
                    starting_line_offset: 0,
                    starting_line_end_offset: 5,
                    ending_line: 4,
                    ending_line_offset: 0,
                    ending_line_end_offset: 4,
                }
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 4, 5)
            }
        ]
    );
}

#[test]
fn test_multi_line_escaped_string() {
    let file_content = "\"Hello \\\"Rig\\\"!\n\\tThis is behind a tab character\"";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print(file_content);
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::StringLiteral,
                lexeme: "\"Hello \\\"Rig\\\"!\n\\tThis is behind a tab character\"".to_string(),
                literal: "Hello \"Rig\"!\n\tThis is behind a tab character".to_string(),
                span: Span {
                    file_name: "<test>".to_string(),
                    starting_line: 1,
                    starting_line_offset: 0,
                    starting_line_end_offset: 14,
                    ending_line: 2,
                    ending_line_offset: 0,
                    ending_line_end_offset: 32,
                }
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 2, 33)
            }
        ]
    );
}

#[test]
fn test_multi_line_escaped_invalid_string() {
    let file_content = "\"Hello \\aRig\\\"!\n\\tThis is behind a tab character\"";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();

    if lexer_output.1.is_empty() {
        panic!("lexer didn't throw error");
    } else {
        for err in lexer_output.1 {
            err.print(file_content);
        }
    }
}

#[test]
fn test_multi_line_unterminated_string() {
    let file_content = "\"Hello\nWorld\nfrom\nRig!";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();

    if lexer_output.1.is_empty() {
        panic!("lexer didn't throw error");
    } else {
        for err in lexer_output.1 {
            err.print(file_content);
        }
    }
}

#[test]
fn test_string_unterminated_string() {
    let file_content = "\"Hello World!";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();

    if lexer_output.1.is_empty() {
        panic!("lexer didn't throw error");
    } else {
        for err in lexer_output.1 {
            err.print(file_content);
        }
    }
}

#[test]
fn test_ident() {
    let file_content = "ident1\nident2 ident3\nident4\nident5!use";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print(file_content);
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("ident1"),
                lexeme: String::from("ident1"),
                span: Span::for_single_line("<test>", 1, 0, 5)
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("ident2"),
                lexeme: String::from("ident2"),
                span: Span::for_single_line("<test>", 2, 0, 5)
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("ident3"),
                lexeme: String::from("ident3"),
                span: Span::for_single_line("<test>", 2, 7, 12)
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("ident4"),
                lexeme: String::from("ident4"),
                span: Span::for_single_line("<test>", 3, 0, 5)
            },
            Token {
                token_type: TokenType::Identifier,
                literal: String::from("ident5"),
                lexeme: String::from("ident5"),
                span: Span::for_single_line("<test>", 4, 0, 5)
            },
            Token {
                token_type: TokenType::Bang,
                literal: String::from("!"),
                lexeme: String::from("!"),
                span: Span::for_single_line("<test>", 4, 6, 6)
            },
            Token {
                token_type: TokenType::Keyword,
                literal: String::from("use"),
                lexeme: String::from("use"),
                span: Span::for_single_line("<test>", 4, 7, 9)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 4, 10)
            }
        ]
    );
}

#[test]
fn test_number_literal() {
    let file_content = "1234\n123.4 1234\n123. 1234!";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();
    let tokens = lexer_output.0;

    if !lexer_output.1.is_empty() {
        for err in lexer_output.1 {
            err.print(file_content);
        }
        panic!("unexpected error occurred in the lexer");
    }

    assert_eq!(
        tokens,
        [
            Token {
                token_type: TokenType::NumberLiteral,
                literal: String::from("1234"),
                lexeme: String::from("1234"),
                span: Span::for_single_line("<test>", 1, 0, 3)
            },
            Token {
                token_type: TokenType::NumberLiteral,
                literal: String::from("123.4"),
                lexeme: String::from("123.4"),
                span: Span::for_single_line("<test>", 2, 0, 4)
            },
            Token {
                token_type: TokenType::NumberLiteral,
                literal: String::from("1234"),
                lexeme: String::from("1234"),
                span: Span::for_single_line("<test>", 2, 6, 9)
            },
            Token {
                token_type: TokenType::NumberLiteral,
                literal: String::from("123."),
                lexeme: String::from("123."),
                span: Span::for_single_line("<test>", 3, 0, 3)
            },
            Token {
                token_type: TokenType::NumberLiteral,
                literal: String::from("1234"),
                lexeme: String::from("1234"),
                span: Span::for_single_line("<test>", 3, 5, 8)
            },
            Token {
                token_type: TokenType::Bang,
                literal: String::from("!"),
                lexeme: String::from("!"),
                span: Span::for_single_line("<test>", 3, 9, 9)
            },
            Token {
                token_type: TokenType::EOF,
                lexeme: String::new(),
                literal: String::new(),
                span: Span::for_single_char("<test>", 3, 10)
            }
        ]
    );
}

#[test]
fn test_invalid_number() {
    let file_content = "123..456";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();

    if lexer_output.1.is_empty() {
        panic!("lexer didn't throw error");
    } else {
        for err in lexer_output.1 {
            err.print(file_content);
        }
    }
}

#[test]
fn test_unknown_char() {
    let file_content = "@use lexer::Lexer;";
    let mut lexer = Lexer::new(file_content, "<test>");
    let lexer_output = lexer.lex();

    if lexer_output.1.is_empty() {
        panic!("lexer didn't throw error");
    } else {
        for err in lexer_output.1 {
            err.print(file_content);
        }
    }
}
