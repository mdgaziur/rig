#[macro_use]
mod macros;
mod escape;

use crate::escape::escape;
use rig_ast::token::{Token, TokenType, KEYWORDS};
use rig_error::{ErrorCode, ErrorType, RigError};
use rig_span::Span;

pub struct Lexer<'l> {
    file_contents: &'l str,
    file_path: &'l str,
    line: usize,
    offset: usize,
    pos: usize,
}

impl<'l> Lexer<'l> {
    pub fn new(file_contents: &'l str, file_path: &'l str) -> Self {
        Lexer {
            file_contents,
            file_path,
            pos: 0,
            line: 1,
            offset: 0,
        }
    }

    pub fn lex(&mut self) -> (Vec<Token>, Vec<RigError>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        while !self.eof() {
            match self.peek().unwrap() {
                // single character tokens
                '(' => tokens.push(single_char_token!(self, '(', TokenType::LeftParen)),
                ')' => tokens.push(single_char_token!(self, ')', TokenType::RightParen)),
                '{' => tokens.push(single_char_token!(self, '{', TokenType::LeftBrace)),
                '}' => tokens.push(single_char_token!(self, '}', TokenType::RightBrace)),
                '[' => tokens.push(single_char_token!(self, '}', TokenType::LeftThirdBracket)),
                ']' => tokens.push(single_char_token!(self, '}', TokenType::RightThirdBracket)),
                ',' => tokens.push(single_char_token!(self, ',', TokenType::Comma)),
                ';' => tokens.push(single_char_token!(self, ';', TokenType::Semicolon)),
                '.' => tokens.push(single_char_token!(self, ';', TokenType::Dot)),

                // double or single character tokens
                ':' => {
                    double_char_token!(
                        self,
                        tokens,
                        ':',
                        "::",
                        ':',
                        TokenType::Scope,
                        TokenType::Colon
                    )
                }
                '!' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "!=",
                        '!',
                        TokenType::NotEqual,
                        TokenType::Bang
                    )
                }
                '+' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "+=",
                        '+',
                        TokenType::PlusEquals,
                        TokenType::Plus
                    )
                }
                '-' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "+=",
                        '>',
                        "->",
                        '+',
                        TokenType::MinusEquals,
                        TokenType::Arrow,
                        TokenType::Minus
                    )
                }
                '*' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "*=",
                        '*',
                        TokenType::MultiplyEquals,
                        TokenType::Multiply
                    )
                }
                '/' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "/=",
                        '/',
                        TokenType::DivideEquals,
                        TokenType::Divide
                    )
                }
                '#' => {
                    while let Some(ch) = self.peek() {
                        if ch == '\n' {
                            break;
                        }
                        self.advance();
                        self.advance();
                    }
                }
                '%' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "%=",
                        '%',
                        TokenType::ModulusEquals,
                        TokenType::Modulus
                    )
                }
                '&' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "&=",
                        '&',
                        "&&",
                        '&',
                        TokenType::AndOpEquals,
                        TokenType::And,
                        TokenType::AndOp
                    )
                }
                '|' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "|=",
                        '|',
                        "||",
                        '|',
                        TokenType::OrOpEquals,
                        TokenType::Or,
                        TokenType::OrOp
                    )
                }
                '=' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "==",
                        '=',
                        TokenType::EqualEqual,
                        TokenType::Equal
                    )
                }
                '<' => {
                    triple_char_token!(
                        self,
                        tokens,
                        '<',
                        '=',
                        '=',
                        '<',
                        "<<",
                        "<=",
                        "<<=",
                        TokenType::LessThan,
                        TokenType::LeftShift,
                        TokenType::LessThanOrEquals,
                        TokenType::LeftShiftEquals
                    );
                }
                '>' => {
                    triple_char_token!(
                        self,
                        tokens,
                        '>',
                        '=',
                        '=',
                        '>',
                        ">>",
                        ">=",
                        ">>=",
                        TokenType::GreaterThan,
                        TokenType::RightShift,
                        TokenType::GreaterThanOrEquals,
                        TokenType::RightShiftEquals
                    );
                }
                '^' => {
                    double_char_token!(
                        self,
                        tokens,
                        '=',
                        "^=",
                        '^',
                        TokenType::XorEquals,
                        TokenType::Xor
                    )
                }
                '"' => {
                    let mut lexeme = String::new();
                    lexeme.push(self.peek().unwrap());
                    let mut literal = String::new();
                    let starting_line = self.line;
                    let starting_line_offset = self.offset;
                    let mut starting_line_end_offset = self.offset;
                    let mut terminated = false;
                    let mut invalid = false;
                    self.advance();

                    while !self.eof() && self.peek() != Some('\n') {
                        if self.peek() == Some('"') {
                            lexeme.push(self.peek().unwrap());
                            terminated = true;
                            break;
                        } else if self.peek() == Some('\\') {
                            // do escaping stuff
                            lexeme.push(self.peek().unwrap());
                            let escape_char_offset = self.offset;
                            let escape_char_line = self.line;
                            self.advance();
                            if let Some(ch) = self.peek() {
                                lexeme.push(self.peek().unwrap());
                                if let Ok(escaped) = escape(ch) {
                                    literal.push(escaped);
                                    starting_line_end_offset += 2;
                                    self.advance();
                                } else if self.peek() == Some('\n') {
                                    errors.push(RigError {
                                        message: String::from("invalid escape character"),
                                        error_type: ErrorType::Hard,
                                        file_path: self.file_path.to_string(),
                                        hint: None,
                                        error_code: ErrorCode::E0004,
                                        span: Span::for_single_line(
                                            self.file_path,
                                            escape_char_line,
                                            escape_char_offset,
                                            escape_char_offset,
                                        ),
                                    });
                                    invalid = true;
                                    break;
                                } else {
                                    errors.push(RigError {
                                        message: String::from("invalid escape character"),
                                        error_type: ErrorType::Hard,
                                        file_path: self.file_path.to_string(),
                                        hint: None,
                                        error_code: ErrorCode::E0004,
                                        span: Span::for_single_line(
                                            self.file_path,
                                            escape_char_line,
                                            escape_char_offset,
                                            self.offset,
                                        ),
                                    });
                                    invalid = true;
                                    break;
                                }
                            } else {
                                errors.push(RigError {
                                    message: String::from("unexpected eof"),
                                    error_type: ErrorType::Hard,
                                    file_path: self.file_path.to_string(),
                                    hint: None,
                                    error_code: ErrorCode::E0004,
                                    span: Span::for_single_char(
                                        self.file_path,
                                        self.line,
                                        self.offset,
                                    ),
                                });
                                invalid = true;
                                break;
                            }
                        } else {
                            lexeme.push(self.peek().unwrap());
                            literal.push(self.peek().unwrap());
                            starting_line_end_offset += 1;
                            self.advance();
                        }
                    }

                    if invalid {
                        continue;
                    }

                    if self.eof() {
                        errors.push(RigError {
                            message: String::from("unterminated string literal"),
                            error_type: ErrorType::Hard,
                            file_path: self.file_path.to_string(),
                            hint: Some(String::from("insert '\"' here")),
                            error_code: ErrorCode::E0002,
                            span: Span::for_single_line(
                                self.file_path,
                                starting_line,
                                starting_line_offset,
                                starting_line_end_offset,
                            ),
                        });
                        continue;
                    } else if terminated {
                        tokens.push(Token {
                            token_type: TokenType::StringLiteral,
                            lexeme,
                            literal,
                            span: Span::for_single_line(
                                self.file_path,
                                starting_line,
                                starting_line_offset,
                                starting_line_end_offset + 1,
                            ),
                        });
                        self.advance();
                        continue;
                    }

                    if self.peek() == Some('\n') {
                        literal.push(self.peek().unwrap());
                        lexeme.push(self.peek().unwrap());
                        self.advance();
                    }
                    let mut ending_line = self.line;
                    let mut ending_line_offset = self.offset;
                    let mut ending_line_end_offset = self.offset;
                    while !self.eof() && self.peek() != Some('"') {
                        if self.peek() == Some('\n') {
                            lexeme.push(self.peek().unwrap());
                            literal.push(self.peek().unwrap());
                            self.advance();
                            if !self.eof() {
                                ending_line_offset = self.offset;
                                ending_line = self.line;
                                ending_line_end_offset = self.offset;
                            }
                        } else if self.peek() == Some('"') {
                            lexeme.push(self.peek().unwrap());
                            terminated = true;
                            break;
                        } else if self.peek() == Some('\\') {
                            lexeme.push(self.peek().unwrap());
                            let escape_char_offset = self.offset;
                            let escape_char_line = self.line;
                            // do escaping stuff
                            self.advance();
                            if let Some(ch) = self.peek() {
                                lexeme.push(self.peek().unwrap());
                                if let Ok(escaped) = escape(ch) {
                                    literal.push(escaped);
                                    ending_line_end_offset += 2;
                                    self.advance();
                                } else if self.peek() == Some('\n') {
                                    errors.push(RigError {
                                        message: String::from("invalid escape character"),
                                        error_type: ErrorType::Hard,
                                        file_path: self.file_path.to_string(),
                                        hint: None,
                                        error_code: ErrorCode::E0004,
                                        span: Span::for_single_line(
                                            self.file_path,
                                            escape_char_line,
                                            escape_char_offset,
                                            escape_char_offset,
                                        ),
                                    });
                                    invalid = true;
                                    break;
                                } else {
                                    errors.push(RigError {
                                        message: String::from("invalid escape character"),
                                        error_type: ErrorType::Hard,
                                        file_path: self.file_path.to_string(),
                                        hint: None,
                                        error_code: ErrorCode::E0004,
                                        span: Span::for_single_line(
                                            self.file_path,
                                            escape_char_line,
                                            escape_char_offset,
                                            self.offset,
                                        ),
                                    });
                                    invalid = true;
                                }
                            } else {
                                errors.push(RigError {
                                    message: String::from("unexpected eof"),
                                    error_type: ErrorType::Hard,
                                    file_path: self.file_path.to_string(),
                                    hint: None,
                                    error_code: ErrorCode::E0005,
                                    span: Span::for_single_char(
                                        self.file_path,
                                        self.line,
                                        self.offset,
                                    ),
                                });
                                invalid = true;
                                break;
                            }
                        } else {
                            lexeme.push(self.peek().unwrap());
                            literal.push(self.peek().unwrap());
                            ending_line_end_offset += 1;
                            self.advance();
                        }
                    }

                    if invalid {
                        continue;
                    }

                    let mut span = Span {
                        file_name: self.file_path.to_string(),
                        starting_line,
                        starting_line_offset,
                        starting_line_end_offset,
                        ending_line,
                        ending_line_offset,
                        ending_line_end_offset,
                    };

                    if self.eof() {
                        span.ending_line_end_offset -= 1;
                        errors.push(RigError {
                            message: String::from("unterminated string literal"),
                            error_type: ErrorType::Hard,
                            file_path: self.file_path.to_string(),
                            hint: Some(String::from("insert '\"' here")),
                            error_code: ErrorCode::E0002,
                            span,
                        });
                        continue;
                    } else {
                        lexeme.push(self.peek().unwrap());
                        tokens.push(Token {
                            token_type: TokenType::StringLiteral,
                            lexeme,
                            literal,
                            span,
                        });
                    }
                }

                ch if ch.is_alphabetic() || ch == '_' => {
                    let mut ident = String::new();
                    let line = self.line;
                    let starting_position = self.offset;
                    let mut ending_position = self.offset;

                    while !self.eof() {
                        ident.push(self.peek().unwrap());
                        ending_position = self.offset;

                        if let Some(ch) = self.peek_next() {
                            if !ch.is_alphanumeric() && ch != '_' {
                                break;
                            }
                        }

                        self.advance();
                    }

                    tokens.push(Token {
                        token_type: if KEYWORDS.contains(&ident.as_str()) {
                            TokenType::Keyword
                        } else {
                            TokenType::Identifier
                        },
                        literal: ident.clone(),
                        lexeme: ident.clone(),
                        span: Span::for_single_line(
                            self.file_path,
                            line,
                            starting_position,
                            ending_position,
                        ),
                    });

                    if self.eof() {
                        break;
                    }
                }

                ch if ch.is_ascii_digit() => {
                    let mut num = String::new();
                    let line = self.line;
                    let starting_position = self.offset;
                    let mut ending_position = self.offset;
                    let mut dot_count = 0;

                    let mut invalid_num = false;
                    while !self.eof() {
                        let ch = self.peek().unwrap();
                        if ch == '.' {
                            dot_count += 1;
                        }
                        if dot_count > 1 {
                            errors.push(RigError {
                                message: String::from(
                                    "invalid integer literal: too many dots in one literal",
                                ),
                                error_type: ErrorType::Hard,
                                file_path: self.file_path.to_string(),
                                hint: Some(String::from("remove this dot")),
                                error_code: ErrorCode::E0003,
                                span: Span::for_single_line(
                                    self.file_path,
                                    line,
                                    starting_position,
                                    ending_position,
                                ),
                            });
                            self.advance();
                            invalid_num = true;
                            break;
                        }

                        num.push(ch);
                        ending_position = self.offset;

                        if let Some(ch) = self.peek_next() {
                            if !ch.is_ascii_digit() && ch != '.' {
                                break;
                            }
                        }

                        self.advance();
                    }
                    if invalid_num {
                        continue;
                    }

                    tokens.push(Token {
                        token_type: TokenType::NumberLiteral,
                        literal: num.clone(),
                        lexeme: num.clone(),
                        span: Span::for_single_line(
                            self.file_path,
                            line,
                            starting_position,
                            ending_position,
                        ),
                    });
                }

                // whitespace characters
                ch if ch.is_whitespace() => {}

                // unknown character
                _ => errors.push(RigError {
                    message: String::from("unknown character"),
                    error_type: ErrorType::Hard,
                    file_path: self.file_path.to_string(),
                    hint: None,
                    error_code: ErrorCode::E0001,
                    span: Span::for_single_char(self.file_path, self.line, self.offset),
                }),
            }
            self.advance();
        }

        tokens.push(Token {
            token_type: TokenType::EOF,
            lexeme: String::new(),
            literal: String::new(),
            span: Span::for_single_char(self.file_path, self.line, self.offset),
        });

        (tokens, errors)
    }

    fn peek_next(&self) -> Option<char> {
        self.file_contents.chars().nth(self.pos + 1)
    }

    fn peek(&self) -> Option<char> {
        self.file_contents.chars().nth(self.pos)
    }

    fn eof(&self) -> bool {
        self.pos >= self.file_contents.len()
    }

    fn advance(&mut self) {
        if self.peek() != Some('\n') {
            self.offset += 1;
        }
        self.pos += 1;

        if !self.eof() && self.peek() == Some('\n') {
            self.line += 1;
            self.offset = 0;
        }
    }
}
