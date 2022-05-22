#[macro_use]
mod macros;
mod escape;

use crate::escape::escape;
use rig_ast::token::{Token, TokenType, KEYWORDS};
use rig_error::{ErrorCode, ErrorType, RigError};
use rig_span::Span;

use std::str::Chars;

pub struct Lexer<'l> {
    chars: Chars<'l>,
    file_path: &'l str,
    line: usize,
    offset: usize,
    current: char,
    pos: usize,
    src_len: usize,
}

impl<'l> Lexer<'l> {
    pub fn new(file_contents: &'l str, file_path: &'l str) -> Self {
        let mut chars = file_contents.chars();

        Lexer {
            file_path,
            pos: 0,
            line: 1,
            offset: 0,
            current: chars.next().unwrap_or_default(),
            chars,
            src_len: file_contents.len(),
        }
    }

    pub fn lex(&mut self) -> (Vec<Token>, Vec<RigError>) {
        let mut tokens = Vec::new();
        let mut errors = Vec::new();

        'mainloop: while !self.eof() {
            match self.peek() {
                // single character tokens
                '(' => tokens.push(single_char_token!(self, '(', TokenType::LeftParen)),
                ')' => tokens.push(single_char_token!(self, ')', TokenType::RightParen)),
                '{' => tokens.push(single_char_token!(self, '{', TokenType::LeftBrace)),
                '}' => tokens.push(single_char_token!(self, '}', TokenType::RightBrace)),
                '[' => tokens.push(single_char_token!(self, '[', TokenType::LeftThirdBracket)),
                ']' => tokens.push(single_char_token!(self, ']', TokenType::RightThirdBracket)),
                ',' => tokens.push(single_char_token!(self, ',', TokenType::Comma)),
                ';' => tokens.push(single_char_token!(self, ';', TokenType::Semicolon)),
                '.' => tokens.push(single_char_token!(self, '.', TokenType::Dot)),

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
                    while !self.eof() {
                        let ch = self.peek();

                        if ch == '\n' {
                            break;
                        }
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
                        '>',
                        "=>",
                        '=',
                        TokenType::EqualEqual,
                        TokenType::FatArrow,
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
                    let mut lexeme = String::from("\"");
                    let mut literal = String::new();
                    let starting_line = self.line;
                    let starting_pos = self.offset;
                    let mut terminated = false;

                    self.advance();
                    while !self.eof() {
                        if self.peek() == '"' {
                            terminated = true;
                            break;
                        }

                        if self.peek() == '\\' {
                            lexeme.push(self.peek());
                            self.advance();

                            if !self.eof() {
                                let ch = self.peek();

                                if let Ok(e) = escape(ch) {
                                    lexeme.push(ch);
                                    literal.push(e);
                                } else {
                                    errors.push(RigError::with_no_hint_and_notes(
                                        ErrorType::Hard,
                                        ErrorCode::E0004,
                                        "Invalid escape character",
                                        Span {
                                            starting_line: self.line,
                                            starting_line_offset: self.offset - 1,
                                            ending_line: self.line,
                                            ending_line_end_offset: self.offset,
                                            file_name: self.file_path.to_string(),
                                        },
                                    ));
                                    continue 'mainloop;
                                }
                            } else {
                                errors.push(RigError::with_no_hint_and_notes(
                                    ErrorType::Hard,
                                    ErrorCode::E0005,
                                    "Unexpected eof",
                                    Span::for_single_char(self.file_path, self.line, self.offset),
                                ));
                                continue 'mainloop;
                            }
                        } else {
                            lexeme.push(self.peek());
                            literal.push(self.peek());
                        }

                        self.advance();
                    }

                    let mut sp = Span {
                        starting_line,
                        starting_line_offset: starting_pos,
                        ending_line: self.line,
                        ending_line_end_offset: self.offset,
                        file_name: self.file_path.to_string(),
                    };
                    if !terminated {
                        if self.eof() && sp.starting_line == sp.ending_line {
                            sp.ending_line_end_offset -= 1;
                        }
                        errors.push(RigError::with_hint(
                            ErrorType::Hard,
                            ErrorCode::E0002,
                            "Unterminated string literal",
                            sp,
                            "Insert `\"` here",
                            Span::for_single_char(self.file_path, self.line, self.offset),
                        ));
                        continue;
                    } else {
                        lexeme.push('"');
                    }

                    tokens.push(Token {
                        literal,
                        lexeme,
                        token_type: TokenType::StringLiteral,
                        span: sp,
                    })
                }

                ch if ch.is_alphabetic() || ch == '_' => {
                    let mut ident = String::new();
                    let line = self.line;
                    let starting_position = self.offset;
                    let mut ending_position = self.offset;

                    while !self.eof() {
                        ident.push(self.peek());
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
                    let mut last_dot_position = None;

                    while !self.eof() {
                        let ch = self.peek();
                        if ch == '.' {
                            dot_count += 1;
                            last_dot_position = Some(Span::for_single_char(
                                self.file_path,
                                self.line,
                                self.offset,
                            ));
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

                    if dot_count > 1 {
                        errors.push(RigError::with_hint(
                            ErrorType::Hard,
                            ErrorCode::E0003,
                            "Invalid integer literal",
                            Span::for_single_line(
                                self.file_path,
                                line,
                                starting_position,
                                ending_position,
                            ),
                            "Remove this",
                            last_dot_position.unwrap(), // there's no other way to construct an invalid integer other than more than one dots
                        ));
                        self.advance();
                        continue 'mainloop;
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
                _ => errors.push(RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0001,
                    "Unknown character",
                    Span::for_single_char(self.file_path, self.line, self.offset),
                )),
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

    fn peek_next(&mut self) -> Option<char> {
        let mut iter = self.chars.clone();
        iter.next()
    }

    fn peek(&self) -> char {
        self.current
    }

    fn eof(&self) -> bool {
        self.pos >= self.src_len || self.peek() == '\x00'
    }

    fn advance(&mut self) {
        if self.peek() != '\n' {
            self.offset += 1;
        }
        self.pos += 1;
        if let Some(ch) = self.chars.next() {
            self.current = ch;

            if self.peek() == '\n' {
                self.line += 1;
                self.offset = 0;
            }
        }
    }
}
