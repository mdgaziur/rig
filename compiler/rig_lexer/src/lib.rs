use crate::token::{LexicalToken, NumberKind, TokenKind};
use rig_errors::{CodeError, ErrorCode};
use rig_intern::{intern, InternedString, INTERNER};
use rig_span::Span;
use std::str::Chars;

pub mod token;

pub struct Lexer<'l> {
    file_content_iterator: Chars<'l>,
    file_path: InternedString,
    pos: usize,
    len: usize,
}

impl<'l> Lexer<'l> {
    pub fn new(file_content_iterator: Chars<'l>, file_path: InternedString) -> Self {
        Self {
            len: file_content_iterator.clone().count(),
            file_content_iterator,
            file_path,
            pos: 0,
        }
    }

    // Does lexical analysis on the next character(s)
    // Caller must ensure that the lexer hasn't reached the end of
    // file
    pub fn lex_once(&mut self) -> Result<LexicalToken, CodeError> {
        assert!(!self.is_eof());

        match self.current() {
            '(' => Ok(LexicalToken {
                kind: TokenKind::LParen,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            ')' => Ok(LexicalToken {
                kind: TokenKind::RParen,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            '{' => Ok(LexicalToken {
                kind: TokenKind::LBrace,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            '}' => Ok(LexicalToken {
                kind: TokenKind::RBrace,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            '[' => Ok(LexicalToken {
                kind: TokenKind::LBracket,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            ']' => Ok(LexicalToken {
                kind: TokenKind::RBracket,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            '=' => match self.try_peek_next() {
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::Eq,
                        raw: intern!("=="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Assign,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '<' => match self.try_peek_next() {
                Some('<') => {
                    self.advance();
                    match self.try_peek_next() {
                        Some('=') => {
                            self.advance();
                            Ok(LexicalToken {
                                kind: TokenKind::LShiftEq,
                                raw: intern!("<<="),
                                span: Span::new(self.pos - 2, self.pos, self.file_path),
                            })
                        }
                        _ => Ok(LexicalToken {
                            kind: TokenKind::LShift,
                            raw: intern!("<<"),
                            span: Span::new(self.pos - 1, self.pos, self.file_path),
                        }),
                    }
                }
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::LessEq,
                        raw: intern!("<="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Less,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '>' => match self.try_peek_next() {
                Some('>') => {
                    self.advance();
                    match self.try_peek_next() {
                        Some('=') => {
                            self.advance();
                            Ok(LexicalToken {
                                kind: TokenKind::RShiftEq,
                                raw: intern!(">>="),
                                span: Span::new(self.pos - 2, self.pos, self.file_path),
                            })
                        }
                        _ => Ok(LexicalToken {
                            kind: TokenKind::RShift,
                            raw: intern!(">>"),
                            span: Span::new(self.pos - 1, self.pos, self.file_path),
                        }),
                    }
                }
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::GreaterEq,
                        raw: intern!(">="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Greater,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '+' => match self.try_peek_next() {
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::PlusEq,
                        raw: intern!("+="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Plus,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '-' => match self.try_peek_next() {
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::MinusEq,
                        raw: intern!("-="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Minus,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '*' => match self.try_peek_next() {
                Some('*') => {
                    self.advance();
                    match self.try_peek_next() {
                        Some('=') => {
                            self.advance();
                            Ok(LexicalToken {
                                kind: TokenKind::PowerEq,
                                raw: intern!("**="),
                                span: Span::new(self.pos - 2, self.pos, self.file_path),
                            })
                        }
                        _ => Ok(LexicalToken {
                            kind: TokenKind::Power,
                            raw: intern!("**"),
                            span: Span::new(self.pos - 1, self.pos, self.file_path),
                        }),
                    }
                }
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::MulEq,
                        raw: intern!("*="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Mul,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '/' => match self.try_peek_next() {
                Some('/') => {
                    self.advance();
                    while !self.is_eof() && self.current() != '\n' {
                        self.advance();
                    }

                    if !self.is_eof() {
                        self.lex_once()
                    } else {
                        Ok(LexicalToken {
                            kind: TokenKind::Eof,
                            raw: intern!(""),
                            span: Span::new(self.pos, self.pos, self.file_path),
                        })
                    }
                }
                Some('*') => {
                    // /* comment */
                    //  ^
                    self.advance();
                    // /* comment */
                    //   ^
                    self.advance();

                    let mut depth = 1;
                    while !self.is_eof() && depth != 0 {
                        if self.current() == '/' && self.try_peek_next() == Some('*') {
                            depth += 1;
                            self.advance();
                        } else if self.current() == '*' && self.try_peek_next() == Some('/') {
                            depth -= 1;
                            dbg!(depth);
                            self.advance();
                        }

                        self.advance();
                    }

                    if depth != 0 && self.is_eof() {
                        Err(CodeError {
                            error_code: ErrorCode::SyntaxError,
                            message: intern!("Unterminated multiline comment"),
                            pos: Span::new(self.pos, self.pos, self.file_path),
                            hints: vec![],
                            notes: vec![],
                        })?
                    } else {
                        Ok(LexicalToken {
                            kind: TokenKind::Eof,
                            raw: intern!(""),
                            span: Span::new(self.pos, self.pos, self.file_path),
                        })
                    }
                }
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::DivEq,
                        raw: intern!("/="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Div,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '&' => match self.try_peek_next() {
                Some('&') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::LogicalAnd,
                        raw: intern!("&&"),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::AndEq,
                        raw: intern!("&="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::And,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '|' => match self.try_peek_next() {
                Some('|') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::LogicalOr,
                        raw: intern!("||"),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::OrEq,
                        raw: intern!("|="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Or,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '^' => match self.try_peek_next() {
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::XorEq,
                        raw: intern!("^="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Xor,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '!' => match self.try_peek_next() {
                Some('=') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::NotEq,
                        raw: intern!("!="),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Not,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            '~' => Ok(LexicalToken {
                kind: TokenKind::BinaryNot,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            ':' => match self.try_peek_next() {
                Some(':') => {
                    self.advance();
                    Ok(LexicalToken {
                        kind: TokenKind::PathSep,
                        raw: intern!("::"),
                        span: Span::new(self.pos - 1, self.pos, self.file_path),
                    })
                }
                _ => Ok(LexicalToken {
                    kind: TokenKind::Colon,
                    raw: intern!(self.current()),
                    span: Span::new(self.pos, self.pos, self.file_path),
                }),
            },
            ',' => Ok(LexicalToken {
                kind: TokenKind::Comma,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            ';' => Ok(LexicalToken {
                kind: TokenKind::Semi,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            '.' => Ok(LexicalToken {
                kind: TokenKind::Dot,
                raw: intern!(self.current()),
                span: Span::new(self.pos, self.pos, self.file_path),
            }),
            ch if ch.is_ascii_digit() => {
                fn collect_digits(lexer: &mut Lexer, radix: u32) -> String {
                    let mut number = String::new();

                    // NOTE: We keep eating dots in decimal numbers here. Decimal numbers
                    //       will get validated by the parser. This is done to make sure
                    //       that codes like "123.method()" don't get rejected.
                    while !lexer.is_eof()
                        && (lexer.current().is_digit(radix)
                            || (lexer.current() == '.' && radix == 10))
                    {
                        number.push(lexer.current());
                        lexer.advance();
                    }

                    if !lexer.is_eof() && !number.is_empty() {
                        lexer.retreat();
                    }

                    if number.is_empty() {
                        number = String::from("0");
                    }

                    number
                }
                let start_pos = self.pos;
                let number_kind;
                let number;

                if ch == '0' {
                    number = match self.try_peek_next() {
                        Some('x') => {
                            number_kind = NumberKind::Hex;
                            self.advance();
                            self.advance();
                            collect_digits(self, 16)
                        }
                        Some('b') => {
                            number_kind = NumberKind::Bin;
                            self.advance();
                            self.advance();
                            collect_digits(self, 2)
                        }
                        Some('o') => {
                            number_kind = NumberKind::Oct;
                            self.advance();
                            self.advance();
                            collect_digits(self, 8)
                        }
                        Some(ch) if ch.is_ascii_digit() => {
                            number_kind = NumberKind::Dec;
                            collect_digits(self, 10)
                        }
                        _ => {
                            number_kind = NumberKind::Dec;
                            String::from("0")
                        }
                    };
                } else {
                    number_kind = NumberKind::Dec;
                    number = collect_digits(self, 10);
                }

                Ok(LexicalToken {
                    kind: TokenKind::Number {
                        number: intern!(number),
                        kind: number_kind,
                    },
                    span: Span::new(start_pos, self.pos, self.file_path),
                    raw: intern!(number),
                })
            }
            ch if ch.is_alphabetic() || ch == '_' => {
                let start_pos = self.pos;
                let mut ident = String::new();
                while !self.is_eof() && (self.current().is_alphanumeric() || self.current() == '_')
                {
                    ident.push(self.current());
                    self.advance();
                }
                self.retreat();

                let kind = match ident.as_str() {
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "fn" => TokenKind::Fn,
                    "pub" => TokenKind::Pub,
                    "struct" => TokenKind::Struct,
                    "trait" => TokenKind::Trait,
                    "mut" => TokenKind::Mut,
                    "const" => TokenKind::Const,
                    "static" => TokenKind::Static,
                    "type" => TokenKind::Type,
                    "use" => TokenKind::Use,
                    "for" => TokenKind::For,
                    "while" => TokenKind::While,
                    "loop" => TokenKind::Loop,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "anon" => TokenKind::Anon,
                    "vararg" => TokenKind::Vararg,
                    "let" => TokenKind::Let,
                    "where" => TokenKind::Where,
                    "impl" => TokenKind::Impl,
                    "in" => TokenKind::In,
                    "mod" => TokenKind::Mod,
                    _ => TokenKind::Ident(intern!(ident)),
                };

                Ok(LexicalToken {
                    kind,
                    span: Span::new(start_pos, self.pos, self.file_path),
                    raw: intern!(ident),
                })
            }
            ch if ch.is_whitespace() => {
                self.advance();
                if !self.is_eof() {
                    self.lex_once()
                } else {
                    Ok(LexicalToken {
                        kind: TokenKind::Eof,
                        raw: intern!(""),
                        span: Span::new(self.pos, self.pos, self.file_path),
                    })
                }
            }
            '"' => {
                let start_pos = self.pos;
                let mut s = String::new();
                self.advance();

                while !self.is_eof() && self.current() != '"' {
                    s.push(self.current());
                    self.advance();
                }

                if self.is_eof() {
                    Err(CodeError {
                        error_code: ErrorCode::SyntaxError,
                        message: intern!("Unterminated string literal"),
                        pos: Span::new(self.pos, self.pos, self.file_path),
                        hints: vec![],
                        notes: vec![],
                    })?
                } else {
                    Ok(LexicalToken {
                        kind: TokenKind::String(intern!(s.clone())),
                        raw: intern!(format!("\"{s}\"")),
                        span: Span::new(start_pos, self.pos, self.file_path),
                    })
                }
            }
            _ => Err(CodeError {
                error_code: ErrorCode::SyntaxError,
                message: intern!(format!("Unknown character: \"{}\"", self.current())),
                pos: Span::new(self.pos, self.pos, self.file_path),
                hints: vec![],
                notes: vec![],
            })?,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.len <= self.pos
    }

    pub fn advance(&mut self) {
        self.pos += 1;
    }

    fn retreat(&mut self) {
        self.pos -= 1;
    }

    fn current(&self) -> char {
        self.file_content_iterator.clone().nth(self.pos).unwrap()
    }

    fn try_peek_next(&self) -> Option<char> {
        self.file_content_iterator.clone().nth(self.pos + 1)
    }
}