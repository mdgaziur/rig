macro_rules! single_char_token {
    ($self:ident, $lexeme:literal, $type:expr) => {
        Token {
            span: Span::for_single_char($self.file_path.clone(), $self.line, $self.offset),
            lexeme: String::from($lexeme),
            literal: String::from($lexeme),
            token_type: $type,
        }
    };
}

macro_rules! double_char_token {
    ($self:ident, $tokens:ident, $repeat:literal, $repeat_lexeme:literal, $single_lexeme:literal, $repeat_type:expr, $single_type:expr) => {
        if !$self.eof() && $self.peek_next() == Some($repeat) {
            $tokens.push(Token {
                span: Span::for_single_line(
                    $self.file_path.clone(),
                    $self.line,
                    $self.offset,
                    $self.offset + 1,
                ),
                lexeme: String::from($repeat_lexeme),
                literal: String::from($repeat_lexeme),
                token_type: $repeat_type,
            });
            $self.advance();
        } else {
            $tokens.push(Token {
                span: Span::for_single_char($self.file_path.clone(), $self.line, $self.offset),
                lexeme: String::from($single_lexeme),
                literal: String::from($single_lexeme),
                token_type: $single_type,
            });
        }
    };

    ($self:ident, $tokens:ident, $repeat:literal, $repeat_lexeme:literal, $repeat_2:literal, $repeat_2_lexeme:literal, $single_lexeme:literal, $repeat_type:expr, $repeat_2_type:expr, $single_type:expr) => {
        if !$self.eof() && $self.peek_next() == Some($repeat) {
            $tokens.push(Token {
                span: Span::for_single_line(
                    $self.file_path.clone(),
                    $self.line,
                    $self.offset,
                    $self.offset + 1,
                ),
                lexeme: String::from($repeat_lexeme),
                literal: String::from($repeat_lexeme),
                token_type: $repeat_type,
            });
            $self.advance();
        } else if !$self.eof() && $self.peek_next() == Some($repeat_2) {
            $tokens.push(Token {
                span: Span::for_single_line(
                    $self.file_path.clone(),
                    $self.line,
                    $self.offset,
                    $self.offset + 1,
                ),
                lexeme: String::from($repeat_2_lexeme),
                literal: String::from($repeat_2_lexeme),
                token_type: $repeat_2_type,
            });
            $self.advance();
        } else {
            $tokens.push(Token {
                span: Span::for_single_char($self.file_path.clone(), $self.line, $self.offset),
                lexeme: String::from($single_lexeme),
                literal: String::from($single_lexeme),
                token_type: $single_type,
            });
        }
    };
}

macro_rules! triple_char_token {
    ($self:ident, $tokens:ident, $second_char:literal, $second_char2:literal, $third_char:literal, $single_char_lexeme:literal, $second_char_lexeme:literal, $second_char_lexeme2:literal, $third_char_lexeme:literal, $single_type:expr, $two_char_type:expr, $two_char_type2:expr, $three_char_type:expr) => {
        if !$self.eof() && $self.peek_next() == Some($second_char) {
            $self.advance();
            if !$self.eof() && $self.peek_next() == Some($third_char) {
                $tokens.push(Token {
                    span: Span::for_single_line(
                        $self.file_path.clone(),
                        $self.line,
                        $self.pos - 1,
                        $self.pos + 1,
                    ),
                    literal: String::from($third_char_lexeme),
                    lexeme: String::from($third_char_lexeme),
                    token_type: $three_char_type,
                });
                $self.advance();
            } else {
                $tokens.push(Token {
                    span: Span::for_single_line(
                        $self.file_path.clone(),
                        $self.line,
                        $self.pos - 1,
                        $self.pos,
                    ),
                    literal: String::from($second_char_lexeme),
                    lexeme: String::from($second_char_lexeme),
                    token_type: $two_char_type,
                });
            }
        } else if !$self.eof() && $self.peek_next() == Some($second_char2) {
            $tokens.push(Token {
                span: Span::for_single_line(
                    $self.file_path.clone(),
                    $self.line,
                    $self.pos,
                    $self.pos + 1,
                ),
                literal: String::from($second_char_lexeme2),
                lexeme: String::from($second_char_lexeme2),
                token_type: $two_char_type2,
            });
            $self.advance();
        } else {
            $tokens.push(Token {
                span: Span::for_single_char($self.file_path.clone(), $self.line, $self.pos),
                literal: String::from($single_char_lexeme),
                lexeme: String::from($single_char_lexeme),
                token_type: $single_type,
            })
        }
    };
}
