macro_rules! single_char_token {
    ($self:ident, $lexeme:literal, $type:expr) => {
        Token {
            span: Span::for_single_char($self.file_path, $self.line, $self.offset),
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
                    $self.file_path,
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
                span: Span::for_single_char($self.file_path, $self.line, $self.offset),
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
                    $self.file_path,
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
                    $self.file_path,
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
                span: Span::for_single_char($self.file_path, $self.line, $self.offset),
                lexeme: String::from($single_lexeme),
                literal: String::from($single_lexeme),
                token_type: $single_type,
            });
        }
    };
}
