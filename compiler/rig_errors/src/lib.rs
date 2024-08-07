use owo_colors::OwoColorize;
use rig_intern::{intern, InternedString, INTERNER};
use rig_session::Session;
use rig_span::{FullLineSnippet, Span};

use std::env::args;
use std::fmt::Display;

pub fn display_compiler_error(message: impl ToString + Display) {
    eprintln!("{}", message.red());
}

#[derive(Debug, Clone)]
pub struct CodeError {
    pub message: InternedString,
    pub hints: Vec<Diagnostic>,
    pub notes: Vec<Diagnostic>,
    pub error_code: ErrorCode,
    pub pos: Span,
}

impl CodeError {
    pub fn without_notes_and_hint(message: &str, pos: Span, error_code: ErrorCode) -> Self {
        Self {
            message: intern!(message),
            pos,
            error_code,
            notes: vec![],
            hints: vec![],
        }
    }

    pub fn unexpected_token(pos: Span) -> Self {
        Self {
            error_code: ErrorCode::SyntaxError,
            message: intern!("unexpected token"),
            pos,
            notes: vec![],
            hints: vec![],
        }
    }

    pub fn unexpected_token_with_hint(pos: Span, hint: impl ToString) -> Self {
        Self {
            error_code: ErrorCode::SyntaxError,
            message: intern!("unexpected token"),
            pos,
            notes: vec![],
            hints: vec![Diagnostic {
                message: intern!(hint),
                pos,
            }],
        }
    }

    pub fn unexpected_token_with_hintpos(pos: Span, hint: impl ToString, hint_pos: Span) -> Self {
        Self {
            error_code: ErrorCode::SyntaxError,
            message: intern!("unexpected token"),
            pos,
            notes: vec![],
            hints: vec![Diagnostic {
                message: intern!(hint),
                pos: hint_pos,
            }],
        }
    }

    pub fn unexpected_token_with_note(pos: Span, note: impl ToString) -> Self {
        Self {
            error_code: ErrorCode::SyntaxError,
            message: intern!("unexpected token"),
            pos,
            notes: vec![Diagnostic {
                message: intern!(note),
                pos,
            }],
            hints: vec![],
        }
    }

    pub fn warning(message: impl ToString, pos: Span) -> Self {
        Self {
            error_code: ErrorCode::Warning,
            message: intern!(message),
            pos,
            notes: vec![],
            hints: vec![],
        }
    }

    pub fn display(&self, session: &Session) {
        display_diag(
            session,
            self.message.get(),
            self.pos,
            self.error_code.to_diag_kind(),
            true,
        );

        for hint in self.hints.iter() {
            display_diag(
                session,
                hint.message.get(),
                hint.pos,
                DiagKind::Hint,
                self.pos != hint.pos,
            );
        }

        for note in self.notes.iter() {
            display_diag(
                session,
                note.message.get(),
                note.pos,
                DiagKind::Note,
                self.pos != note.pos,
            );
        }

        if self.error_code != ErrorCode::Warning {
            eprintln!(
                "{}\n",
                format!(
                    "Try running `{} explain {}`",
                    args().next().unwrap(),
                    self.error_code.to_error_code()
                )
                .bold()
            );
        }
    }
}

fn display_diag(
    session: &Session,
    message: String,
    pos: Span,
    kind: DiagKind,
    display_snippet: bool,
) {
    let interner = INTERNER.get().unwrap().read();
    let FullLineSnippet {
        snippet,
        starting_line,
        ending_line,
        starting_line_offset,
        ending_line_offset,
        ..
    } = pos.get_snippet(interner.get_interned_string(session.get_file_content(pos.file_path)));
    let snippet = snippet.replace('\t', "    ");
    let snippet_lines = snippet.trim_end().split('\n').collect::<Vec<&str>>();
    let min_padding_before_bar = ending_line.to_string().len();

    eprintln!("{}: {}", kind, message.bold());
    if !display_snippet {
        return;
    }

    eprintln!(
        "{}{} {}:{}:{}",
        " ".repeat(min_padding_before_bar),
        "-->".blue().bold(),
        pos.file_path.get(),
        starting_line,
        starting_line_offset + 1
    );
    let empty_bar = format!("{} |", " ".repeat(min_padding_before_bar));
    eprintln!("{}", empty_bar.blue().bold());
    let mut line_num = starting_line;
    for line in snippet_lines.iter() {
        let line_num_disp = format!(
            "{}{} | ",
            line_num,
            " ".repeat(min_padding_before_bar - line_num.to_string().len())
        );
        eprintln!("{}{}", line_num_disp.blue().bold(), line);
        let marker_count = if line_num != starting_line && line_num != ending_line {
            line.len()
        } else if line_num == starting_line && line_num == ending_line {
            ending_line_offset - starting_line_offset + 1
        } else if line_num == starting_line {
            line.len() - starting_line_offset
        } else {
            ending_line_offset + 1
        };

        let markers = format!(
            "{}{}",
            " ".repeat(if line_num == starting_line {
                starting_line_offset
            } else {
                0
            }),
            "^".repeat(marker_count)
        );
        eprintln!("{} {}", empty_bar.blue().bold(), markers.yellow().bold());
        line_num += 1;
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorCode {
    SyntaxError,
    Warning,
    MissingFunctionBody,
    EmptyMatchExpression,
}

impl ErrorCode {
    pub fn to_error_code(self) -> &'static str {
        match self {
            ErrorCode::SyntaxError => "E0001",
            ErrorCode::Warning => "Warning",
            ErrorCode::MissingFunctionBody => "E0002",
            ErrorCode::EmptyMatchExpression => "E0003",
        }
    }

    fn to_diag_kind(self) -> DiagKind {
        match self {
            ErrorCode::SyntaxError => DiagKind::Error("0001".to_string()),
            ErrorCode::Warning => DiagKind::Warning,
            ErrorCode::MissingFunctionBody => DiagKind::Error("0002".to_string()),
            ErrorCode::EmptyMatchExpression => DiagKind::Error("0003".to_string()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Diagnostic {
    pub message: InternedString,
    pub pos: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum DiagKind {
    Error(String),
    Warning,
    Note,
    Hint,
}

impl Display for DiagKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            DiagKind::Error(code) => format!("error[{code}]").red().bold().to_string(),
            DiagKind::Warning => "warning".yellow().bold().to_string(),
            DiagKind::Note => "note".blue().bold().to_string(),
            DiagKind::Hint => "hint".green().bold().to_string(),
        };
        write!(f, "{}", str)
    }
}
