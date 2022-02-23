use colored::Colorize;
use rig_span::Span;
use std::cmp::max;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone)]
pub struct RigError {
    /// Describes the type of error
    pub error_type: ErrorType,

    /// Error code
    pub error_code: ErrorCode,

    /// Describes the erroneous part of the code
    pub span: Span,

    /// Error message
    pub message: String,

    /// Hint
    pub hint: Option<String>,

    /// Location where hint will be displayed
    pub hint_span: Option<Span>,

    /// Notes
    pub notes: Vec<Note>,
}

#[derive(Clone, Debug)]
pub struct Note {
    pub span: Span,
    pub message: String,
}

/// Error code
#[derive(Debug, Clone)]
pub enum ErrorCode {
    /// Unknown character
    E0001,

    /// Unterminated string literal
    E0002,

    /// Invalid integer literal
    E0003,

    /// Invalid escape character
    E0004,

    /// Unexpected token
    E0005,

    /// Invalid assignment
    E0006,

    /// Unreachable/dead code
    E0007,
}

/// Describes the type of error
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    /// Error
    Hard,

    /// Warning
    Soft,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::Hard => write!(f, "{}", "error".bright_red().bold()),
            ErrorType::Soft => write!(f, "{}", "warning".bright_yellow().bold()),
        }
    }
}

trait LiteralSize {
    fn literal_size(self) -> usize;
}

impl LiteralSize for usize {
    fn literal_size(self) -> usize {
        (0..).take_while(|i| 10usize.pow(*i) <= self).count()
    }
}

impl RigError {
    pub fn with_no_hint_and_notes(
        error_type: ErrorType,
        error_code: ErrorCode,
        message: &str,
        span: Span,
    ) -> Self {
        RigError {
            error_type,
            error_code,
            message: message.to_string(),
            span,
            notes: vec![],
            hint: None,
            hint_span: None,
        }
    }

    pub fn with_hint(
        error_type: ErrorType,
        error_code: ErrorCode,
        message: &str,
        span: Span,
        hint: &str,
        hint_span: Span,
    ) -> Self {
        RigError {
            error_type,
            error_code,
            message: message.to_string(),
            span,
            notes: vec![],
            hint: Some(hint.to_string()),
            hint_span: Some(hint_span),
        }
    }

    fn line_number_max_size(&self) -> usize {
        let mut size = max(
            self.span.starting_line.literal_size(),
            self.span.ending_line.literal_size(),
        );

        if let Some(hint_span) = &self.hint_span {
            size = max(
                size,
                max(
                    hint_span.starting_line.literal_size(),
                    hint_span.ending_line.literal_size(),
                ),
            );
        }

        for note in &self.notes {
            size = max(
                size,
                max(
                    note.span.starting_line.literal_size(),
                    note.span.ending_line.literal_size(),
                ),
            );
        }

        size
    }

    fn print_line(line_number: usize, line: &str, max_line_number_size: usize) {
        if line == "" {
            return;
        }
        eprintln!(
            "{} {}",
            format!(
                "{}{}|",
                line_number,
                " ".repeat(max_line_number_size - line_number.literal_size() + 1)
            )
            .bright_blue()
            .bold(),
            line
        );
    }

    fn write_marker(blank_line: &str, padding: usize, count: usize) {
        if count == 0 {
            return;
        }
        eprintln!(
            "{}{}{}",
            blank_line.bright_blue().bold(),
            " ".repeat(padding),
            "^".repeat(count).bright_yellow().bold()
        );
    }

    fn print_span(span: &Span, file_content: &str, blank_line: &str, line_number_max_size: usize) {
        let lines = file_content.split("\n").collect::<Vec<&str>>();

        if span.ending_line - span.starting_line >= 3 {
            // print first two, print three dots, and then the last line :)
            Self::print_line(
                span.starting_line,
                lines[span.starting_line - 1],
                line_number_max_size,
            );
            Self::write_marker(
                &blank_line,
                span.starting_line_offset + 1,
                lines[span.starting_line - 1].len() - span.starting_line_offset,
            );
            Self::print_line(
                span.starting_line + 1,
                lines[span.starting_line],
                line_number_max_size,
            );
            Self::write_marker(&blank_line, 1, lines[span.starting_line].len());
            eprintln!(
                "{} {} {}",
                " ".repeat(line_number_max_size),
                "|".bright_blue().bold(),
                "...".bright_blue().bold()
            );
            Self::print_line(
                span.ending_line,
                lines[span.ending_line - 1],
                line_number_max_size,
            );
            Self::write_marker(
                &blank_line,
                1,
                lines[span.ending_line - 1].len()
                    - (lines[span.ending_line - 1].len() - span.ending_line_end_offset + 1)
                    + 1,
            );
        } else if span.starting_line != span.ending_line {
            Self::print_line(
                span.starting_line,
                lines[span.starting_line - 1],
                line_number_max_size,
            );
            Self::write_marker(
                &blank_line,
                span.starting_line_offset + 1,
                lines[span.starting_line - 1].len() - span.starting_line_offset,
            );

            Self::print_line(
                span.ending_line,
                lines[span.ending_line - 1],
                line_number_max_size,
            );
            Self::write_marker(&blank_line, 1, lines[span.ending_line - 1].len())
        } else {
            Self::print_line(
                span.starting_line,
                lines[span.starting_line - 1],
                line_number_max_size,
            );
            Self::write_marker(
                &blank_line,
                span.starting_line_offset + 1,
                span.ending_line_end_offset - span.starting_line_offset + 1,
            );
        }
    }

    pub fn print(&self, file_content: &str) {
        let line_number_max_size = self.line_number_max_size();
        let blank_line = format!("{} |", " ".repeat(line_number_max_size));

        eprintln!(
            "{}: {}",
            if self.error_type == ErrorType::Hard {
                format!("{}[{:?}]", self.error_type, self.error_code)
                    .bright_red()
                    .bold()
            } else {
                format!("{}[{:?}]", self.error_type, self.error_code)
                    .bright_yellow()
                    .bold()
            },
            self.message
        );
        eprintln!(
            "{}{} {}",
            " ".repeat(line_number_max_size),
            "-->".bright_blue().bold(),
            self.span.to_string()
        );

        eprintln!("{}", blank_line.bright_blue().bold());
        Self::print_span(&self.span, file_content, &blank_line, line_number_max_size);
        eprintln!("{}", blank_line.bright_blue().bold());

        if let Some(hint) = &self.hint {
            eprintln!(
                "{}{} {}",
                " ".repeat(line_number_max_size + 1),
                "= hint:".bright_blue().bold(),
                hint.bright_blue().bold()
            );

            eprintln!("{}", blank_line.bright_blue().bold());
            Self::print_span(
                &self.hint_span.as_ref().unwrap(),
                file_content,
                &blank_line,
                line_number_max_size,
            );
            eprintln!("{}", blank_line.bright_blue().bold());
        }

        for note in &self.notes {
            eprintln!(
                "{}{} {}",
                " ".repeat(line_number_max_size + 1),
                "= note:".bright_blue().bold(),
                note.message.bright_blue().bold()
            );

            eprintln!("{}", blank_line.bright_blue().bold());
            Self::print_span(&note.span, file_content, &blank_line, line_number_max_size);
            eprintln!("{}", blank_line.bright_blue().bold());
        }
    }
}
