mod utils;

use crate::utils::number_len;
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

    /// File path
    pub file_path: String,
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
    E0007
}

/// Describes the type of error
#[derive(Debug, Clone)]
pub enum ErrorType {
    /// Error
    Hard,

    /// Warning
    Soft,
}

impl Display for ErrorType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::Hard => write!(f, "{}", "Error".red().bold()),
            ErrorType::Soft => write!(f, "{}", "Warning".yellow().bold()),
        }
    }
}

impl RigError {
    fn write_marker(
        &self,
        starting_offset: usize,
        ending_offset: usize,
        blank_line: &str,
        hint: Option<&String>,
    ) {
        let mut hint_string = String::new();
        let mut marker_count = ending_offset - starting_offset + 1;
        if hint.is_some() && marker_count == 1 {
            marker_count = 0;
        }
        if let Some(hint) = hint {
            hint_string = String::from("^ hint: ") + hint;
        }
        if marker_count > 0 {
            eprintln!(
                "{} {}{}",
                blank_line.bold().blue(),
                " ".repeat(starting_offset),
                "^".repeat(marker_count).yellow().bold(),
            );
        }
        if hint.is_some() {
            eprintln!(
                "{} {}{}",
                blank_line.blue().bold(),
                " ".repeat(starting_offset + marker_count),
                hint_string.blue().bold()
            );
        }
    }

    fn write_line(&self, prefix: &str, line_number: usize, file_content: &str) {
        eprintln!(
            "{} {}",
            prefix.blue().bold(),
            file_content.split('\n').nth(line_number - 1).unwrap()
        )
    }

    pub fn print(&self, file_content: &str) {
        eprintln!("{}[{:?}]: {}", self.error_type, self.error_code, self.message);
        let starting_line_num_len = number_len(self.span.starting_line);
        let ending_line_num_len = number_len(self.span.ending_line);
        let max_line_num_len = max(starting_line_num_len, ending_line_num_len);
        let blank_line = format!("{} |", " ".repeat(max_line_num_len)).blue().bold();
        let first_line_number = format!(
            "{}{}|",
            self.span.starting_line,
            " ".repeat(max_line_num_len - starting_line_num_len + 1)
        );
        let last_line_number = format!(
            "{}{}|",
            self.span.ending_line,
            " ".repeat(max_line_num_len - ending_line_num_len + 1)
        );

        eprintln!(
            "{}{} {}:{}:{}",
            " ".repeat(max_line_num_len),
            "-->".blue().bold(),
            self.file_path,
            self.span.starting_line,
            self.span.starting_line_offset + 1
        );
        eprintln!("{}", blank_line);
        self.write_line(&first_line_number, self.span.starting_line, file_content);
        if self.span.starting_line != self.span.ending_line {
            self.write_marker(
                self.span.starting_line_offset,
                self.span.starting_line_end_offset,
                &blank_line,
                None,
            );
        } else {
            self.write_marker(
                self.span.starting_line_offset,
                self.span.ending_line_end_offset,
                &blank_line,
                self.hint.as_ref(),
            );
        }

        if self.span.ending_line - self.span.starting_line > 1 {
            eprintln!("{} {}", blank_line, "...".blue().bold());
        }
        if self.span.ending_line - self.span.starting_line >= 1 {
            self.write_line(&last_line_number, self.span.ending_line, file_content);
            self.write_marker(
                self.span.ending_line_offset,
                self.span.ending_line_end_offset,
                &blank_line,
                self.hint.as_ref(),
            );
        }
        eprintln!("{}", blank_line);

        eprintln!();
        eprintln!(
            "{}{}{}",
            "note: use `rig explain ".blue().bold(),
            format!("{:?}", self.error_code).blue().bold(),
            "` to know more about the error".blue().bold()
        );
    }
}
