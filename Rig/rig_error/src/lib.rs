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

    /// Redefinition of function or struct
    E0008,

    /// Cannot import module
    E0009,

    /// Attempt to import private type
    E0010,

    /// Ambiguous import
    E0011,

    /// Invalid import
    E0012,

    /// Failed to resolve type
    E0013,

    /// Expected type, found module
    E0014,

    /// Leaking private type
    E0015,

    /// Invalid multi line comment
    E0016,
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

    pub fn print_span(
        &self,
        lines: &[&str],
        blank_line: &str,
        span: &Span,
        max_line_num_size: usize,
        print_trailing_empty_line: bool,
    ) {
        for line_num in span.starting_line..span.ending_line + 1 {
            let line = lines[line_num - 1].replace('\t', "    ");

            if !print_trailing_empty_line {
                if line_num == span.ending_line && line == "" {
                    break;
                }
            }

            let tab_count = count_tab_until(lines[line_num - 1], span.starting_line_offset);
            let padding = if line_num == self.span.starting_line {
                (span.starting_line_offset + tab_count * 4)
                    .checked_sub(tab_count)
                    .unwrap_or_default()
            } else {
                0
            };
            let count;

            if span.starting_line == span.ending_line {
                if span.starting_line_offset == span.ending_line_end_offset {
                    count = 1;
                } else {
                    let tab_count_until_end = tab_count
                        - count_tab_until(lines[line_num - 1], span.ending_line_end_offset);

                    count = (span.ending_line_end_offset - span.starting_line_offset)
                        + 1
                        + 4 * tab_count_until_end
                        - tab_count_until_end;
                }
            } else if line_num == span.starting_line {
                count = line.len() - padding;
            } else {
                if line_num == span.ending_line {
                    let tab_count_until_end =
                        count_tab_until(lines[line_num - 1], span.ending_line_end_offset);

                    count = span.ending_line_end_offset + 1 + tab_count_until_end * 4
                        - tab_count_until_end;
                } else {
                    count = line.len() - padding;
                }
            }

            Self::print_line(line_num, &line, max_line_num_size);
            Self::write_marker(&blank_line, padding + 1, count);
        }
    }

    pub fn print(&self, file_content: &str) {
        let lines = file_content.split('\n').collect::<Vec<&str>>();
        let max_line_num_size = self.line_number_max_size();
        let blank_line = format!("{} |", " ".repeat(max_line_num_size));

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
            " ".repeat(max_line_num_size),
            "-->".bright_blue().bold(),
            self.span.to_string()
        );

        eprintln!("{}", blank_line.bright_blue().bold());

        self.print_span(&lines, &blank_line, &self.span, max_line_num_size, false);

        eprintln!("{}", blank_line.bright_blue().bold());

        if let Some(hint_span) = &self.hint_span {
            eprintln!(
                "{}",
                format!("{} {}", "help:".green(), self.hint.as_ref().unwrap()).bold()
            );
            eprintln!("{}", blank_line.bright_blue().bold());

            self.print_span(&lines, &blank_line, &hint_span, max_line_num_size, true);

            eprintln!("{}", blank_line.bright_blue().bold());
        }

        for note in &self.notes {
            eprintln!(
                "{}",
                format!("{} {}", "note:".bright_blue(), note.message).bold()
            );
            eprintln!("{}", blank_line.bright_blue().bold());

            self.print_span(&lines, &blank_line, &note.span, max_line_num_size, true);

            eprintln!("{}", blank_line.bright_blue().bold());
        }

        eprintln!(
            "{}",
            format!(
                "For more information about this error, try `{} explain {:?}`\n",
                std::env::args().nth(0).unwrap(),
                self.error_code
            )
            .bold()
        );
    }
}

fn count_tab_until(line: &str, offset: usize) -> usize {
    let mut count = 0;

    for (idx, ch) in line.chars().enumerate() {
        if ch == '\t' {
            count += 1;
        }

        if idx == offset {
            break;
        }
    }

    count
}
