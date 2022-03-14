/// ## [Span] describes the location of a part of code. It's mainly used for diagnostics
/// Examples:
/// ```rig
/// print "Unterminated string
///       ^^^^^^^^^^^^^^^^^^^^
/// literal
/// ^^^^^^^
/// ```
/// The part can be represented in the following way:
/// ```
/// use rig_span::Span;
///
/// Span {
///     file_name: String::from("source.rig"),
///     starting_line: 1,
///     starting_line_offset: 5,
///     ending_line: 2,
///     ending_line_end_offset: 6
/// };
/// ```
///
#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    /// File name
    pub file_name: String,

    /// The line where the part starts
    pub starting_line: usize,

    /// The offset on the starting line where the part starts
    pub starting_line_offset: usize,

    /// The line where the part ends
    pub ending_line: usize,

    /// The offset on the ending line where the part ends
    pub ending_line_end_offset: usize,
}

impl Span {
    pub fn for_single_char(file_name: &str, line: usize, offset: usize) -> Self {
        Self {
            file_name: file_name.to_string(),
            starting_line: line,
            starting_line_offset: offset,
            ending_line: line,
            ending_line_end_offset: offset,
        }
    }

    pub fn for_single_line(
        file_name: &str,
        line: usize,
        offset_start: usize,
        offset_end: usize,
    ) -> Self {
        Self {
            file_name: file_name.to_string(),
            starting_line: line,
            starting_line_offset: offset_start,
            ending_line: line,
            ending_line_end_offset: offset_end,
        }
    }

    pub fn merge(s1: Span, s2: Span) -> Self {
        Self {
            file_name: s1.file_name.to_string(),
            starting_line: s1.starting_line,
            starting_line_offset: s1.starting_line_offset,
            ending_line: s2.ending_line,
            ending_line_end_offset: s2.ending_line_end_offset,
        }
    }
}

impl ToString for Span {
    fn to_string(&self) -> String {
        format!(
            "{}:{}:{}",
            self.file_name, self.starting_line, self.starting_line_offset + 1
        )
    }
}
