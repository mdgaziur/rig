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
///     starting_line_end_offset: 24,
///     ending_line: 2,
///     ending_line_offset: 0,
///     ending_line_end_offset: 6
/// };
/// ```
///
#[derive(Debug, PartialEq)]
pub struct Span {
    /// File name
    pub file_name: String,

    /// The line where the part starts
    pub starting_line: usize,

    /// The offset on the starting line where the part starts
    pub starting_line_offset: usize,

    /// The offset on the starting line where the part ends
    pub starting_line_end_offset: usize,

    /// The line where the part ends
    pub ending_line: usize,

    /// The offset on the ending line where the part ends
    pub ending_line_offset: usize,

    /// The offset on the ending line where the part ends
    pub ending_line_end_offset: usize,
}

impl Span {
    pub fn for_single_char(file_name: String, line: usize, offset: usize) -> Self {
        Self {
            file_name,
            starting_line: line,
            starting_line_offset: offset,
            starting_line_end_offset: offset,
            ending_line: line,
            ending_line_offset: offset,
            ending_line_end_offset: offset,
        }
    }

    pub fn for_single_line(
        file_name: String,
        line: usize,
        offset_start: usize,
        offset_end: usize,
    ) -> Self {
        Self {
            file_name,
            starting_line: line,
            starting_line_offset: offset_start,
            starting_line_end_offset: offset_end,
            ending_line: line,
            ending_line_offset: offset_start,
            ending_line_end_offset: offset_end,
        }
    }
}
