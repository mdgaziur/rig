use rig_intern::{InternedString, INTERNER};
use rig_session::Session;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
    pub file_path: InternedString,
}

impl Span {
    pub fn new(lo: usize, hi: usize, file_path: InternedString) -> Self {
        Self { lo, hi, file_path }
    }

    pub fn contains(&self, rhs: Span) -> bool {
        self.lo <= rhs.lo && self.hi >= rhs.hi
    }

    pub fn is_within(&self, rhs: Span) -> bool {
        self.lo >= rhs.lo && self.hi <= rhs.hi
    }

    pub fn get_snippet(&self, sess: &Session) -> FullLineSnippet {
        fn get_line_for_offset(file_content: &str, offset: usize) -> (usize, usize, usize, usize) {
            let mut line = 1;
            let mut line_start = 0;
            let mut line_end = 0;
            let mut line_pos_offset = 0;
            let mut break_on_newline = false;

            for (pos, ch) in file_content.chars().enumerate() {
                line_end = pos;
                if pos == offset {
                    if ch == '\n' {
                        line += 1;
                    }
                    line_pos_offset = pos - line_start;
                    break_on_newline = true;
                }
                if ch == '\n' {
                    if break_on_newline {
                        line_end = pos;
                        break;
                    } else {
                        line_start = pos + 1;
                        line += 1;
                    }
                }
            }

            (line, line_pos_offset, line_start, line_end)
        }

        let interner = INTERNER.get().unwrap().read();
        let file_content = interner.get_interned_string(sess.get_file_content(self.file_path));
        let (starting_line, starting_line_offset, starting_line_start_offset, _) =
            get_line_for_offset(file_content, self.lo);
        let (ending_line, ending_line_offset, _, ending_line_end_offset) =
            get_line_for_offset(file_content, self.hi);

        FullLineSnippet {
            snippet: file_content[starting_line_start_offset..ending_line_end_offset + 1]
                .to_string(),
            starting_line_offset,
            starting_line_start_offset,
            ending_line_offset,
            ending_line_end_offset,
            starting_line,
            ending_line,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FullLineSnippet {
    pub snippet: String,
    pub starting_line_offset: usize,
    pub starting_line_start_offset: usize,
    pub ending_line_offset: usize,
    pub ending_line_end_offset: usize,
    pub starting_line: usize,
    pub ending_line: usize,
}

#[cfg(test)]
mod test {
    use super::*;
    use rig_intern::{intern, Interner};

    fn test_wrapper(file_path: &str, file_content: &str, lo: usize, hi: usize) -> FullLineSnippet {
        use parking_lot::RwLock;

        INTERNER.init_once(|| RwLock::new(Interner::new()));
        let mut session = Session::new();
        session.insert_file(file_path, file_content);
        Span {
            hi,
            lo,
            file_path: intern!(file_path),
        }
        .get_snippet(&session)
    }

    #[test]
    fn test_single_line() {
        assert_eq!(
            test_wrapper("<test>", "asd\nas\nd", 0, 2),
            FullLineSnippet {
                snippet: String::from("asd\n"),
                starting_line: 1,
                starting_line_offset: 0,
                starting_line_start_offset: 0,
                ending_line: 1,
                ending_line_offset: 2,
                ending_line_end_offset: 3,
            }
        )
    }

    #[test]
    fn test_multi_line() {
        assert_eq!(
            test_wrapper("<test>", "asd\nas\nd", 1, 7),
            FullLineSnippet {
                snippet: String::from("asd\nas\nd"),
                starting_line: 1,
                starting_line_offset: 1,
                starting_line_start_offset: 0,
                ending_line: 3,
                ending_line_offset: 0,
                ending_line_end_offset: 7,
            }
        )
    }

    #[test]
    fn test_multi_line1() {
        assert_eq!(
            test_wrapper("<test>", "asd\nas\nda", 5, 7),
            FullLineSnippet {
                snippet: String::from("as\nda"),
                starting_line: 2,
                starting_line_offset: 1,
                starting_line_start_offset: 4,
                ending_line: 3,
                ending_line_offset: 0,
                ending_line_end_offset: 8,
            }
        )
    }
}
