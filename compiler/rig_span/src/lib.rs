use rig_intern::InternedString;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Span {
    lo: usize,
    hi: usize,
    file_path: InternedString,
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
}
