use bimap::BiMap;
use conquer_once::OnceCell;
use parking_lot::RwLock;
use std::fmt::{Debug, Display, Formatter};

pub static INTERNER: OnceCell<RwLock<Interner>> = OnceCell::uninit();

#[derive(Debug)]
pub struct Interner {
    interned_strings: BiMap<usize, String>,
}

impl Interner {
    pub fn new() -> Self {
        Self {
            interned_strings: BiMap::new(),
        }
    }

    pub fn intern(&mut self, s: String) -> InternedString {
        if self.interned_strings.contains_right(&s) {
            InternedString(*self.interned_strings.get_by_right(&s).unwrap())
        } else {
            // Length shouldn't decrease as we never insert same string twice
            self.interned_strings.insert(self.interned_strings.len(), s);
            InternedString(self.interned_strings.len() - 1)
        }
    }

    pub fn get_interned_string(&self, is: InternedString) -> &str {
        self.interned_strings.get_by_left(&is.0).unwrap()
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct InternedString(usize);

impl InternedString {
    pub fn get(&self) -> String {
        INTERNER
            .get()
            .unwrap()
            .read()
            .get_interned_string(*self)
            .to_string()
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            INTERNER
                .get()
                .unwrap()
                .read()
                .get_interned_string(*self)
                .to_string()
        )
    }
}

impl Debug for InternedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        <Self as Display>::fmt(self, f)
    }
}

#[macro_export]
macro_rules! intern {
    ($s:expr) => {
        {
            // To prevent deadlock(apparently the read lock to INTERNER stays alive after getting interned string)
            let interned = ::rig_intern::INTERNER
                .get()
                .unwrap()
                .write()
                .intern($s.to_string());
            interned
        }
    };
}
