use bimap::BiMap;
use std::fmt::{Debug, Formatter};
use std::sync::{OnceLock, RwLock};

pub static INTERNER: OnceLock<RwLock<Interner>> = OnceLock::new();

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

    fn get_interned_string(&self, is: InternedString) -> &str {
        self.interned_strings.get_by_left(&is.0).unwrap()
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub struct InternedString(usize);

impl InternedString {
    pub fn get(&self) -> String {
        INTERNER
            .get()
            .unwrap()
            .read()
            .unwrap()
            .get_interned_string(*self)
            .to_string()
    }
}

impl Debug for InternedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{:?}",
            INTERNER
                .get()
                .unwrap()
                .read()
                .unwrap()
                .get_interned_string(*self)
                .to_string(),
        )
    }
}

#[macro_export]
macro_rules! intern {
    ($s:expr) => {
        {
            // To prevent deadlock(apparently the read lock to INTERNER stays alive after getting interned string)
            let interned = INTERNER
                .get()
                .unwrap()
                .write()
                .unwrap()
                .intern($s.to_string());
            interned
        }
    };
}
