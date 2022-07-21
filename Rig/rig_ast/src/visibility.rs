use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visibility {
    Pub,
    NotPub,
}

impl From<bool> for Visibility {
    fn from(visible: bool) -> Self {
        if visible {
            Self::Pub
        } else {
            Self::NotPub
        }
    }
}

impl ToString for Visibility {
    fn to_string(&self) -> String {
        match self {
            Self::Pub => String::from("pub"),
            Self::NotPub => String::new(),
        }
    }
}

impl FromStr for Visibility {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "pub" => Ok(Self::Pub),
            "" => Ok(Self::NotPub),
            _ => Err(()),
        }
    }
}
