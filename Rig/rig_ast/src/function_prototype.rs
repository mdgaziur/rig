use crate::expr::Expr;
use crate::visibility::Visibility;
use std::fmt::{Debug, Formatter};

#[derive(Clone)]
pub struct Prototype {
    pub visibility: Visibility,
    pub name: String,
    pub args: Vec<Argument>,
    pub return_ty: Option<Expr>,
}

impl ToString for Prototype {
    fn to_string(&self) -> String {
        format!(
            "{} {} ({:?})",
            self.visibility.to_string(),
            &self.name,
            self.args
        )
    }
}

impl Debug for Prototype {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

#[derive(Clone)]
pub struct Argument {
    pub name: String,
    pub type_: Expr,
}

impl ToString for Argument {
    fn to_string(&self) -> String {
        format!("{}: {:?}, ", self.name, self.type_)
    }
}

impl Debug for Argument {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
