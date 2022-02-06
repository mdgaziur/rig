use crate::expr::Expr;
use crate::visibility::Visibility;
use std::fmt::{Debug, Formatter};

#[derive(Clone, PartialEq)]
pub struct Prototype {
    pub visibility: Visibility,
    pub name: String,
    pub args: Vec<Argument>,
    pub return_ty: Option<Expr>,
}

impl ToString for Prototype {
    fn to_string(&self) -> String {
        let args_string: Vec<String> = self.args.iter().map(|a| a.to_string()).collect();
        format!(
            "{} {}({}) -> {:?}",
            self.visibility.to_string(),
            &self.name,
            args_string.join(","),
            self.return_ty
        )
    }
}

impl Debug for Prototype {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(&self.to_string())
    }
}

#[derive(Clone, PartialEq)]
pub struct Argument {
    pub name: String,
    pub type_: Expr,
}

impl ToString for Argument {
    fn to_string(&self) -> String {
        format!("{}: {}", self.name, self.type_.to_string())
    }
}

impl Debug for Argument {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
