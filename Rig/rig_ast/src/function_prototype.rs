use crate::expr::Expr;
use crate::visibility::Visibility;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone, PartialEq)]
pub struct Prototype {
    pub visibility: Visibility,
    pub name: String,
    pub args: Vec<Argument>,
    pub return_ty: Option<Expr>,
    pub fn_type: FnType,
}

impl ToString for Prototype {
    fn to_string(&self) -> String {
        let mut args_string = if self.fn_type == FnType::Method {
            String::from("self, ")
        } else {
            String::new()
        };

        args_string = args_string
            + &self
                .args
                .iter()
                .map(|a| a.to_string())
                .collect::<Vec<String>>()
                .join(", ");

        let ty_string = if let Some(ty) = &self.return_ty {
            format!(" -> {}", ty.to_string(0))
        } else {
            String::new()
        };

        format!(
            "{}{}fn {}({}){}",
            self.visibility.to_string(),
            if self.visibility == Visibility::Pub { " " } else { "" },
            &self.name,
            args_string,
            ty_string,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnType {
    Method,
    Fn,
}

#[derive(Clone, PartialEq)]
pub struct Argument {
    pub name: String,
    pub type_: Expr,
}

impl ToString for Argument {
    fn to_string(&self) -> String {
        format!("{}: {}", self.name, self.type_.to_string(0))
    }
}

impl Debug for Argument {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}
