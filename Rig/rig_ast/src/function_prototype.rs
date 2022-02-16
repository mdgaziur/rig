use crate::expr::Expr;
use crate::visibility::Visibility;
use std::fmt::{Debug, Formatter};

#[derive(Clone, PartialEq)]
pub struct Prototype {
    pub visibility: Visibility,
    pub name: String,
    pub args: Vec<Argument>,
    pub return_ty: Option<Expr>,
    pub fn_type: FnType,
}

impl ToString for Prototype {
    fn to_string(&self) -> String {
        let mut args_string;
        if self.fn_type == FnType::Method {
            args_string = String::from("self, ");
        } else {
            args_string = String::new();
        }
        args_string = args_string + &self.args.iter().map(|a| a.to_string()).collect::<Vec<String>>()
            .join(", ");

        let ty_string;
        if let Some(ty) = &self.return_ty {
            ty_string = format!(" -> {}", ty.to_string(0));
        } else {
            ty_string = String::new();
        }

        format!(
            "{} {}({}){}",
            self.visibility.to_string(),
            &self.name,
            args_string,
            ty_string,
        )
    }
}

impl Debug for Prototype {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        f.write_str(&self.to_string())
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
