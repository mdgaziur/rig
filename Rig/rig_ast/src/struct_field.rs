use crate::expr::Expr;
use crate::visibility::Visibility;

#[derive(Debug, Clone, PartialEq)]
pub struct StructField {
    pub visibility: Visibility,
    pub name: String,
    pub ty: Expr,
}

impl ToString for StructField {
    fn to_string(&self) -> String {
        let mut vis = self.visibility.to_string();
        if self.visibility == Visibility::Pub {
            vis.push(' ');
        }

        format!("{}{}: {}", vis, self.name, self.ty.to_string())
    }
}