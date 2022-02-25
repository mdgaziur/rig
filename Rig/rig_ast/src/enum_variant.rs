use crate::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Option<Vec<EnumVariantField>>
}

impl EnumVariant {
    pub fn to_string(&self, depth: usize) -> String {
        if let Some(fields) = &self.fields {
            let mut res = vec![format!("{}{} {{", "\t".repeat(depth), self.name)];

            res.push(fields.iter()
                .map(|f| format!("{}{}", "\t".repeat(depth + 1), f.to_string()))
                .collect::<Vec<String>>()
                .join(","));

            res.push("\t".repeat(depth) + "}");
            res.join("\n")
        } else {
            "\t".repeat(depth) + &self.name
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVariantField {
    pub name: String,
    pub ty: Expr,
}

impl ToString for EnumVariantField {
    fn to_string(&self) -> String {
        format!("{}: {}", self.name, self.ty.to_string(0))
    }
}