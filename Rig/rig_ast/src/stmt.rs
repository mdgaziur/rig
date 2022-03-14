use crate::expr::Expr;
use crate::function_prototype::Prototype;
use crate::struct_field::StructField;
use crate::visibility::Visibility;
use rig_span::Span;
use crate::enum_variant::EnumVariant;
use crate::match_arms::MatchArm;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    UseStmt {
        path: Expr,
        visibility: Visibility,
        span: Span,
    },
    StructStmt {
        name: String,
        fields: Vec<StructField>,
        visibility: Visibility,
        span: Span,
    },
    ImplStmt {
        struct_name: String,
        methods: Vec<Box<Stmt>>,
        span: Span,
    },
    ExternStmt {
        prototypes: Vec<Prototype>,
        span: Span,
    },
    FnStmt {
        prototype: Prototype,
        body: Box<Stmt>,
        visibility: Visibility,
        span: Span,
    },
    LetStmt {
        name: String,
        ty: Option<Expr>,
        value: Expr,
        visibility: Visibility,
        span: Span,
    },
    IfStmt {
        condition: Expr,
        body: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
    WhileStmt {
        condition: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    ForStmt {
        var: String,
        iterable: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    PrintStmt {
        expr: Expr,
        span: Span,
    },
    ReturnStmt {
        expr: Expr,
        span: Span,
    },
    BlockStmt {
        exprs: Vec<Box<Stmt>>,
        span: Span,
    },
    ExprStmt {
        expr: Expr,
        span: Span,
    },
    BreakStmt {
        span: Span,
    },
    ContinueStmt {
        span: Span,
    },
    ModStmt {
        name: String,
        body: Option<Vec<Stmt>>,
        visibility: Visibility,
        span: Span,
    },
    EnumStmt {
        name: String,
        variants: Vec<EnumVariant>,
        visibility: Visibility,
        span: Span,
    },
    MatchStmt {
        matched: Expr,
        arms: Vec<MatchArm>,
        span: Span,
    },
}

impl Stmt {
    pub fn to_string(&self, block_depth: usize) -> String {
        let res = match self {
            Stmt::UseStmt {
                path, visibility, ..
            } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                format!("{}use {};", vis, path.to_string(block_depth))
            }
            Stmt::StructStmt {
                visibility,
                name,
                fields,
                ..
            } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                let mut res = format!("{}struct {} {{", vis, name);
                if !fields.is_empty() {
                    res.push('\n');
                }

                for field in fields {
                    res += &format!("{}{},\n", "\t".repeat(block_depth + 1), field.to_string());
                }

                res += &format!("{}}}", "\t".repeat(block_depth));
                res
            }
            Stmt::ImplStmt {
                struct_name,
                methods,
                ..
            } => {
                let mut stringified_methods = methods
                    .iter()
                    .map(|m| "\t".repeat(block_depth + 1) + &m.to_string(block_depth + 1))
                    .collect::<Vec<String>>()
                    .join("\n");
                if !stringified_methods.is_empty() {
                    stringified_methods.push('\n');
                }

                format!(
                    "impl {} {{\n{}{}}}\n",
                    struct_name,
                    stringified_methods,
                    "\t".repeat(block_depth)
                )
            }
            Stmt::FnStmt {
                prototype, body, ..
            } => format!("{} {}", prototype.to_string(), body.to_string(block_depth)),
            Stmt::LetStmt {
                name,
                value,
                ty,
                visibility,
                ..
            } => {
                let mut vis = visibility.to_string();
                if !vis.is_empty() {
                    vis.push(' ');
                }

                let type_ = match ty {
                    Some(t) => format!(": {}", t.to_string(block_depth)),
                    None => String::new(),
                };

                format!(
                    "{}let {}{} = {};",
                    vis,
                    name,
                    type_,
                    value.to_string(block_depth)
                )
            }
            Stmt::IfStmt {
                condition,
                body,
                else_branch,
                ..
            } => {
                let top = format!(
                    "if {} {}",
                    condition.to_string(block_depth),
                    body.to_string(block_depth)
                );

                if let Some(else_branch) = else_branch {
                    format!("{} else {}", top, else_branch.to_string(block_depth))
                } else {
                    top
                }
            }
            Stmt::WhileStmt {
                condition, body, ..
            } => {
                format!(
                    "while {} {}",
                    condition.to_string(block_depth),
                    body.to_string(block_depth),
                )
            }
            Stmt::ForStmt {
                var,
                iterable,
                body,
                ..
            } => {
                format!(
                    "for {} in {} {}",
                    var,
                    iterable.to_string(block_depth),
                    body.to_string(block_depth)
                )
            }
            Stmt::PrintStmt { expr, .. } => format!("print {};", expr.to_string(block_depth)),
            Stmt::BlockStmt { exprs, .. } => {
                let mut res = vec![String::from("{")];

                for expr in exprs {
                    res.push(format!(
                        "{}{}",
                        "\t".repeat(block_depth + 1),
                        expr.to_string(block_depth + 1)
                    ));
                }

                res.push(format!("{}}}", "\t".repeat(block_depth)));
                res.join("\n")
            }
            Stmt::ExprStmt { expr, .. } => format!("{};", expr.to_string(block_depth)),
            Stmt::BreakStmt { .. } => String::from("break;"),
            Stmt::ContinueStmt { .. } => String::from("continue;"),
            Stmt::ReturnStmt { expr, .. } => format!("return {};", expr.to_string(block_depth)),
            Stmt::ExternStmt { prototypes, .. } => {
                let stringified_prototypes = prototypes
                    .iter()
                    .map(|p| format!("{}{};", "\t".repeat(block_depth + 1), p.to_string()))
                    .collect::<Vec<String>>()
                    .join(";\n");

                let newline = if !stringified_prototypes.is_empty() {
                    "\n"
                } else {
                    ""
                };
                format!(
                    "extern {{{}{}{}{}}}",
                    newline,
                    stringified_prototypes,
                    newline,
                    "\t".repeat(block_depth)
                )
            }
            Stmt::ModStmt { name, body, .. } => {
                let mut res = vec![format!("mod {}", name)];

                if let Some(body) = body {
                    res[0] += " {";

                    for stmt in body {
                        res.push(format!(
                            "{}{}",
                            "\t".repeat(block_depth + 1),
                            stmt.to_string(block_depth + 1)
                        ));
                    }

                    res.push(format!("{}}}", "\t".repeat(block_depth)));
                } else {
                    res.push(";".to_string());
                }

                res.join("\n")
            }
            Stmt::EnumStmt { visibility, name, variants, .. } => {
                let mut res = vec![format!("{}enum {} {{", match visibility {
                    Visibility::Pub => "pub ",
                    Visibility::NotPub => "",
                }, name)];

                res.push(variants.iter()
                    .map(|v| v.to_string(block_depth + 1))
                    .collect::<Vec<String>>()
                    .join(",\n"));

                res.push("\t".repeat(block_depth) + "}");
                res.join("\n")
            },
            Stmt::MatchStmt { matched, arms, .. } => {
                let mut res = vec![format!("match {} {{", matched.to_string(0))];

                res.extend(arms.iter()
                    .map(|arm| arm.to_string(block_depth + 1))
                    .collect::<Vec<String>>());

                res.push("\t".repeat(block_depth) + "}");
                res.join("\n")
            }
        };

        res
    }

    pub fn node_name(&self) -> &str {
        match self {
            Stmt::UseStmt { .. } => "use",
            Stmt::StructStmt { .. } => "struct",
            Stmt::ImplStmt { .. } => "impl",
            Stmt::ExternStmt { .. } => "extern",
            Stmt::FnStmt { .. } => "fn",
            Stmt::LetStmt { .. } => "let",
            Stmt::IfStmt { .. } => "if",
            Stmt::WhileStmt { .. } => "while",
            Stmt::ForStmt { .. } => "for",
            Stmt::PrintStmt { .. } => "print",
            Stmt::ReturnStmt { .. } => "return",
            Stmt::BlockStmt { .. } => "block",
            Stmt::ExprStmt { .. } => "expression",
            Stmt::BreakStmt { .. } => "break",
            Stmt::ContinueStmt { .. } => "continue",
            Stmt::ModStmt { .. } => "mod",
            Stmt::EnumStmt { .. } => "enum",
            Stmt::MatchStmt { .. } => "match",
        }
    }

    pub fn get_span(&self) -> Span {
        // TODO: not proud of this weird way to get span
        match self {
            Stmt::UseStmt { span, .. } => span,
            Stmt::StructStmt { span, .. } => span,
            Stmt::ImplStmt { span, .. } => span,
            Stmt::ExternStmt { span, .. } => span,
            Stmt::FnStmt { span, .. } => span,
            Stmt::LetStmt { span, .. } => span,
            Stmt::IfStmt { span, .. } => span,
            Stmt::WhileStmt { span, .. } => span,
            Stmt::ForStmt { span, .. } => span,
            Stmt::PrintStmt { span, .. } => span,
            Stmt::ReturnStmt { span, .. } => span,
            Stmt::BlockStmt { span, .. } => span,
            Stmt::ExprStmt { span, .. } => span,
            Stmt::BreakStmt { span, .. } => span,
            Stmt::ContinueStmt { span, .. } => span,
            Stmt::ModStmt { span, .. } => span,
            Stmt::EnumStmt { span, .. } => span,
            Stmt::MatchStmt { span, .. } => span,
        }
        .clone()
    }
}
