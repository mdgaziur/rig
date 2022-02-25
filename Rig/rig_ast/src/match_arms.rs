use crate::expr::Expr;
use crate::stmt::Stmt;

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub match_: Expr,
    pub body: Stmt,
}

impl MatchArm {
    pub fn to_string(&self, depth: usize) -> String {
        format!("{}{} => {}", "\t".repeat(depth), self.match_.to_string(0), self.body.to_string(depth))
    }
}