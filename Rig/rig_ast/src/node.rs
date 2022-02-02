use std::fmt::{Debug, Display};

trait ASTNode: Debug + Display {
    fn accept<T, V: ASTNodeVisitor<T>>(&self, visitor: V) -> T;
}

trait ASTNodeVisitor<T> {
    // exprs
    fn visit_assignment_expr(expr: &AssignmentExpr) -> T;
    fn visit_binary_expr(expr: &BinaryExpr) -> T;
    fn visit_logical_expr(expr: &LogicalExpr) -> T;
    fn visit_unary_expr(expr: &UnaryExpr) -> T;
    fn visit_get_expr(expr: &GetExpr) -> T;
    fn visit_path_expr(expr: &PathExpr) -> T;

    // stmts
    fn visit_use_stmt(&self) -> T;
    fn visit_extern_stmt(&self) -> T;
    fn visit_struct_stmt(&self) -> T;
    fn visit_fn_stmt(&self) -> T;
    fn visit_var_decl_stmt(&self) -> T;
    fn visit_if_stmt(&self) -> T;
    fn visit_while_stmt(&self) -> T;
    fn visit_for_stmt(&self) -> T;
    fn visit_block_stmt(&self) -> T;
    fn visit_expr_stmt(&self) -> T;
    fn visit_print_stmt(&self) -> T;
}