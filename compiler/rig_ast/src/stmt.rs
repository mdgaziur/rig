use rig_span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Stmt {
    kind: Box<StmtKind>,
    span: Span
}

#[derive(Debug, Clone, PartialEq)]
pub enum StmtKind {

}
