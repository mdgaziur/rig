use rig_ast::expr::Expr;
use rig_ast::op::{BinaryOperator, UnaryOperator};
use rig_ast::visibility::Visibility;
use rig_error::{ErrorCode, ErrorType, RigError};
use rig_project::Project;
use rig_span::Span;
use rig_types::builtins::{BOOL_TYPEID, FLOAT_TYPEID, INT_TYPEID, NULL_TYPEID, STRING_TYPEID};
use rig_types::checked_expr::{
    CheckedBinary, CheckedBoolean, CheckedCall, CheckedExpr, CheckedFloat, CheckedInteger,
    CheckedNull, CheckedPath, CheckedString, CheckedUnary, CheckedVariable,
};
use rig_types::{ModuleId, ScopeId, TypeIdOrModuleId};
use rig_utils::bug;

pub fn check_expr(
    project: &Project,
    module_id: ModuleId,
    scope_id: ScopeId,
    expr: &Expr,
) -> (Option<CheckedExpr>, Vec<RigError>) {
    match expr {
        Expr::FloatLiteralExpr { value, span } => (
            Some(CheckedExpr::Float(CheckedFloat {
                value: *value,
                span: span.clone(),
                ty: FLOAT_TYPEID,
            })),
            vec![],
        ),
        Expr::IntegerLiteralExpr { value, span } => (
            Some(CheckedExpr::Int(CheckedInteger {
                value: *value,
                span: span.clone(),
                ty: INT_TYPEID,
            })),
            vec![],
        ),
        Expr::StringLiteralExpr { value, span } => (
            Some(CheckedExpr::String(CheckedString {
                value: value.clone(),
                span: span.clone(),
                ty: STRING_TYPEID,
            })),
            vec![],
        ),
        Expr::BooleanLiteralExpr { value, span } => (
            Some(CheckedExpr::Boolean(CheckedBoolean {
                value: *value,
                span: span.clone(),
                ty: BOOL_TYPEID,
            })),
            vec![],
        ),
        Expr::NullLiteralExpr { span } => (
            Some(CheckedExpr::Null(CheckedNull {
                ty: NULL_TYPEID,
                span: span.clone(),
            })),
            vec![],
        ),
        Expr::UnaryExpr { op, rhs, span } => {
            check_unary(project, module_id, scope_id, op, rhs, span)
        }
        Expr::CallExpr { name, args, span } => {
            check_call(project, module_id, scope_id, name, args, span)
        }
        Expr::VariableExpr { name, span } => {
            check_variable(project, module_id, scope_id, name, span)
        }
        Expr::PathExpr { path, span } => check_path(project, module_id, path, span),
        Expr::BinaryExpr { lhs, op, rhs, span } => {
            check_binary(project, module_id, scope_id, lhs, *op, rhs, span)
        }
        _ => todo!("{:?}", expr),
    }
}

fn check_binary(
    project: &Project,
    module_id: ModuleId,
    scope_id: ScopeId,
    lhs: &Expr,
    op: BinaryOperator,
    rhs: &Expr,
    span: &Span,
) -> (Option<CheckedExpr>, Vec<RigError>) {
    let mut errors = vec![];
    let (lhs_expr, lhs_errs) = check_expr(project, module_id, scope_id, lhs);
    let (rhs_expr, rhs_errs) = check_expr(project, module_id, scope_id, rhs);
    errors.extend(lhs_errs);
    errors.extend(rhs_errs);

    if !errors.is_empty() {
        return (None, errors);
    }

    let lhs_expr = lhs_expr.unwrap();
    let rhs_expr = rhs_expr.unwrap();

    if op == BinaryOperator::Plus {
        if !can_add(&lhs_expr, &rhs_expr) {
            return (
                None,
                vec![RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0022,
                    &format!(
                        "cannot add `{}` and `{}`",
                        lhs_expr.ty().typeid_to_string(&project.modules),
                        rhs_expr.ty().typeid_to_string(&project.modules)
                    ),
                    span.clone(),
                )],
            );
        }
        return (
            Some(CheckedExpr::Binary(CheckedBinary {
                op,
                ty: lhs_expr.ty(),
                lhs: Box::new(lhs_expr),
                rhs: Box::new(rhs_expr),
                span: span.clone(),
            })),
            vec![],
        );
    }

    todo!()
}

fn check_path(
    project: &Project,
    module_id: ModuleId,
    path: &[String],
    span: &Span,
) -> (Option<CheckedExpr>, Vec<RigError>) {
    let module = project.get_module(module_id);
    match module.try_resolve(&project.modules, path) {
        Ok(ty) => {
            if ty.get_visibility() == Visibility::NotPub && ty.get_moduleid() != module_id {
                return (
                    None,
                    vec![RigError::with_no_hint_and_notes(
                        ErrorType::Hard,
                        ErrorCode::E0010,
                        "Cannot access private type",
                        span.clone(),
                    )],
                );
            }

            let ty = match ty.get_typeid() {
                Some(ty) => ty,
                None => {
                    return (
                        None,
                        vec![RigError::with_no_hint_and_notes(
                            ErrorType::Hard,
                            ErrorCode::E0014,
                            &format!("Path `{}` is not a type", path.join("::")),
                            span.clone(),
                        )],
                    )
                }
            };
            (
                Some(CheckedExpr::Path(CheckedPath {
                    path: Vec::from(path.clone()),
                    ty,
                    span: span.clone(),
                })),
                vec![],
            )
        }
        Err(e) => {
            return (
                None,
                vec![RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0013,
                    &e.to_string(),
                    span.clone(),
                )],
            )
        }
    }
}

fn check_variable(
    project: &Project,
    module_id: ModuleId,
    scope_id: ScopeId,
    name: &str,
    span: &Span,
) -> (Option<CheckedExpr>, Vec<RigError>) {
    let module = project.get_module(module_id);
    module.get_scope(scope_id).variables.get(name).map_or_else(
        || {
            module
                .try_resolve(&project.modules, &[name.to_string()])
                .map_or_else(
                    |e| {
                        (
                            None,
                            vec![RigError::with_no_hint_and_notes(
                                ErrorType::Hard,
                                ErrorCode::E0012,
                                &format!("Type `{}` not found: {e}", name),
                                span.clone(),
                            )],
                        )
                    },
                    |ty| {
                        let ty = match ty.get_typeid() {
                            Some(ty) => ty,
                            None => {
                                return (
                                    None,
                                    vec![RigError::with_no_hint_and_notes(
                                        ErrorType::Hard,
                                        ErrorCode::E0014,
                                        &format!("Path `{}` is not a type", name),
                                        span.clone(),
                                    )],
                                )
                            }
                        };
                        (
                            Some(CheckedExpr::Path(CheckedPath {
                                path: vec![name.to_string()],
                                ty,
                                span: span.clone(),
                            })),
                            vec![],
                        )
                    },
                )
        },
        |ty| {
            let checked_expr = CheckedExpr::Variable(CheckedVariable {
                ty: *ty,
                name: name.to_string(),
                span: span.clone(),
            });
            (Some(checked_expr), vec![])
        },
    )
}

fn check_unary(
    project: &Project,
    module_id: ModuleId,
    scope_id: ScopeId,
    op: &UnaryOperator,
    expr: &Expr,
    span: &Span,
) -> (Option<CheckedExpr>, Vec<RigError>) {
    let (checked_expr, mut errors) = check_expr(project, module_id, scope_id, expr);

    if let None = checked_expr {
        return (None, errors);
    }
    let checked_expr = checked_expr.unwrap();

    match op {
        UnaryOperator::Negate => {
            if checked_expr.ty() != FLOAT_TYPEID && checked_expr.ty() != INT_TYPEID {
                errors.push(RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0017,
                    &format!(
                        "Cannot apply unary operator `-` to type `{}`",
                        checked_expr.ty().typeid_to_string(&project.modules)
                    ),
                    span.clone(),
                ));

                return (None, errors);
            }
        }
        UnaryOperator::Not => {
            if checked_expr.ty() != BOOL_TYPEID {
                errors.push(RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0017,
                    &format!(
                        "Cannot apply unary operator `!` to type `{}`",
                        checked_expr.ty().typeid_to_string(&project.modules)
                    ),
                    span.clone(),
                ));

                return (None, errors);
            }
        }
    }

    (
        Some(CheckedExpr::Unary(CheckedUnary {
            op: *op,
            ty: checked_expr.ty(),
            rhs: Box::new(checked_expr),
            span: expr.get_span(),
        })),
        errors,
    )
}

fn check_call(
    project: &Project,
    module_id: ModuleId,
    scope_id: ScopeId,
    name: &Expr,
    args: &[Expr],
    span: &Span,
) -> (Option<CheckedExpr>, Vec<RigError>) {
    let module = project.get_module(module_id);
    let path = match check_expr(project, module_id, scope_id, name) {
        (Some(e), _) => e,
        (None, errors) => return (None, errors),
    };
    let (name, name_span) = match &path {
        CheckedExpr::Path(p) => (p.path.clone(), p.span.clone()),
        CheckedExpr::Variable(v) => (vec![v.name.clone()], v.span.clone()),
        e => bug!(e, "Expected path or checked variable"),
    };

    let fn_ = match module.try_resolve(&project.modules, &name) {
        Ok(ty) => match ty {
            TypeIdOrModuleId::TypeId(typeid, vis) => {
                if typeid.get_module_id() != module_id && vis == Visibility::NotPub {
                    return (
                        None,
                        vec![RigError::with_no_hint_and_notes(
                            ErrorType::Hard,
                            ErrorCode::E0018,
                            &format!("Cannot access private type `{}`", name.join("::"),),
                            name_span.clone(),
                        )],
                    );
                } else {
                    typeid
                }
            }
            TypeIdOrModuleId::ModuleId(_, ..) => {
                return (
                    None,
                    vec![RigError::with_no_hint_and_notes(
                        ErrorType::Hard,
                        ErrorCode::E0018,
                        &format!("`{}` is not a function", name.join("::")),
                        name_span.clone(),
                    )],
                );
            }
        },
        Err(e) => {
            return (
                None,
                vec![RigError::with_no_hint_and_notes(
                    ErrorType::Hard,
                    ErrorCode::E0018,
                    &format!("Cannot resolve function `{}`: {}", name.join("::"), e),
                    name_span.clone(),
                )],
            );
        }
    };

    let checked_fn = &module.functions[fn_.1];
    if checked_fn.args.len() != args.len() {
        return (
            None,
            vec![RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0019,
                &format!(
                    "Function `{}` expected {} arguments, but got {}",
                    name.join("::"),
                    checked_fn.args.len(),
                    args.len()
                ),
                span.clone(),
            )],
        );
    }

    let mut errors = vec![];
    let mut checked_args = vec![];
    for idx in 0..args.len() {
        let arg_ty = match check_expr(project, module_id, scope_id, &args[idx]) {
            (Some(arg_ty), errs) => {
                errors.extend(errs);
                arg_ty
            }
            (None, errs) => {
                errors.extend(errs);
                return (None, errors);
            }
        };

        if arg_ty.ty() != checked_fn.args[idx].ty {
            errors.push(RigError::with_no_hint_and_notes(
                ErrorType::Hard,
                ErrorCode::E0020,
                &format!(
                    "Function `{}` expected argument of type `{}`, but got `{}`",
                    name.join("::"),
                    checked_fn.args[idx].ty.typeid_to_string(&project.modules),
                    arg_ty.ty().typeid_to_string(&project.modules),
                ),
                args[idx].get_span(),
            ));
        }

        checked_args.push(Box::new(arg_ty));
    }

    (
        Some(CheckedExpr::Call(CheckedCall {
            ty: checked_fn.return_ty.unwrap_or(NULL_TYPEID),
            span: span.clone(),
            name: Box::new(path),
            args: checked_args,
        })),
        errors,
    )
}

fn can_add(lhs: &CheckedExpr, rhs: &CheckedExpr) -> bool {
    match (lhs.ty(), rhs.ty()) {
        (INT_TYPEID, INT_TYPEID) => true,
        (FLOAT_TYPEID, FLOAT_TYPEID) => true,
        (STRING_TYPEID, STRING_TYPEID) => true,
        (FLOAT_TYPEID, INT_TYPEID) => true,
        (INT_TYPEID, FLOAT_TYPEID) => true,
        _ => false,
    }
}
