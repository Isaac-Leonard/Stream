use std::ops::Range;

use crate::ast::*;

/// return (need_to_continue_search, founded reference)
type Spanned<T> = (T, Range<usize>);
pub fn get_definition_of_expr(
    expr: &ExpEnvironment,

    ident_offset: usize,
) -> (bool, Option<Spanned<String>>) {
    use CompExpression::*;
    match expr.expression.as_ref() {
        Read(local) => {
            if ident_offset >= expr.located.start && ident_offset < expr.located.end {
                (
                    false,
                    local.declared_at.clone().map(|s| (local.name.clone(), s)),
                )
            } else {
                (true, None)
            }
        }
        Assign(lhs, rhs) => match get_definition_of_expr(lhs, ident_offset) {
            (_, Some(value)) => return (false, Some(value)),
            (_, None) => get_definition_of_expr(rhs, ident_offset),
        },
        BinOp(op, lhs, rhs) => match get_definition_of_expr(lhs, ident_offset) {
            (true, None) => get_definition_of_expr(rhs, ident_offset),
            (false, None) => (false, None),
            (true, Some(value)) | (false, Some(value)) => (false, Some(value)),
        },
        Call(callee, args) => {
            return (false, None);
            for expr in args {
                match get_definition_of_expr(&expr, ident_offset) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        }
        IfElse {
            cond,
            then,
            otherwise,
        } => {
            match get_definition_of_expr(cond, ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(then, ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(otherwise, ident_offset) {
                (true, None) => return (true, None),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
        }
        List(lst) => {
            for expr in lst {
                match get_definition_of_expr(expr, ident_offset) {
                    (_, None) => continue,
                    (_, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        }
        Index(arr, index) => match get_definition_of_expr(arr, ident_offset) {
            (_, Some(def)) => (false, Some(def)),
            _ => get_definition_of_expr(index, ident_offset),
        },
        Value(CompData::Func(ast)) => {
            if ast.body.is_some() {
                get_definition_of_expr(&ast.body.as_ref().unwrap().body, ident_offset)
            } else {
                (true, None)
            }
        }
        _ => (true, None),
    }
}
