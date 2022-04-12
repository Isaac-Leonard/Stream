use std::ops::Range;

use im_rc::Vector;

use crate::ast::*;

/// return (need_to_continue_search, founded reference)
type Spanned<T> = (T, Range<usize>);
pub fn get_definition_of_expr(
    expr: &ExpEnvironment,
    definition_ass_list: Vector<Spanned<String>>,
    ident_offset: usize,
) -> (bool, Option<Spanned<String>>) {
    use CompExpression::*;
    match expr.expression.as_ref() {
        Read(local) => {
            if ident_offset >= expr.located.start && ident_offset < expr.located.end {
                let index = definition_ass_list
                    .iter()
                    .position(|decl| decl.0 == local.name);
                (false, index.map(|i| definition_ass_list[i].clone()))
            } else {
                (true, None)
            }
        }
        Assign(lhs, rhs) => {
            match get_definition_of_expr(lhs, definition_ass_list.clone(), ident_offset) {
                (true, None) => get_definition_of_expr(rhs, definition_ass_list, ident_offset),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
        }
        BinOp(op, lhs, rhs) => {
            match get_definition_of_expr(lhs, definition_ass_list.clone(), ident_offset) {
                (true, None) => {
                    get_definition_of_expr(rhs, definition_ass_list.clone(), ident_offset)
                }
                (false, None) => (false, None),
                (true, Some(value)) | (false, Some(value)) => (false, Some(value)),
            }
        }
        Call(callee, args) => {
            return (false, None);
            for expr in args {
                match get_definition_of_expr(&expr, definition_ass_list.clone(), ident_offset) {
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
            match get_definition_of_expr(cond, definition_ass_list.clone(), ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(then, definition_ass_list.clone(), ident_offset) {
                (true, None) => {}
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
            match get_definition_of_expr(otherwise, definition_ass_list, ident_offset) {
                (true, None) => return (true, None),
                (true, Some(value)) => return (false, Some(value)),
                (false, None) => return (false, None),
                (false, Some(value)) => return (false, Some(value)),
            }
        }
        List(lst) => {
            for expr in lst {
                match get_definition_of_expr(expr, definition_ass_list.clone(), ident_offset) {
                    (true, None) => continue,
                    (true, Some(value)) => return (false, Some(value)),
                    (false, None) => return (false, None),
                    (false, Some(value)) => return (false, Some(value)),
                }
            }
            (true, None)
        }
        _ => (true, None),
    }
}
