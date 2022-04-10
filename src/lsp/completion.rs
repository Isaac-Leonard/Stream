use crate::ast::*;
use std::collections::HashMap;
pub enum ImCompleteCompletionItem {
    Variable(String),
    Function(String, Vec<String>),
}
/// return (need_to_continue_search, founded reference)
pub fn completion(
    ast: &crate::ExpEnvironment,
    ident_offset: usize,
) -> HashMap<String, ImCompleteCompletionItem> {
    let mut map = HashMap::new();
    map
}

pub fn get_completion_of(
    expr: &ExpEnvironment,
    definition_map: &mut HashMap<String, ImCompleteCompletionItem>,
    ident_offset: usize,
) -> bool {
    use CompExpression::*;
    match expr.expression.as_ref() {
        Value(_) => true,
        // List(exprs) => exprs
        //     .iter()
        //     .for_each(|expr| get_definition(expr, definition_ass_list)),
        Read(local) => {
            if ident_offset >= expr.located.start && ident_offset < expr.located.end {
                false
            } else {
                true
            }
        }
        Assign(name, rhs) => {
            if let Read(name) = name.expression.as_ref() {
                definition_map.insert(
                    name.name.clone(),
                    ImCompleteCompletionItem::Variable(name.name.clone()),
                );
            };
            get_completion_of(&rhs, definition_map, ident_offset)
        }
        BinOp(op, lhs, rhs) => match get_completion_of(&lhs, definition_map, ident_offset) {
            true => get_completion_of(&rhs, definition_map, ident_offset),
            false => false,
        },
        Call(callee, args) => {
            definition_map.insert(
                callee.name.clone(),
                ImCompleteCompletionItem::Variable(callee.name.clone()),
            );
            for expr in args {
                match get_completion_of(&expr, definition_map, ident_offset) {
                    true => continue,
                    false => return false,
                }
            }
            true
        }
        IfElse {
            cond,
            then,
            otherwise,
        } => {
            match get_completion_of(&cond, definition_map, ident_offset) {
                true => {}
                false => return false,
            }
            match get_completion_of(&then, definition_map, ident_offset) {
                true => {}
                false => return false,
            }
            get_completion_of(&otherwise, definition_map, ident_offset)
        }
        List(lst) => {
            for expr in lst {
                match get_completion_of(&expr, definition_map, ident_offset) {
                    true => continue,
                    false => return false,
                }
            }
            true
        }
        _ => false,
    }
}
