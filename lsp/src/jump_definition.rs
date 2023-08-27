use std::ops::Range;
use stream::ast::*;

/// return (need_to_continue_search, founded reference)
type Spanned<T> = (T, Range<usize>);
pub fn get_definition_of_expr(
	expr: &ExpEnvironment,

	ident_offset: usize,
) -> (bool, Option<(String, (String, Range<usize>))>) {
	use CompExpression::*;
	match expr.expression.as_ref() {
		Typeof(var) => get_definition_of_expr(var, ident_offset),
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
		BinOp(_, lhs, rhs) => match get_definition_of_expr(lhs, ident_offset) {
			(true, None) => get_definition_of_expr(rhs, ident_offset),
			(false, None) => (false, None),
			(true, Some(value)) | (false, Some(value)) => (false, Some(value)),
		},
		Call(callee, args) => {
			if let Some(loc) = &callee.declared_at {
				let call_end = args
					.get(0)
					.map(|x| x.located.start)
					.unwrap_or(expr.located.end);
				if ident_offset >= expr.located.start && ident_offset < call_end {
					return (false, Some((callee.name.clone(), loc.clone())));
				};
			}
			for expr in args {
				let res = get_definition_of_expr(expr, ident_offset);
				if res.1.is_some() {
					return res;
				}
			}
			(true, None)
		}
		IfElse(if_exp) => {
			match get_definition_of_expr(&if_exp.cond, ident_offset) {
				(true, None) => {}
				(true, Some(value)) => return (false, Some(value)),
				(false, None) => return (false, None),
				(false, Some(value)) => return (false, Some(value)),
			}
			match get_definition_of_expr(&if_exp.then, ident_offset) {
				(true, None) => {}
				(true, Some(value)) => return (false, Some(value)),
				(false, None) => return (false, None),
				(false, Some(value)) => return (false, Some(value)),
			}
			get_definition_of_expr(&if_exp.otherwise, ident_offset)
		}
		WhileLoop { cond, body } => match get_definition_of_expr(cond, ident_offset) {
			(_, None) => get_definition_of_expr(body, ident_offset),
			(_, Some(value)) => (false, Some(value)),
		},
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
