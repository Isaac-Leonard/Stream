use stream::ast::*;

macro_rules! find_in_list {
	($list:expr,$pos:expr) => {
		for arg in $list {
			if let Some(ty) = hover(arg, $pos) {
				return Some(ty);
			}
		}
	};
}

pub fn hover(ast: &ExpEnvironment, pos: u32) -> Option<CompType> {
	use CompExpression::*;
	if !ast.located.contains(&(pos as usize)) {
		return None;
	};
	match ast.expression.as_ref() {
		Read(var) => return Some(var.typing.clone()),
		Call(var, args) => {
			find_in_list!(args, pos);
			return Some(var.typing.clone());
		}
		Array(elements) => find_in_list!(elements, pos),
		List(expressions) => find_in_list!(expressions, pos),
		Assign(lhs, rhs) => find_in_list!([lhs, rhs], pos),
		WhileLoop { cond, body } => find_in_list!([cond, body], pos),
		IfElse(if_exp) => find_in_list!([&if_exp.cond, &if_exp.then, &if_exp.otherwise], pos),
		Index(arr, index) => find_in_list!([arr, index], pos),
		BinOp(_, lhs, rhs) => find_in_list!([lhs, rhs], pos),
		OneOp(_, exp) => return hover(exp, pos),
		Typeof(exp) => return hover(exp, pos),
		DotAccess(obj, prop) => match prop.1.contains(&(pos as usize)) {
			true => return Some(ast.result_type.clone()),
			false => return hover(obj, pos),
		},
		Value(CompData::Func(func)) => return hover(&func.body.as_ref()?.body, pos),
		Struct(data) => {
			for (key, val) in data {
				if key.1.contains(&(pos as usize)) {
					return Some(val.result_type.clone());
				} else if val.located.contains(&(pos as usize)) {
					return hover(val, pos);
				}
			}
		}
		Value(_) => {}
	};
	None
}
