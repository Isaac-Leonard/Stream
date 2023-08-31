use std::ops::Range;
use stream::ast1::*;

use im_rc::Vector;

#[derive(Debug, Clone)]
pub enum ReferenceSymbol {
	Founded((String, Range<usize>)),
	Founding(usize),
}
use ReferenceSymbol::*;
pub fn get_reference(
	ast: &ExpEnvironment,
	ident_offset: usize,
	include_self: bool,
) -> Vec<(String, Range<usize>)> {
	let mut reference_list = vec![];
	// for (_, v) in ast.iter() {
	//     if v.name.1.end < ident_offset {
	//         vector.push_back(v.name.clone());
	//     }
	// }
	let mut kv_list = ast;
	let mut reference_symbol = ReferenceSymbol::Founding(ident_offset);
	// let mut fn_vector = Vector::new();
	reference_list
}

pub fn get_reference_of_expr(
	expr: &ExpEnvironment,
	definition_ass_list: Vector<(String, Range<usize>)>,
	reference_symbol: ReferenceSymbol,
	reference_list: &mut Vec<(String, Range<usize>)>,
	include_self: bool,
) {
	use CompExpression::*;
	match &expr.expression.as_ref() {
		Value(_) => {}
		Read(name) => {
			if let Founded((symbol_name, symbol_span)) = reference_symbol {
				if symbol_name == name.name {
					let index = definition_ass_list
						.iter()
						.position(|decl| decl.0 == symbol_name);
					if let Some(symbol) = index.map(|i| definition_ass_list.get(i).unwrap().clone())
					{
						if symbol == (symbol_name.to_string(), symbol_span) {
							reference_list.push((name.name.clone(), expr.located.clone()));
						}
					};
				}
			}
			// if ident_offset >= local.1.start && ident_offset < local.1.end {
			//     let index = definition_ass_list
			//         .iter()
			//         .position(|decl| decl.0 == local.0);
			//     (
			//         false,
			//         index.map(|i| definition_ass_list.get(i).unwrap().clone()),
			//     )
			// } else {
			//     (true, None)
			// }
		}
		BinOp(op, lhs, rhs) => {
			get_reference_of_expr(
				lhs,
				definition_ass_list.clone(),
				reference_symbol.clone(),
				reference_list,
				include_self,
			);
			get_reference_of_expr(
				rhs,
				definition_ass_list.clone(),
				reference_symbol,
				reference_list,
				include_self,
			);
		}
		Call(callee, _, args) => {}
		IfElse(if_exp) => {
			get_reference_of_expr(
				&if_exp.cond,
				definition_ass_list.clone(),
				reference_symbol.clone(),
				reference_list,
				include_self,
			);
			get_reference_of_expr(
				&if_exp.then,
				definition_ass_list.clone(),
				reference_symbol.clone(),
				reference_list,
				include_self,
			);
			get_reference_of_expr(
				&if_exp.otherwise,
				definition_ass_list,
				reference_symbol.clone(),
				reference_list,
				include_self,
			);
		}
		List(lst) => {
			for expr in lst {
				get_reference_of_expr(
					expr,
					definition_ass_list.clone(),
					reference_symbol.clone(),
					reference_list,
					include_self,
				);
			}
		}
		_ => {}
	}
}
