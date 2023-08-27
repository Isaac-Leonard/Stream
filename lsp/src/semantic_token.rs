use std::collections::HashMap;
use stream::ast;
use tower_lsp::lsp_types::{SemanticToken, SemanticTokenType};

pub const LEGEND_TYPE: &[SemanticTokenType] = &[
	SemanticTokenType::FUNCTION,
	SemanticTokenType::VARIABLE,
	SemanticTokenType::STRING,
	SemanticTokenType::COMMENT,
	SemanticTokenType::NUMBER,
	SemanticTokenType::KEYWORD,
	SemanticTokenType::OPERATOR,
	SemanticTokenType::PARAMETER,
];
#[derive(Debug)]
pub struct ImCompleteSemanticToken {
	pub start: usize,
	pub length: usize,
	pub token_type: usize,
}

pub fn semantic_token_from_ast(ast: &ast::ExpEnvironment) -> Vec<ImCompleteSemanticToken> {
	let mut semantic_tokens = vec![];

	semantic_token_from_expr(ast, &mut semantic_tokens);
	semantic_tokens
}

pub fn semantic_token_from_expr(
	expr: &ast::ExpEnvironment,
	semantic_tokens: &mut Vec<ImCompleteSemanticToken>,
) {
	use ast::CompExpression::*;
	match expr.expression.as_ref() {
		Value(_) => {}
		List(_) => {}
		Read(var) => {
			semantic_tokens.push(ImCompleteSemanticToken {
				start: expr.located.start,
				length: expr.located.len(),
				token_type: LEGEND_TYPE
					.iter()
					.position(|item| item == &SemanticTokenType::VARIABLE)
					.unwrap(),
			});
		}
		Assign(lhs, rhs) => {
			semantic_tokens.push(ImCompleteSemanticToken {
				start: expr.located.start,
				length: expr.located.len(),
				token_type: LEGEND_TYPE
					.iter()
					.position(|item| item == &SemanticTokenType::VARIABLE)
					.unwrap(),
			});
			semantic_token_from_expr(rhs, semantic_tokens);
		}
		BinOp(op, lhs, rhs) => {
			semantic_token_from_expr(lhs, semantic_tokens);
			semantic_token_from_expr(rhs, semantic_tokens);
		}
		Call(var, params) => {
			semantic_tokens.push(ImCompleteSemanticToken {
				start: expr.located.start,
				length: expr.located.start
					- params
						.first()
						.map(|x| x.located.start)
						.unwrap_or(expr.located.end),
				token_type: LEGEND_TYPE
					.iter()
					.position(|item| item == &SemanticTokenType::VARIABLE)
					.unwrap(),
			});

			params.iter().for_each(|p| {
				semantic_token_from_expr(p, semantic_tokens);
			});
		}
		IfElse(if_exp) => {
			semantic_token_from_expr(&if_exp.cond, semantic_tokens);
			semantic_token_from_expr(&if_exp.then, semantic_tokens);
			semantic_token_from_expr(&if_exp.otherwise, semantic_tokens);
		}
		_ => {}
	}
}
