use crate::{ast::*, extract_or, lexer::Token};
use chumsky::{error::Cheap, prelude::*, recursive::Recursive};

fn symbol_parser() -> impl Parser<Token, Symbol, Error = Cheap<Token>> {
    filter_map(|e, x| match x {
        Token::Str(x) => Ok(RawData::Str(x)),
        Token::Float(x) => Ok(RawData::Float(x)),
        Token::Int(x) => Ok(RawData::Int(x)),
        Token::Null => Ok(RawData::Null),
        Token::True => Ok(RawData::Bool(true)),
        Token::False => Ok(RawData::Bool(false)),
        _ => Err(Cheap::expected_input_found(e, Vec::new(), None)),
    })
    .map(Symbol::Data)
    .or(token_ident().map(String::from).map(Symbol::Identifier))
    .labelled("Symbol")
}

fn type_parser() -> impl Parser<Token, CustomType, Error = Cheap<Token>> {
    recursive(|ty: Recursive<Token, CustomType, _>| {
        let singular = token_ident()
            .then(
                ty.clone()
                    .separated_by(separator())
                    .at_least(1)
                    .delimited_by(Token::Operator(format!("<")), Token::Operator(format!(">")))
                    .or_not(),
            )
            .map(|x| CustomType::Lone(UseType::complex(x.0, x.1.unwrap_or_default())))
            .boxed();
        let callible = (ty.clone().separated_by(separator()))
            .delimited_by(Token::StartBracket, Token::EndBracket)
            .then(just(Token::Colon).ignore_then(ty.clone()))
            .map(|x| CustomType::Callible(x.0, Box::new(x.1)))
            .boxed();
        let union = (singular.clone().or(callible.clone()))
            .separated_by(just(Token::Operator(format!("|"))))
            .at_least(2)
            .map(CustomType::Union);
        let array = ty
            .clone()
            .map(Box::new)
            .then_ignore(just(Token::Terminator))
            .then(filter_map(|e, x| {
                extract_or!(
                    x,
                    Token::Int,
                    Cheap::expected_input_found(e, Vec::new(), None)
                )
            }))
            .delimited_by(Token::StartArray, Token::EndArray)
            .map(|x| CustomType::Array(x.0, x.1));

        let struct_parser = token_ident()
            .then_ignore(just(Token::Colon))
            .then(ty.clone())
            .separated_by(just(Token::Terminator))
            .allow_trailing()
            .delimited_by(Token::StartBlock, Token::EndBlock)
            .map(CustomType::Struct);
        (callible)
            .or(union)
            .or(singular)
            .or(array)
            .or(struct_parser)
    })
}

fn op_parser(op: Op) -> impl Parser<Token, Op, Error = Cheap<Token>> + 'static {
    just(Token::Operator(op.get_str())).to(op)
}

fn token_ident() -> impl Parser<Token, String, Error = Cheap<Token>> {
    filter_map(|e, t| match t {
        Token::Ident(str) => Ok(str),
        _ => Err(Cheap::expected_input_found(e, Vec::new(), Some(t))),
    })
}

fn as_name() -> impl Parser<Token, Option<String>, Error = Cheap<Token>> {
    just(Token::As).ignore_then(token_ident()).or_not()
}

fn separator() -> impl Parser<Token, Token, Error = Cheap<Token>> {
    just(Token::Separator)
}

fn token_str() -> impl Parser<Token, String, Error = Cheap<Token>> {
    filter_map(|e, t| match t {
        Token::Str(str) => Ok(str),
        _ => Err(Cheap::expected_input_found(e, Vec::new(), Some(t))),
    })
}

fn exp_parser<'a>() -> impl Parser<Token, Expression, Error = Cheap<Token>> + 'a {
    use Expression::*;
    recursive(|exp: Recursive<'_, Token, Expression, Cheap<_>>| {
        let block_exp = exp
            .clone()
            .repeated()
            .delimited_by(Token::StartBlock, Token::EndBlock)
            .map_with_span(Expression::Block)
            .then_ignore(just(Token::Terminator).or_not())
            .labelled("Block");

        let array_parser = exp
            .clone()
            .separated_by(separator())
            .delimited_by(Token::StartArray, Token::EndArray)
            .map_with_span(Array);

        let func_declaration = token_ident()
            .separated_by(separator())
            .at_least(1)
            .delimited_by(
                Token::Operator(String::from("<")),
                Token::Operator(String::from(">")),
            )
            .or_not()
            .map(Option::unwrap_or_default)
            .then(
                token_ident()
                    .then(just(Token::Colon).ignore_then(type_parser()))
                    .separated_by(just(Token::Separator))
                    .delimited_by(Token::StartBracket, Token::EndBracket)
                    .then(just(Token::Colon).ignore_then(type_parser()).or_not())
                    .then(
                        just(Token::Operator(String::from("=>")))
                            .ignore_then(exp.clone().map(Box::new))
                            .or_not(),
                    ),
            )
            .map(|(generics, ((args, ret), body))| Function {
                generics,
                args,
                body,
                return_type: ret
                    .unwrap_or_else(|| CustomType::Lone(UseType::simple("Null".to_string()))),
            })
            .map(RawData::Func)
            .map(Symbol::Data)
            .labelled("Function");

        let func_call = token_ident()
            .then(
                exp.clone()
                    .separated_by(separator())
                    .delimited_by(Token::StartBracket, Token::EndBracket),
            )
            .map_with_span(|(name, args), span| FuncCall(name, args, span))
            .labelled("Call");

        let struct_exp = token_ident()
            .then_ignore(just(Token::Colon))
            .then(exp.clone())
            .separated_by(separator())
            .allow_trailing()
            .delimited_by(Token::StartBlock, Token::EndBlock)
            .map_with_span(Struct);

        let primary_exp = func_call
            .or(symbol_parser().map_with_span(Terminal))
            .or(exp
                .clone()
                .delimited_by(Token::StartBracket, Token::EndBracket))
            .boxed()
            .labelled("Primary");

        let typeof_check = just(Token::Typeof)
            .ignore_then(primary_exp.clone().map(Box::new))
            .map_with_span(Typeof)
            .boxed();
        let index_parser = primary_exp
            .clone()
            .then(
                primary_exp
                    .clone()
                    .delimited_by(Token::StartArray, Token::EndArray),
            )
            .map_with_span(|x, span| Index(Box::new(x.0), Box::new(x.1), span));

        let primary_exp = index_parser.clone().or(primary_exp.clone());
        let primary_exp = typeof_check.clone().or(primary_exp.clone());

        let mult_parser = (primary_exp.clone())
            .then(
                op_parser(Op::Mult)
                    .or(op_parser(Op::Div))
                    .then(primary_exp.clone().map(Box::new))
                    .repeated(),
            )
            .map(|(l, t)| {
                t.iter().fold(l, |left, (op, right)| {
                    Expression::BinOp(
                        op.clone(),
                        Box::new(left.clone()),
                        right.clone(),
                        left.get_range().start..right.get_range().end,
                    )
                })
            })
            .boxed();

        let compare_parser = (mult_parser.clone().map(Box::new))
            .then(op_parser(Op::Le).or(op_parser(Op::Ge)))
            .then(mult_parser.clone().map(Box::new))
            .map_with_span(|x, span| Expression::BinOp(x.0 .1, x.0 .0, x.1, span))
            .boxed();

        let equal_parser = (compare_parser.clone().or(mult_parser.clone()).map(Box::new))
            .then(op_parser(Op::Eq).or(op_parser(Op::Neq)))
            .then(compare_parser.clone().or(mult_parser.clone()).map(Box::new))
            .map_with_span(|x, span| Expression::BinOp(x.0 .1, x.0 .0, x.1, span));

        let add_sub_parser = (mult_parser.clone())
            .then(
                (op_parser(Op::Add).or(op_parser(Op::Sub)))
                    .then(mult_parser.clone().map(Box::new))
                    .repeated(),
            )
            .map(|x| {
                x.1.iter().fold(x.0, |left, (op, right)| {
                    BinOp(
                        op.clone(),
                        Box::new(left.clone()),
                        right.clone(),
                        left.get_range().start..right.get_range().end,
                    )
                })
            })
            .boxed();

        let if_parser = just(Token::If)
            .ignore_then(exp.clone().map(Box::new))
            .then(block_exp.clone().map(Box::new))
            .then_ignore(just(Token::Else))
            .then(block_exp.clone().map(Box::new))
            .map_with_span(|x, span| IfElse(x.0 .0, x.0 .1, x.1, span));
        let loop_parser = just(Token::While)
            .ignore_then(exp.clone())
            .then(block_exp.clone())
            .map_with_span(|x, span| Loop(Box::new(x.0), Box::new(x.1), span));

        let type_declaration = just(Token::Type)
            .ignore_then(token_ident())
            .then_ignore(just(Token::Operator(String::from("="))))
            .then(type_parser())
            .labelled("Type assignment")
            .map_with_span(|x, r| TypeDeclaration(x.0, x.1, r))
            .boxed();

        let reassign = index_parser
            .clone()
            .or(token_ident()
                .map(Symbol::Identifier)
                .map_with_span(Terminal))
            .map(Box::new)
            .then_ignore(just(Token::Operator(format!("="))))
            .then(exp.clone().map(Box::new))
            .map_with_span(|x, span| Assign(x.0, x.1, span));

        let is_external = just(Token::Exported).or_not().map(|x| x.is_some());

        let declaration = is_external
            .then((just(Token::Let).to(false)).or(just(Token::Constant).to(true)))
            .then(token_ident())
            .then(just(Token::Colon).ignore_then(type_parser()).or_not())
            .then_ignore(just(Token::Operator(format!("="))))
            .then(exp.clone().map(Box::new))
            .map_with_span(|x, span| {
                InitAssign(x.0 .0 .0 .0, x.0 .0 .0 .1, x.0 .0 .1, x.0 .1, x.1, span)
            })
            .labelled("Declaration");

        let expression = struct_exp
            .or(if_parser)
            .or(loop_parser)
            .or(reassign)
            .or(type_declaration.clone())
            .or(declaration)
            .or(equal_parser)
            .or(compare_parser)
            .or(func_declaration.map_with_span(Terminal))
            .or(add_sub_parser)
            .or(array_parser)
            .then_ignore(just(Token::Terminator).or_not())
            .boxed();
        let expression = (expression.clone().map(Box::new))
            .then_ignore(just(Token::Operator(format!("."))))
            .then(token_ident())
            .map_with_span(|exp, range| DotAccess(exp.0, exp.1, range))
            .or(expression);
        expression.or(block_exp)
    })
}

pub fn parser() -> impl Parser<Token, (Vec<ImportFrom>, Expression), Error = Cheap<Token>> {
    let imports = just(Token::From)
        .ignore_then(token_str())
        .then_ignore(just(Token::Import))
        .then(
            just(Token::Operator(String::from("*")))
                .ignore_then(as_name())
                .map(Import::All)
                .or(token_ident()
                    .then(as_name())
                    .separated_by(separator())
                    .delimited_by(Token::StartBlock, Token::EndBlock)
                    .map(Import::Specific)),
        )
        .then_ignore(just(Token::Terminator).or_not())
        .map(|(file, imports)| ImportFrom { file, imports })
        .repeated();
    imports
        .then(exp_parser().repeated().map_with_span(Expression::Block))
        .then_ignore(end())
}
