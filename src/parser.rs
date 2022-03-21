use crate::ast::*;
use chumsky::{
    error::Cheap,
    prelude::*,
    recursive::Recursive,
    text::{ident, keyword, whitespace},
};

fn integer() -> impl Parser<char, i32, Error = Cheap<char>> {
    filter::<_, _, Cheap<char>>(char::is_ascii_digit)
        .repeated()
        .at_least(1)
        .collect::<String>()
        .map(parse_to_i32)
}

fn float() -> impl Parser<char, f32, Error = Cheap<char>> {
    (filter::<_, _, Cheap<char>>(char::is_ascii_digit)
        .repeated()
        .at_least(1)
        .collect::<String>()
        .then_ignore(just('.'))
        .then(
            filter::<_, _, Cheap<char>>(char::is_ascii_digit)
                .repeated()
                .at_least(1)
                .collect::<String>(),
        ))
    .map(|x| format!("{}.{}", x.0, x.1))
    .map(parse_to_f32)
}

fn parse_to_i32(x: String) -> i32 {
    x.parse::<i32>().unwrap()
}

fn parse_to_f32(x: String) -> f32 {
    x.parse::<f32>().unwrap()
}

fn string() -> impl Parser<char, String, Error = Cheap<char>> {
    let escape = just('\\').ignore_then(
        just('\\')
            .or(just('/'))
            .or(just('"'))
            .or(just('b').to('\x08'))
            .or(just('f').to('\x0C'))
            .or(just('n').to('\n'))
            .or(just('r').to('\r'))
            .or(just('t').to('\t'))
            .or(just('0').to('\0')),
    );

    just('"')
        .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
        .then_ignore(just('"'))
        .collect::<String>()
}

fn symbol_parser() -> impl Parser<char, Symbol, Error = Cheap<char>> {
    string()
        .map(RawData::Str)
        .or(float().map(RawData::Float))
        .or(integer().map(RawData::Int))
        .or(keyword("null").to(RawData::Null))
        .or(keyword("true").to(RawData::Bool(true)))
        .or(keyword("false").to(RawData::Bool(false)))
        .map(Symbol::Data)
        .or(ident().map(String::from).map(Symbol::Identifier))
        .labelled("Symbol")
}

fn type_parser() -> impl Parser<char, CustomType, Error = Cheap<char>> {
    recursive(|ty: Recursive<char, CustomType, _>| {
        let singular = ident()
            .map(String::from)
            .then_ignore(whitespace())
            .then(
                (ty.clone().padded())
                    .separated_by(just(','))
                    .at_least(1)
                    .delimited_by('<', '>')
                    .or_not(),
            )
            .map(|x| CustomType::Lone(UseType::complex(x.0, x.1.unwrap_or_default())))
            .boxed();
        let callible = (ty.clone().padded().separated_by(just(',')))
            .delimited_by('(', ')')
            .then(just(':').padded().ignore_then(ty.clone()))
            .map(|x| CustomType::Callible(x.0, Box::new(x.1)));
        let union = (singular.clone().or(callible.clone()))
            .separated_by(just('|').padded())
            .at_least(2)
            .map(CustomType::Union);
        let array = ty
            .clone()
            .map(Box::new)
            .then_ignore(just(";").padded())
            .then(integer())
            .delimited_by('[', ']')
            .map(|x| CustomType::Array(x.0, x.1));

        let struct_parser = ident()
            .map(String::from)
            .then_ignore(just(":").padded())
            .then(ty.clone())
            .separated_by(just(";").padded())
            .allow_trailing()
            .padded()
            .delimited_by('{', '}')
            .map(CustomType::Struct);
        (callible)
            .or(union)
            .or(singular)
            .or(array)
            .or(struct_parser)
    })
}

fn op_parser(op: Op) -> impl Parser<char, Op, Error = Cheap<char>> + 'static {
    just(op.get_str()).to(op)
}

fn exp_parser<'a>() -> impl Parser<char, Expression, Error = Cheap<char>> + 'a {
    use Expression::*;
    recursive(|exp: Recursive<'_, char, Expression, Cheap<char>>| {
        let block_exp = exp
            .clone()
            .padded()
            .repeated()
            .padded()
            .delimited_by('{', '}')
            .map_with_span(Expression::Block)
            .then_ignore(whitespace().then(just(';')).or_not())
            .labelled("Block");
        let array_parser = exp
            .clone()
            .padded()
            .separated_by(just(','))
            .delimited_by('[', ']')
            .map_with_span(Array);

        let func_declaration = ident()
            .padded()
            .separated_by(just(','))
            .at_least(1)
            .delimited_by('<', '>')
            .or_not()
            .map(Option::unwrap_or_default)
            .then_ignore(whitespace())
            .then(
                ident()
                    .map(String::from)
                    .padded()
                    .then(just(':').then(whitespace()).ignore_then(type_parser()))
                    .then_ignore(whitespace())
                    .separated_by(just(','))
                    .delimited_by('(', ')')
                    .then_ignore(whitespace())
                    .then(just(':').ignore_then(type_parser().padded()).or_not())
                    .then(
                        (just("=>").then(whitespace()))
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

        let func_call = ident()
            .map(String::from)
            .then_ignore(whitespace())
            .then(
                exp.clone()
                    .padded()
                    .separated_by(just(','))
                    .delimited_by('(', ')'),
            )
            .map_with_span(|(name, args), span| FuncCall(name, args, span))
            .labelled("Call");

        let struct_exp = ident()
            .map(String::from)
            .then_ignore(just(":").padded())
            .then(exp.clone())
            .separated_by(just(",").padded())
            .allow_trailing()
            .padded()
            .delimited_by('{', '}')
            .map_with_span(Struct);

        let primary_exp = func_call
            .or(symbol_parser().map_with_span(Terminal))
            .or(exp.clone().padded().delimited_by('(', ')'))
            .boxed()
            .labelled("Primary");

        let typeof_check = keyword("typeof")
            .then(whitespace())
            .ignore_then(primary_exp.clone().map(Box::new))
            .map_with_span(Typeof)
            .boxed();
        let index_parser = primary_exp
            .clone()
            .then_ignore(whitespace())
            .then(primary_exp.clone().padded().delimited_by('[', ']'))
            .map_with_span(|x, span| Index(Box::new(x.0), Box::new(x.1), span));

        let primary_exp = index_parser.clone().or(primary_exp.clone());
        let primary_exp = typeof_check.clone().or(primary_exp.clone());

        let mult_parser = (primary_exp.clone())
            .then(
                op_parser(Op::Mult)
                    .or(op_parser(Op::Div))
                    .padded()
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
            .then(op_parser(Op::Le).or(op_parser(Op::Ge)).padded())
            .then(mult_parser.clone().map(Box::new))
            .map_with_span(|x, span| Expression::BinOp(x.0 .1, x.0 .0, x.1, span))
            .boxed();

        let equal_parser = (compare_parser.clone().or(mult_parser.clone()).map(Box::new))
            .then(op_parser(Op::Eq).or(op_parser(Op::Neq)).padded())
            .then(compare_parser.clone().or(mult_parser.clone()).map(Box::new))
            .map_with_span(|x, span| Expression::BinOp(x.0 .1, x.0 .0, x.1, span));

        let add_sub_parser = (mult_parser.clone())
            .then(
                (op_parser(Op::Add).or(op_parser(Op::Sub)))
                    .padded()
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

        let if_parser = keyword("if")
            .ignore_then(exp.clone().map(Box::new).padded())
            .then(block_exp.clone().map(Box::new))
            .then_ignore(keyword("else").padded())
            .then(block_exp.clone().map(Box::new))
            .map_with_span(|x, span| IfElse(x.0 .0, x.0 .1, x.1, span));
        let loop_parser = just("while")
            .ignore_then(exp.clone().padded())
            .then(block_exp.clone())
            .map_with_span(|x, span| Loop(Box::new(x.0), Box::new(x.1), span));

        let type_declaration = keyword("type")
            .then_ignore(whitespace())
            .ignore_then(ident().map(String::from))
            .then_ignore(just('=').padded())
            .then(type_parser())
            .labelled("Type assignment")
            .map_with_span(|x, r| TypeDeclaration(x.0, x.1, r))
            .boxed();

        let reassign = index_parser
            .clone()
            .or(ident()
                .map(String::from)
                .map(Symbol::Identifier)
                .map_with_span(Terminal))
            .map(Box::new)
            .then_ignore(just('=').padded())
            .then(exp.clone().map(Box::new))
            .map_with_span(|x, span| Assign(x.0, x.1, span));

        let is_external = just("extern").or_not().map(|x| x.is_some());

        let declaration = is_external
            .then(
                keyword("let")
                    .to(false)
                    .or(keyword("const").to(true))
                    .padded(),
            )
            .then(ident().map(String::from))
            .then(just(':').padded().ignore_then(type_parser()).or_not())
            .then_ignore(just('=').padded())
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
            .then_ignore(whitespace().then(just(';')).or_not())
            .boxed();
        let expression = (expression.clone().map(Box::new))
            .then_ignore(just(".").padded())
            .then(ident().map(String::from))
            .map_with_span(|exp, range| DotAccess(exp.0, exp.1, range))
            .or(expression);
        expression.or(block_exp)
    })
}

pub fn parser() -> impl Parser<char, (Vec<ImportFrom>, Expression), Error = Cheap<char>> {
    let as_name = just("as")
        .then(whitespace())
        .ignore_then(ident())
        .or_not()
        .boxed();
    let imports = just("from")
        .ignore_then(string().padded())
        .then_ignore(just("import"))
        .then_ignore(whitespace())
        .then(
            (just('*').ignore_then(as_name.clone().padded()))
                .map(Import::All)
                .or((ident().then_ignore(whitespace()).then(as_name.clone()))
                    .separated_by(just(',').padded())
                    .padded()
                    .delimited_by('{', '}')
                    .padded()
                    .map(Import::Specific)),
        )
        .then_ignore(just(';').or_not())
        .map(|(file, imports)| ImportFrom { file, imports })
        .padded()
        .repeated();
    imports
        .then(
            exp_parser()
                .padded()
                .repeated()
                .map_with_span(Expression::Block),
        )
        .then_ignore(end())
}
