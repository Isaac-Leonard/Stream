use crate::ast::*;
use chumsky::{
    error::Cheap,
    prelude::*,
    recursive::Recursive,
    text::{ident, whitespace},
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
        .or(raw("null").to(RawData::Null))
        .or(raw("true").to(RawData::Bool(true)))
        .or(raw("false").to(RawData::Bool(false)))
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
        (callible).or(union).or(singular)
    })
}

fn raw(string: &str) -> impl Parser<char, (), Error = Cheap<char>> {
    seq(string.chars())
}

fn op_parser(op: Op) -> impl Parser<char, Op, Error = Cheap<char>> {
    raw(&op.get_str()).to(op)
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
                        (raw("=>").then(whitespace()))
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

        let primary_exp = func_call
            .or(symbol_parser().map_with_span(Terminal))
            .or(func_declaration.map_with_span(Terminal))
            .or(exp.clone().padded().delimited_by('(', ')'))
            .boxed()
            .labelled("Primary");

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

        let typeof_check = raw("typeof")
            .then(whitespace())
            .ignore_then(exp.clone().map(Box::new))
            .map_with_span(Typeof);

        let if_parser = raw("if")
            .ignore_then(exp.clone().map(Box::new).padded())
            .then(block_exp.clone().map(Box::new))
            .then_ignore(raw("else").padded())
            .then(block_exp.clone().map(Box::new))
            .map_with_span(|x, span| IfElse(x.0 .0, x.0 .1, x.1, span));
        let index_parser = primary_exp
            .clone()
            .then_ignore(whitespace())
            .then(primary_exp.clone().padded().delimited_by('[', ']'))
            .map_with_span(|x, span| Index(Box::new(x.0), Box::new(x.1), span));
        let loop_parser = raw("while")
            .ignore_then(exp.clone().padded())
            .then(block_exp.clone())
            .map_with_span(|x, span| Loop(Box::new(x.0), Box::new(x.1), span));

        let type_declaration = raw("type")
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

        let is_external = raw("extern").or_not().map(|x| x.is_some());

        let declaration = is_external
            .then(raw("let").to(false).or(raw("const").to(true)).padded())
            .then(ident().map(String::from))
            .then(just(':').padded().ignore_then(type_parser()).or_not())
            .then_ignore(just('=').padded())
            .then(exp.clone().map(Box::new))
            .map_with_span(|x, span| {
                InitAssign(x.0 .0 .0 .0, x.0 .0 .0 .1, x.0 .0 .1, x.0 .1, x.1, span)
            })
            .labelled("Declaration");

        let expression = (if_parser)
            .or(typeof_check)
            .or(loop_parser)
            .or(reassign)
            .or(type_declaration.clone())
            .or(declaration)
            .or(equal_parser)
            .or(compare_parser)
            .or(index_parser)
            .or(add_sub_parser)
            .or(array_parser)
            .then_ignore(whitespace().then(just(';')).or_not())
            .boxed();

        expression.or(block_exp)
    })
}

pub fn parser() -> impl Parser<char, (Vec<ImportFrom>, Expression), Error = Cheap<char>> {
    let as_name = raw("as")
        .then(whitespace())
        .ignore_then(ident())
        .or_not()
        .boxed();
    let imports = raw("from")
        .ignore_then(string().padded())
        .then_ignore(raw("import"))
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

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use super::parser;
    use crate::ast::*;
    #[test]
    fn add_expression() {
        use Expression::*;
        assert_eq!(
            parser().parse("5+2").unwrap(),
            [Addition(
                Box::new(Terminal(Symbol::Data(RawData::Int(5)), 0..1)),
                Box::new(Terminal(Symbol::Data(RawData::Int(2)), 2..3)),
                0..3
            )]
        );
    }

    #[test]
    fn add_mult_add_expression() {
        use Expression::*;
        assert_eq!(
            parser().parse("5+2*4+8").unwrap(),
            vec![Addition(
                Box::new(Addition(
                    Box::new(Terminal(Symbol::Data(RawData::Int(5)), 0..1)),
                    Box::new(Multiplication(
                        Box::new(Terminal(Symbol::Data(RawData::Int(2)), 2..3)),
                        Box::new(Terminal(Symbol::Data(RawData::Int(4)), 4..5)),
                        2..5
                    )),
                    0..5
                )),
                Box::new(Terminal(Symbol::Data(RawData::Int(8)), 6..7)),
                0..7
            )]
        );
    }

    #[test]
    fn empty_while_expression() {
        use Expression::*;
        assert_eq!(
            parser().parse("while true {}").unwrap(),
            vec![Loop(
                Box::new(Terminal(Symbol::Data(RawData::Bool(true)), 6..11)),
                Box::new(Block(Vec::new(), 11..13)),
                0..13
            )]
        );
    }

    #[test]
    fn empty_ifelse_expression() {
        use Expression::*;
        assert_eq!(
            parser().parse("if true {} else {}").unwrap(),
            vec![IfElse(
                Box::new(Terminal(Symbol::Data(RawData::Bool(true)), 3..8)),
                Box::new(Block(Vec::new(), 8..10)),
                Box::new(Block(Vec::new(), 16..18)),
                0..18
            )]
        );
    }
    #[test]
    fn empty_main_declaration() {
        use Expression::*;
        assert_eq!(
            parser().parse("let main=():Int=>{}").unwrap(),
            vec![InitAssign(
                false,
                false,
                "main".to_string(),
                None,
                Box::new(Terminal(
                    Symbol::Data(RawData::Func(Function {
                        args: Vec::new(),
                        body: Some(Box::new(Block(Vec::new(), 17..19))),
                        return_type: CustomType::Lone(UseType::simple("Int".to_string()))
                    })),
                    9..19
                )),
                0..19
            )]
        );
    }

    #[test]
    fn func_type_declaration() {
        use Expression::*;
        assert_eq!(
            parser().parse("type fn=(Int):Int"),
            Ok(vec![TypeDeclaration(
                "fn".to_string(),
                CustomType::Callible(
                    vec!(CustomType::Lone(UseType::simple("Int".to_string()))),
                    Box::new(CustomType::Lone(UseType::simple("Int".to_string())))
                ),
                0..17
            )])
        );
    }

    #[test]
    fn add_func_declaration() {
        use Expression::*;
        assert_eq!(
            parser().parse("let add=(x:Int, y:Int):Int=>{x+y}").unwrap(),
            vec![InitAssign(
                false,
                false,
                "add".to_string(),
                None,
                Box::new(Terminal(
                    Symbol::Data(RawData::Func(Function {
                        args: vec![
                            (
                                "x".to_string(),
                                CustomType::Lone(UseType::simple("Int".to_string()))
                            ),
                            (
                                "y".to_string(),
                                CustomType::Lone(UseType::simple("Int".to_string()))
                            )
                        ],
                        body: Some(Box::new(Block(
                            vec![Addition(
                                Box::new(Terminal(Symbol::Identifier("x".to_string()), 29..30)),
                                Box::new(Terminal(Symbol::Identifier("y".to_string()), 31..32)),
                                29..32
                            )],
                            28..33
                        ))),
                        return_type: CustomType::Lone(UseType::simple("Int".to_string()))
                    })),
                    8..33
                )),
                0..33
            )]
        );
    }

    #[test]
    fn extern_fn_declaration() {
        use Expression::*;
        assert_eq!(
            parser()
                .parse("extern let sin:Sin=(x:Float):Float")
                .unwrap(),
            vec![InitAssign(
                true,
                false,
                "sin".to_string(),
                Some(CustomType::Lone(UseType::simple("Sin".to_string()))),
                Box::new(Terminal(
                    Symbol::Data(RawData::Func(Function {
                        args: vec![(
                            "x".to_string(),
                            CustomType::Lone(UseType::simple("Float".to_string()))
                        )],
                        body: None,
                        return_type: CustomType::Lone(UseType::simple("Float".to_string()))
                    })),
                    19..34
                )),
                0..34
            )]
        );
    }

    #[test]
    fn extern_fn_declaration_with_semi_than_add_fn() {
        use Expression::*;
        assert_eq!(
            parser()
                .parse("extern let sin:Sin=(x:Float):Float;\nlet add=(x:Int, y:Int):Int=>{x+y}")
                .unwrap(),
            Block(
                InitAssign(
                    true,
                    false,
                    "sin".to_string(),
                    Some(CustomType::Lone(UseType::simple("Sin".to_string()))),
                    Box::new(Terminal(
                        Symbol::Data(RawData::Func(Function {
                            args: vec![(
                                "x".to_string(),
                                CustomType::Lone(UseType::simple("Float".to_string()))
                            )],
                            body: None,
                            return_type: CustomType::Lone(UseType::simple("Float".to_string()))
                        })),
                        19..34
                    )),
                    0..35
                ),
                InitAssign(
                    false,
                    false,
                    "add".to_string(),
                    None,
                    Box::new(Terminal(
                        Symbol::Data(RawData::Func(Function {
                            generics: Vec::new(),
                            args: vec![
                                (
                                    "x".to_string(),
                                    CustomType::Lone(UseType::simple("Int".to_string()))
                                ),
                                (
                                    "y".to_string(),
                                    CustomType::Lone(UseType::simple("Int".to_string()))
                                )
                            ],
                            body: Some(Box::new(Block(
                                vec![Addition(
                                    Box::new(Terminal(Symbol::Identifier("x".to_string()), 65..66)),
                                    Box::new(Terminal(Symbol::Identifier("y".to_string()), 67..68)),
                                    65..68
                                )],
                                64..69
                            ))),
                            return_type: CustomType::Lone(UseType::simple("Int".to_string()))
                        })),
                        44..69
                    )),
                    36..69
                )
            )
        );
    }

    #[test]
    fn extern_fn_declaration_with_semi_double_newline_than_add_fn() {
        use Expression::*;
        assert_eq!(
            parser()
                .parse("extern let sin:Sin=(x:Float):Float;\n\nlet add=(x:Int, y:Int):Int=>{x+y}")
                .unwrap()
                .1,
            Block(
                InitAssign(
                    true,
                    false,
                    "sin".to_string(),
                    Some(CustomType::Lone(UseType::simple("Sin".to_string()))),
                    Box::new(Terminal(
                        Symbol::Data(RawData::Func(Function {
                            args: vec![(
                                "x".to_string(),
                                CustomType::Lone(UseType::simple("Float".to_string()))
                            )],
                            body: None,
                            return_type: CustomType::Lone(UseType::simple("Float".to_string()))
                        })),
                        19..34
                    )),
                    0..35
                ),
                InitAssign(
                    false,
                    false,
                    "add".to_string(),
                    None,
                    Box::new(Terminal(
                        Symbol::Data(RawData::Func(Function {
                            generics: Vec::new(),
                            args: vec![
                                (
                                    "x".to_string(),
                                    CustomType::Lone(UseType::simple("Int".to_string()))
                                ),
                                (
                                    "y".to_string(),
                                    CustomType::Lone(UseType::simple("Int".to_string()))
                                )
                            ],
                            body: Some(Box::new(Block(
                                vec![Addition(
                                    Box::new(Terminal(Symbol::Identifier("x".to_string()), 66..67)),
                                    Box::new(Terminal(Symbol::Identifier("y".to_string()), 68..69)),
                                    66..69
                                )],
                                65..70
                            ))),
                            return_type: CustomType::Lone(UseType::simple("Int".to_string()))
                        })),
                        45..70
                    )),
                    37..70
                ),
                37..70
            )
        );
    }
}
