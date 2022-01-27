#[path = "shared.rs"]
mod shared;
pub mod parser {

    use crate::shared::*;
    use chumsky::{error::Cheap, prelude::*, recursive::Recursive, text::ident};
    fn parse_to_i32(x: String) -> i32 {
        return x.parse::<i32>().unwrap();
    }
    fn type_specifyer() -> impl Parser<char, Vec<String>, Error = Cheap<char>> {
        just(':')
            .ignore_then(
                ident().padded().map(String::from).chain::<String, _, _>(
                    just('|')
                        .ignore_then(ident().padded().map(String::from))
                        .repeated(),
                ),
            )
            .map(|x| x)
    }

    fn integer() -> impl Parser<char, i32, Error = Cheap<char>> {
        filter::<_, _, Cheap<char>>(char::is_ascii_digit)
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map(parse_to_i32)
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
                .or(just('t').to('\t')),
        );

        just('"')
            .ignore_then(filter(|c| *c != '\\' && *c != '"').or(escape).repeated())
            .then_ignore(just('"'))
            .collect::<String>()
    }

    fn symbol_parser() -> impl Parser<char, Symbol, Error = Cheap<char>> {
        ident()
            .map(String::from)
            .map(Symbol::Identifier)
            .or(string().map(RawData::Str).map(Symbol::Data))
            .or(integer().map(RawData::Int).map(Symbol::Data))
    }
    fn exp_parser<'a>(
        main_parser: Recursive<'a, char, Vec<Instr>, Cheap<char>>,
    ) -> impl Parser<char, Expression, Error = Cheap<char>> + 'a {
        use Expression::*;
        recursive(|exp| {
            let func_declaration = ident()
                .padded()
                .map(String::from)
                .then(type_specifyer())
                .chain(
                    just(',')
                        .ignore_then(ident().padded().map(String::from).then(type_specifyer()))
                        .repeated(),
                )
                .or_not()
                .flatten()
                .delimited_by('(', ')')
                .then(type_specifyer().padded().or_not())
                .then_ignore(seq(['=', '>']))
                .then(
                    main_parser
                        .clone()
                        .delimited_by('{', '}')
                        .or(exp.clone().map(Instr::LoneExpression).map(|x| vec![x])),
                )
                .map(|((args, ret), body)| {
                    Symbol::Data(RawData::Func(Function {
                        args,
                        body,
                        return_type: ret.unwrap_or_else(|| vec!["Null".to_string()]),
                    }))
                });

            let func_call = ident()
                .padded()
                .map(String::from)
                .then(
                    exp.clone()
                        .chain(just(',').ignore_then(exp.clone()).repeated())
                        .or_not()
                        .flatten()
                        .delimited_by('(', ')'),
                )
                .map(|(name, args)| FuncCall(name, args));

            let primary_exp = func_call
                .or(symbol_parser().map(Expression::Terminal))
                .or(func_declaration.map(Expression::Terminal))
                .or(exp.delimited_by('(', ')'))
                .boxed();

            let multiply_parser = primary_exp
                .clone()
                .then(one_of(['*', '/']).then(primary_exp.clone()).repeated())
                .map(|(l, t)| {
                    t.iter().fold(l, |left, (op, right)| match op {
                        '*' => Expression::Multiplication(Box::new(left), Box::new(right.clone())),
                        '/' => {
                            Expression::Division(Box::new(left.clone()), Box::new(right.clone()))
                        }
                        _ => panic!("Unexpected operator {}", op),
                    })
                })
                .boxed();
            let comparison_parser = multiply_parser
                .clone()
                .then_ignore(just('<'))
                .then(multiply_parser.clone())
                .map(|x| Expression::LessThan(Box::new(x.0), Box::new(x.1)));
            let equal_parser = comparison_parser
                .clone()
                .or(multiply_parser.clone())
                .then_ignore(seq(['=', '=']))
                .then(comparison_parser.clone().or(multiply_parser.clone()))
                .map(|x| Expression::Equal(Box::new(x.0), Box::new(x.1)));
            comparison_parser
                .or(equal_parser)
                .or(multiply_parser
                    .clone()
                    .then(one_of(['+', '-']).then(multiply_parser).repeated())
                    .map(|x| {
                        x.1.iter().fold(x.0, |left, right| match right.0 {
                            '+' => Expression::Addition(Box::new(left), Box::new(right.1.clone())),
                            '-' => {
                                Expression::Subtraction(Box::new(left), Box::new(right.1.clone()))
                            }
                            _ => panic!("Error: Unexpected operator {}", right.0),
                        })
                    }))
                .padded()
        })
    }

    pub fn type_parser() -> impl Parser<char, CustomType, Error = Cheap<char>> {
        recursive(|bf: Recursive<char, CustomType, _>| {
            ident()
                .padded()
                .map(String::from)
                .chain(
                    just('|')
                        .ignore_then(ident().padded().map(String::from))
                        .repeated(),
                )
                .map(|x| CustomType::Union(x))
                .or(bf
                    .clone()
                    .chain(just(',').ignore_then(bf.clone()).repeated())
                    .or_not()
                    .flatten()
                    .delimited_by('(', ')')
                    .then_ignore(just(':'))
                    .then(bf.clone())
                    .map(|x: (Vec<CustomType>, CustomType)| {
                        CustomType::Callible(x.0, Box::new(x.1))
                    }))
        })
    }

    pub fn parser() -> impl Parser<char, Vec<Instr>, Error = Cheap<char>> {
        use Instr::*;
        recursive(|bf: Recursive<char, Vec<Instr>, _>| {
            let exp = exp_parser(bf.clone()).boxed();
            seq("let".chars())
                .padded()
                .ignore_then(ident())
                .map(String::from)
                .then(type_specifyer().or_not())
                .then_ignore(just('='))
                .then(exp.clone())
                .map(|x| InitAssign(true, x.0 .0, x.0 .1, x.1))
                .or(ident()
                    .map(String::from)
                    .then(type_specifyer().or_not())
                    .then_ignore(just('='))
                    .then(exp.clone())
                    .map(|x| Assign(x.0 .0, x.1)))
                .or(seq("while".chars())
                    .ignore_then(exp.clone())
                    .then(bf.clone().delimited_by('{', '}'))
                    .map(|x| Loop(x.0, x.1)))
                .or(seq("if".chars())
                    .ignore_then(exp.clone())
                    .then(
                        bf.clone()
                            .delimited_by('{', '}')
                            .then_ignore(seq("else".chars()).padded())
                            .then(bf.clone().delimited_by('{', '}')),
                    )
                    .map(|x| IfElse(x.0, x.1 .0, x.1 .1)))
                .or(seq("type".chars())
                    .ignore_then(ident().padded().map(String::from))
                    .then_ignore(just('=').padded())
                    .then(type_parser())
                    .map(|x| TypeDeclaration(x.0, x.1)))
                .or(exp.map(LoneExpression).padded())
                .recover_with(nested_delimiters('{', '}', [], |_| {
                    Invalid("Syntax error".to_string())
                }))
                .recover_with(skip_then_retry_until(['}']))
                .padded()
                .repeated()
        })
        .then_ignore(end())
    }
}

#[cfg(test)]
mod tests {
    use chumsky::Parser;

    use super::parser::parser;
    use crate::shared::*;
    #[test]
    fn add_expression() {
        use Expression::*;
        assert_eq!(
            parser().parse("5+2").unwrap(),
            vec![Instr::LoneExpression(Addition(
                Box::new(Terminal(Symbol::Data(RawData::Int(5)))),
                Box::new(Terminal(Symbol::Data(RawData::Int(2))))
            ))]
        );
    }

    #[test]
    fn add_mult_add_expression() {
        use Expression::*;
        assert_eq!(
            parser().parse("5+2*4+8").unwrap(),
            vec![Instr::LoneExpression(Addition(
                Box::new(Addition(
                    Box::new(Terminal(Symbol::Data(RawData::Int(5)))),
                    Box::new(Multiplication(
                        Box::new(Terminal(Symbol::Data(RawData::Int(2)))),
                        Box::new(Terminal(Symbol::Data(RawData::Int(4))))
                    ))
                )),
                Box::new(Terminal(Symbol::Data(RawData::Int(8))))
            ))]
        );
    }
}
