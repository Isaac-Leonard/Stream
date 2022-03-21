use chumsky::{error::Cheap, prelude::*, text::ident};
#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Null,
    False,
    True,
    Str(String),
    If,
    Else,
    While,
    Ident(String),
    Int(i32),
    Colon,
    Terminator,
    Separator,
    Type,
    Typeof,
    Operator(String),
    Constant(bool),
    Exported,
    Float(f32),
    StartArray,
    EndArray,
    StartBlock,
    EndBlock,
    StartBracket,
    EndBracket,
}

fn integer() -> impl Parser<char, i32, Error = Cheap<char>> {
    text::int(10).map(parse_to_i32)
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

pub fn lexer() -> impl Parser<char, Vec<Token>, Error = Cheap<char>> {
    string()
        .map(Token::Str)
        .or(float().map(Token::Float))
        .or(integer().map(Token::Int))
        .or(ident().map(String::from).map(|x| match x.as_str() {
            "null" => Token::Null,
            "true" => Token::True,
            "false" => Token::False,
            "if" => Token::If,
            "else" => Token::Else,
            "type" => Token::Type,
            "typeof" => Token::Typeof,
            _ => Token::Ident(x),
        }))
        .or(one_of("+-*/=!<>.")
            .repeated()
            .at_least(1)
            .collect::<String>()
            .map(Token::Operator))
        .or(just(';').to(Token::Terminator))
        .or(just(',').to(Token::Separator))
        .or(just('(').to(Token::StartBracket))
        .or(just(')').to(Token::EndBracket))
        .or(just('[').to(Token::StartArray))
        .or(just(']').to(Token::EndArray))
        .or(just('{').to(Token::StartBlock))
        .or(just('}').to(Token::EndBlock))
        .padded()
        .repeated()
}
