use chumsky::prelude::*;
use std::collections::HashMap;

fn doc_parser() -> impl Parser<char, HashMap<String, String>, Error = Simple<char>> {
    let find_start = take_until(just("## Functions"));

    let op = |c| just(c).padded();

    let name_parser = just("###")
        .padded()
        .ignore_then(just("**").padded())
        .ignore_then(
            text::ident()
                .or(op(">=").map(|o| o.to_string()))
                .or(op("<=").map(|o| o.to_string()))
                .or(op("+").map(|o| o.to_string()))
                .or(op("-").map(|o| o.to_string()))
                .or(op("=").map(|o| o.to_string()))
                .or(op(">").map(|o| o.to_string()))
                .or(op("<").map(|o| o.to_string())),
        )
        .then_ignore(just("**"))
        .padded();

    let body_parser = just(':')
        .padded()
        .ignore_then(take_until(just("```\n")))
        .padded()
        .map(|(start, end)| format!("{}\n{end}", start.iter().collect::<String>()));

    let section_parser = name_parser
        .then(body_parser)
        .padded()
        .map(|(name, body)| (name, body));

    let doc = find_start
        .ignore_then(section_parser.padded().repeated())
        .map(|sections| HashMap::from_iter(sections.into_iter()));

    doc.padded()
        .then_ignore(take_until(end()))
        .then_ignore(end())
}

pub fn load_doc() -> HashMap<String, String> {
    let mut path = std::env::current_dir().unwrap();
    path.push("Docs.md");

    let src = std::fs::read_to_string(path).unwrap();
    let ast = doc_parser().parse(src);
    match ast {
        Ok(ast) => return ast,
        Err(err) => {
            println!("{err:?}");
            panic!("failed to parse docs");
        }
    }
}
