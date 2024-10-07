use chumsky::prelude::*;
use std::collections::HashMap;

fn doc_parser() -> impl Parser<char, HashMap<String, String>, Error = Simple<char>> {
    let find_start = take_until(just("## Functions"));

    let punctuation = one_of(r#"!$,_-./:;?+<=>#%&*@[\]{|}`^~"#);
    let letters = one_of("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
    let digit = one_of("0123456789");

    let symbol = choice((letters.clone(), punctuation.clone()))
        .then(choice((letters, punctuation, digit)).repeated())
        .map(|(start, end)| format!("{start}{}", end.iter().collect::<String>()));

    let name_parser = just("###")
        .padded()
        .ignore_then(symbol)
        .padded()
        .map(|name| name.trim().to_string());

    let body_parser = just('-')
        .then(take_until(just("```\n")))
        .padded()
        .map(|(dash, (start, end))| format!("{dash}{}\n{end}", start.iter().collect::<String>()));

    let section_parser = name_parser
        .then(body_parser)
        .padded()
        .map(|(name, body)| (name, body));

    let doc = find_start
        .ignore_then(section_parser.padded().repeated())
        .map(HashMap::from_iter);

    doc.padded()
        .then_ignore(take_until(end()))
        .then_ignore(end())
}

pub fn load_doc() -> HashMap<String, String> {
    let src = include_str!("../Docs.md");
    let ast = doc_parser().parse(src);
    match ast {
        Ok(ast) => ast,
        Err(err) => {
            println!("{err:?}");
            panic!("failed to parse docs");
        }
    }
}

#[test]
fn test_load_doc() {
    let doc = load_doc();
    assert!(doc.contains_key("fn"));
    assert!(doc.contains_key("lambda"));
    assert!(doc.contains_key("var"));
    assert!(doc.contains_key("+"));
    assert!(doc.contains_key("-"));
    assert!(doc.contains_key("="));
    assert!(doc.contains_key(">"));
    assert!(doc.contains_key("<"));
    assert!(doc.contains_key(">="));
    assert!(doc.contains_key("<="));
    assert!(doc.contains_key("and"));
    assert!(doc.contains_key("or"));
    assert!(doc.contains_key("not"));
    assert!(doc.contains_key("print"));
    assert!(doc.contains_key("typeof"));
    assert!(doc.contains_key("help"));
    assert!(doc.contains_key("list"));
    assert!(doc.contains_key("cons"));
    assert!(doc.contains_key("car"));
    assert!(doc.contains_key("cdr"));
    assert!(doc.contains_key("append"));
    assert!(doc.contains_key("reverse"));
    assert!(doc.contains_key("nth"));
    assert!(doc.contains_key("length"));
    assert!(doc.contains_key("map"));
    assert!(doc.contains_key("fold"));
    assert!(doc.contains_key("fold-right"));
    assert!(doc.contains_key("filter"));
    assert!(doc.contains_key("filter"));
    assert!(doc.contains_key("assert"));
    assert!(doc.contains_key("number?"));
}
