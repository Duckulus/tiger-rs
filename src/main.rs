use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use std::{env, fs};
use tiger::lexer::lexer;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let (tokens, err) = lexer().parse(&src).into_output_errors();
    if !err.is_empty() {
        err.into_iter()
            .map(|e| e.map_token(|c| c.to_string()))
            .for_each(|e| {
                dbg!(e.span());
                Report::build(ReportKind::Error, (filename.clone(), e.span().into_range()))
                    .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
                    .with_message(e.to_string())
                    .with_label(
                        Label::new((filename.clone(), e.span().into_range()))
                            .with_message(e.reason().to_string())
                            .with_color(Color::Red),
                    )
                    .with_labels(e.contexts().map(|(label, span)| {
                        Label::new((filename.clone(), span.into_range()))
                            .with_message(format!("while parsing this {}", label))
                            .with_color(Color::Yellow)
                    }))
                    .finish()
                    .print(sources([(filename.clone(), src.clone())]))
                    .unwrap()
            })
    }
    dbg!(tokens.unwrap());
}
