use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::prelude::*;
use std::{env, fs};
use tiger::parse::lexer::lexer;
use tiger::parse::parser::exp_parser;
use tiger::semant::escape::find_escape;
use tiger::semant::trans_exp;
use tiger::trans::mips_frame::MipsFrame;

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = fs::read_to_string(&filename).expect("Failed to read file");

    let (tokens, err) = lexer().parse(&src).into_output_errors();
    let (program, parse_errs) = if let Some(tokens) = &tokens {
        exp_parser()
            .map_with(|program, e| (program, e.span()))
            .parse(
                tokens
                    .as_slice()
                    .map((src.len()..src.len()).into(), |(t, s)| (t, s)),
            )
            .into_output_errors()
    } else {
        (None, Vec::new())
    };
    err.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|c| c.to_string())),
        )
        .for_each(|e| {
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
        });

    let exp = program.unwrap().0;
    find_escape(&exp.0);
    let ir = trans_exp::<MipsFrame>(exp);
    if let Ok(ir) = ir {
        dbg!(ir);
    } else if let Err(e) = ir {
        Report::build(ReportKind::Error, (filename.clone(), e.span().into_range()))
            .with_config(ariadne::Config::new().with_index_type(ariadne::IndexType::Byte))
            .with_message("Error while typechecking")
            .with_label(
                Label::new((filename.clone(), e.span().into_range()))
                    .with_message(format!("{:?}", e.kind()))
                    .with_color(Color::Red),
            )
            .finish()
            .print(sources([(filename.clone(), src.clone())]))
            .unwrap()
    }
}
