// mod ground_atoms;

mod ast;
mod debug;
mod infer;
mod parse;
mod text;
mod util;

use crate::ast::GroundAtom;
use infer::Denotation;
use std::collections::HashSet;

// fn stdin_to_vecu8() -> Vec<u8> {
//     let mut stdin = std::io::stdin().lock();
//     let mut buffer = vec![];
//     std::io::Read::read_to_end(&mut stdin, &mut buffer).expect("buffer overflow");
//     buffer
// }

fn stdin_to_string() -> String {
    use std::io::Read as _;
    let mut buffer = String::new();
    std::io::stdin().lock().read_to_string(&mut buffer).expect("overflow?");
    buffer
}

fn main() {
    let source = stdin_to_string();
    let maybe_program = parse::ended(parse::program)(&source);
    let mut program = match maybe_program {
        Err(nom::Err::Error(e)) => {
            return println!("{}", nom::error::convert_error(source.as_str(), e.clone()));
        }
        Err(e) => return println!("PARSE ERROR {e:#?}"),
        Ok((rest, program)) => {
            println!("UNPARSED SUFFIX: {rest:?}");
            program
        }
    };
    println!("PROGRAM: {:#?}", program);
    program.preprocess();
    println!("PREPROCESSED: {:#?}", program);

    let mut unbound_vars = HashSet::default();
    for (ridx, rule) in program.rules.iter().enumerate() {
        if rule.wildcard_in_consequent() {
            println!("ERROR: rule #{ridx:?}: {rule:?} has misplaced wildcard",);
            return;
        }
        rule.unbound_variables(&mut unbound_vars);
        if !unbound_vars.is_empty() {
            println!("ERROR: rule #{ridx:?}: {rule:?} has unbound vars {unbound_vars:?}",);
            return;
        }
    }

    if let Err(counter_example) = program.termination_test(10) {
        println!("Termination test failed by {counter_example:?}");
        return;
    }

    let Denotation { trues, prev_trues } = program.alternating_fixpoint();
    fn vecify<'a>(x: impl IntoIterator<Item = &'a GroundAtom>) -> Vec<&'a GroundAtom> {
        let mut vec: Vec<_> = x.into_iter().collect();
        vec.sort();
        vec
    }
    println!("TRUES: {:#?}", vecify(trues.vec_set.as_slice()));
    println!("PREV TRUES: {:#?}", vecify(prev_trues.vec_set.as_slice()));

    println!("TEXT TABLE:");
    text::Text::print_text_table()
}
