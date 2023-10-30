mod ast;
mod atoms;
mod debug;
mod infer;
mod parse;

use ast::Atom;
use infer::Denotation;

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
    let program = match maybe_program {
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
    // program.preprocess();
    // println!("PREPROCESSED: {:#?}", program);

    for (ridx, rule) in program.rules.iter().enumerate() {
        // if rule.wildcards_in_neg_antecedents() {
        //     println!("ERROR: rule #{ridx:?}: {rule:?} has wildcard in neg antecedents",);
        //     return;
        // }
        let mut buf = std::collections::HashSet::default();
        rule.unbound_variables(&mut buf);
        if !buf.is_empty() {
            println!("ERROR: rule #{ridx:?}: {rule:?} has unbound vars {buf:?}",);
            return;
        }
    }
    // program.static_reflect_simpler();
    // Rule::enforce_subconsequence(&mut rules);
    // Rule::enforce_says(&mut rules);
    // Rule::enforce_subconsequence(&mut rules);
    // println!("PROGRAM AFTER REFLECTION: {:#?}", program);

    for rule in &program.rules {
        println!("rule {:?}", rule);
        for atom in &rule.pos_antecedents {
            println!("pos_antecedent {:?}", atom.vars_to_wildcards());
        }
    }

    if let Err(counter_example) = program.termination_test(10) {
        println!("Termination test failed by {counter_example:?}");
        return;
    }

    let Denotation { trues, prev_trues } = program.alternating_fixpoint();
    fn vecify<'a>(x: impl IntoIterator<Item = &'a Atom>) -> Vec<&'a Atom> {
        let mut vec: Vec<_> = x.into_iter().collect();
        vec.sort();
        vec
    }
    println!("TRUES: {:#?}", vecify(trues.as_slice()));
    println!("PREV TRUES: {:#?}", vecify(prev_trues.as_slice()));
}
