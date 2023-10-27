mod ast;
mod debug;
mod infer;
mod parse;
mod preprocess;

use crate::infer::Atoms;
use ast::{Atom, Rule};
use infer::Denotation;

fn stdin_to_vecu8() -> Vec<u8> {
    let mut stdin = std::io::stdin().lock();
    let mut buffer = vec![];
    std::io::Read::read_to_end(&mut stdin, &mut buffer).expect("buffer overflow");
    buffer
}

fn stdin_to_string() -> String {
    use std::io::Read as _;
    let mut buffer = String::new();
    std::io::stdin().lock().read_to_string(&mut buffer).expect("overflow?");
    buffer
}

fn main() {
    let mut source = stdin_to_string();
    preprocess::remove_comments(&mut source);
    // let source = Box::leak(Box::new(source));
    let program = nom::combinator::all_consuming(parse::wsr(parse::program))(&source);
    let mut rules = match program {
        Err(nom::Err::Error(e)) => {
            return println!("{}", nom::error::convert_error(source.as_str(), e.clone()));
        }
        Err(e) => return println!("PARSE ERROR {e:#?}"),
        Ok((rest, rules)) => {
            println!("UNPARSED SUFFIX: {rest:?}");
            rules
        }
    };
    // println!("RULES: {:#?}", rules);

    for (ridx, rule) in rules.iter().enumerate() {
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
    Rule::static_reflect_simpler(&mut rules);
    // Rule::enforce_subconsequence(&mut rules);
    // Rule::enforce_says(&mut rules);
    // Rule::enforce_subconsequence(&mut rules);
    println!("RULES: {:#?}", rules);

    for rule in &rules {
        println!("rule {:?}", rule);
        for atom in &rule.pos_antecedents {
            println!("pos_antecedent {:?}", atom.vars_to_wildcards());
        }
    }

    {
        if let Err(counter_example) = Atoms::termination_test(&rules, 10) {
            println!("Termination test failed by {counter_example:?}");
            return;
        }
    }

    let Denotation { trues, prev_trues } = infer::Atoms::alternating_fixpoint(rules);
    fn vecify<'a>(x: impl Iterator<Item = &'a Atom>) -> Vec<&'a Atom> {
        let mut vec: Vec<_> = x.collect();
        vec.sort();
        vec
    }
    println!("TRUES: {:#?}", vecify(trues.iter()));
    println!("PREV TRUES: {:#?}", vecify(prev_trues.iter()));
}
