mod ast;
mod debug;
mod infer;
mod parse;
mod text;
mod util;

use std::collections::HashSet;
use std::time::{Duration, Instant};

fn stdin_to_string() -> String {
    use std::io::Read as _;
    let mut buffer = String::new();
    std::io::stdin().lock().read_to_string(&mut buffer).expect("overflow?");
    buffer
}

#[derive(Debug)]
struct RunConfig {
    max_alt_rounds: u16,
    max_atom_depth: u16,
    max_known_atoms: u32,
}
const RUN_CONFIG: RunConfig =
    RunConfig { max_alt_rounds: 8, max_atom_depth: 10, max_known_atoms: 30_000 };

fn timed<R>(func: impl FnOnce() -> R) -> (Duration, R) {
    let start = Instant::now();
    let r = func();
    (start.elapsed(), r)
}

fn main() {
    use ast::{Constant, GroundAtom};

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
    let empty_tuple = GroundAtom::Tuple(vec![]);
    for rule in &program.rules {
        let (by, whom) = match &rule.part_name {
            None => ("", &empty_tuple),
            Some(name) => (" by ", name),
        };
        if rule.misplaced_wildcards() {
            println!("ERROR: misplaced wildcard in rule{by}{whom:?}:\n`{rule:?}`");
            return;
        }
        rule.unbound_variables(&mut unbound_vars);
        // println!("rule {rule:?} unbound_vars {unbound_vars:?}",);
        if !unbound_vars.is_empty() {
            println!("ERROR: unbound variables {unbound_vars:?} in rule{by}{whom:?}:\n`{rule:?}` ",);
            return;
        }
    }

    println!("RUN CONFIG: {RUN_CONFIG:#?}");

    let (dur, termination_test_res) = timed(|| program.termination_test());
    println!("Termination test took {dur:?}");
    if let Err(err) = termination_test_res {
        return println!("Termination test error {err:?}");
    }

    let pos_antecedent_patterns = program.pos_antecedent_patterns();
    println!("POS ANTECEDENT PATTERNS {pos_antecedent_patterns:#?}");
    for rule in program.rules.iter() {
        for pos_antecedent in rule.pos_antecedents.iter() {
            let mut count = 0;
            for patt in pos_antecedent_patterns.iter() {
                if pos_antecedent.subsumed_by(patt) {
                    count += 1;
                    println!("- PATT:{patt:?}");
                }
            }
            println!("each {count} subsumes: {pos_antecedent:?}\n");
        }
    }

    println!("TEXT TABLE:");
    text::Text::print_text_table();

    let (dur, alternating_fixpoint_res) = timed(|| program.alternating_fixpoint());
    println!("Alternating fixpoint took {dur:?}");
    let raw_denotation = match alternating_fixpoint_res {
        Err(err) => return println!("Alternating fixpoint error {err:?}"),
        Ok(raw_denotation) => raw_denotation,
    };

    let error_ga = GroundAtom::Constant(Constant::from_str("error"));
    let error_ga_result = raw_denotation.test(&error_ga);
    let denotation = raw_denotation.to_denotation();

    println!("DENOTATION {denotation:#?}");
    println!("error? {:?}", error_ga_result);

    println!("DENOTATION AFTER HIDING {:#?}", denotation.not_shown_hidden());
}
