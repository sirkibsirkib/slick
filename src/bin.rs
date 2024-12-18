use slick::{infer::Config, util::VecSet, *};
use std::{
    collections::HashSet,
    time::{Duration, Instant},
};

fn stdin_to_string() -> String {
    use std::io::Read as _;
    let mut buffer = String::new();
    std::io::stdin().lock().read_to_string(&mut buffer).expect("overflow?");
    buffer
}

fn timed<R>(func: impl FnOnce() -> R) -> (Duration, R) {
    let start = Instant::now();
    let r = func();
    (start.elapsed(), r)
}

fn main() {
    let config = &Config::default();

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
    // program.reflect_within(&GroundAtom::Constant(crate::text::Text::from_str("msg1")));
    println!("PREPROCESSED: {:#?}", program);

    let mut unbound_vars = HashSet::default();
    // let empty_tuple = GroundAtom::Tuple(vec![]);
    for rule in &program.rules {
        // let (prefix, within) = match &rule.rule_within {
        //     None => ("", &empty_tuple),
        //     Some(within) => (" within ", within),
        // };
        if rule.misplaced_wildcards() {
            println!("ERROR: misplaced wildcard in rule:\n`{rule:?}`");
            return;
        }
        rule.unbound_variables(&mut unbound_vars);
        if !unbound_vars.is_empty() {
            println!("ERROR: unbound variables {unbound_vars:?} in rule:\n`{rule:?}` ",);
            return;
        }
    }

    println!("RUN CONFIG: {config:#?}");

    // let (dur, termination_test_res) = timed(|| program.termination_test(config));
    // println!("Termination test took {dur:?}");
    // if let Err(err) = termination_test_res {
    //     return println!("Termination test error {err:?}");
    // }

    // run up to RUN_CONFIG.static_rounds of big_steps without rules with negative consequents.
    // if this infers `error` then we already know it will infer error!
    // let (dur, pseudo_static_error_test_res) = timed(|| program.pseudo_static_error_test(config));
    // println!("Pseudo-static error test took {dur:?}. Result is {pseudo_static_error_test_res:#?}");

    let pos_antecedent_patterns: VecSet<Pattern> = program
        .rules
        .iter()
        .flat_map(|rule| rule.rule_body.pos_antecedents.iter().map(Atom::as_pattern))
        .collect();
    println!("POS ANTECEDENT PATTERNS {pos_antecedent_patterns:#?}");

    let neg_antecedent_patterns: VecSet<Pattern> = program
        .rules
        .iter()
        .flat_map(|rule| rule.rule_body.neg_antecedents.iter().map(Atom::as_pattern))
        .collect();
    println!("NEG ANTECEDENT PATTERNS {neg_antecedent_patterns:#?}");

    let pos_antecedent_patterns: VecSet<Pattern> = program
        .rules
        .iter()
        .flat_map(|rule| rule.consequents.iter().map(Atom::as_pattern))
        .collect();
    println!("CONSEQUENT PATTERNS {pos_antecedent_patterns:#?}");

    // for rule in program.rules.iter() {
    //     for pos_antecedent in rule.rule_body.pos_antecedents.iter() {
    //         let mut count = 0;
    //         for patt in pos_antecedent_patterns.iter() {
    //             if pos_antecedent.subsumed_by(patt) {
    //                 count += 1;
    //                 println!("- PATT:{patt:?}");
    //             }
    //         }
    //         println!("each {count} subsumes: {pos_antecedent:?}\n");
    //     }
    // }

    println!("TEXT TABLE:");
    text::Text::print_text_table();

    let (dur, denotation_res) = timed(|| program.denotation(config));
    println!("Computing the denotation took {dur:?}");
    let denotation = match denotation_res {
        Err(err) => return println!("Alternating fixpoint error {err:?}"),
        Ok(denotation) => denotation,
    };

    let error_ga = GroundAtom::Constant(Constant::from_str("error"));
    let error_ga_result = denotation.test(&error_ga);

    println!("DENOTATION {denotation:#?}");
    println!("error? {:?}", error_ga_result);

    // println!("DENOTATION AFTER HIDING {:#?}", denotation.hide_unshown());
}
