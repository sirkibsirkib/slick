use std::collections::HashSet;

mod ast;
mod debug;
// mod externalize;
mod infer;
// mod internalize;
// mod ir;
mod parse;
mod preprocess;

use ast::{Atom, Constant, Rule};

fn stdin_to_vecu8() -> Vec<u8> {
    let mut stdin = std::io::stdin().lock();
    let mut buffer = vec![];
    std::io::Read::read_to_end(&mut stdin, &mut buffer).expect("buffer overflow");
    buffer
}

fn stdin_to_string() -> String {
    use std::io::Read as _;
    let mut buffer = String::new();
    std::io::stdin()
        .lock()
        .read_to_string(&mut buffer)
        .expect("overflow?");
    buffer
}

fn main() {
    let mut source = stdin_to_string();
    preprocess::remove_comments(&mut source);
    let rules = nom::combinator::all_consuming(parse::wsr(parse::rules))(&source);
    let mut rules = match rules {
        Err(nom::Err::Error(e)) => {
            return println!("{}", nom::error::convert_error(source.as_str(), e.clone()));
        }
        Err(e) => return println!("PARSE ERROR {e:#?}"),
        Ok((rest, rules)) => {
            println!("REST {rest:?}");
            rules
        }
    };
    println!("RULES: {:#?}", rules);

    for (ridx, rule) in rules.iter().enumerate() {
        if rule.wildcards_in_consequents() {
            println!("ERROR: rule #{ridx:?}: {rule:?} has wildcard in consequents",);
            return;
        }
        let mut buf = std::collections::HashSet::default();
        rule.unbound_variables(&mut buf);
        if !buf.is_empty() {
            println!("ERROR: rule #{ridx:?}: {rule:?} has unbound vars {buf:?}",);
            return;
        }
    }
    Rule::static_reflect(&mut rules);
    println!("RULES: {:#?}", rules);
    // Rule::enforce_subconsequence(&mut rules);
    // let rep =
    //     |name: &'static [u8], n: usize| std::iter::repeat_with(|| Constant(name.to_vec())).take(n);
    // let sayers = rep(b"amy", 3)
    //     .chain(rep(b"bob", 4))
    //     .chain(rep(b"cho", 2))
    //     .chain(rep(b"dan", 2));
    // Rule::enforce_says(rules.iter_mut().zip(sayers));
    // for rule in &rules {
    //     println!("{rule:?}");
    // }

    {
        let alt: Vec<_> = rules.iter().map(Rule::without_neg_antecedents).collect();
        // println!("TESTING RULES: {:#?}", alt);
        let res = infer::Atoms::termination_test(&alt, 10);
        if let Err(counter_example) = res {
            println!("Termination test violated by {counter_example:?}");
            return;
        }
    }

    let [t, u] = infer::Atoms::alternating_fixpoint(&rules);
    let vecify = |x: HashSet<Atom>| {
        let mut vec: Vec<_> = x.into_iter().collect();
        vec.sort();
        vec
    };
    let [t, u] = [vecify(t), vecify(u)];
    let p = |atoms: &[Atom]| {
        for atom in atoms {
            println!("{atom:?}");
        }
    };
    println!("\nTRUE:");
    p(&t);

    println!("\nUNKNOWN:");
    p(&u);
}
