use std::collections::HashSet;

mod ast;
mod internalize;
mod ir;
mod parse;

fn main() {
    let x = b"A says X :- A likes B, B says X. A likes _ :- A.";
    let rules = parse::rules(x);
    println!("{:#?}", rules);
    let rules = match rules {
        Err(_) => return,
        Ok((_rest, rules)) => rules,
    };

    for (ridx, rule) in rules.iter().enumerate() {
        let mut buf = HashSet::default();
        rule.unbound_variables(&mut buf);
        if !buf.is_empty() {
            println!("rule #{:?}: {:?} has unbound vars {:?}", ridx, rule, buf);
        }
    }
    let (rules, stb) = internalize::internalize_rules(&rules);
    println!("{:#?} {:#?}", rules, stb);
}
