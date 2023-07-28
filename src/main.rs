mod ast;
mod infer;
mod internalize;
mod ir;
mod parse;
mod pretty;

fn main() {
    let x = b"amy likes bob. X likes Y :- Y likes X. X likes X :- X.";
    let rules = parse::rules(x);
    let rules = match rules {
        Err(e) => return println!("PARSE ERROR {:#?}", e),
        Ok((_rest, rules)) => rules,
    };

    for (ridx, rule) in rules.iter().enumerate() {
        if rule.wildcards_in_consequents() {
            println!("rule #{:?}: {:?} has wildcard in consequents", ridx, rule);
        }
        let mut buf = std::collections::HashSet::default();
        rule.unbound_variables(&mut buf);
        if !buf.is_empty() {
            println!("rule #{:?}: {:?} has unbound vars {:?}", ridx, rule, buf);
        }
    }
    let (rules, symbol_table) = internalize::internalize_rules(&rules);
    // println!("{:#?} {:#?}", rules, symbol_table);
    let atoms = infer::Atoms::big_step(&rules, infer::NegKnowledge::Empty);
    println!(
        "{:?}",
        pretty::Pretty {
            t: &atoms,
            symbol_table: &symbol_table
        }
    );
}
