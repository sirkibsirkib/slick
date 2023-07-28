mod ast;
mod debug;
mod externalize;
mod infer;
mod internalize;
mod ir;
mod parse;

fn main() {
    let x = b"
    A say X :- A say (X if Y), A say Y.
    amy say (a if b).
    amy say b.
    admin amy.
    A :- admin A.
    X :- A say X, admin A.
    ";
    // let x = b"
    // admin amy.
    // A :- admin A.
    // ";
    let rules = parse::rules(x);
    let rules = match rules {
        Err(e) => return println!("PARSE ERROR {:#?}", e),
        Ok((_rest, rules)) => rules,
    };
    println!("RULES: {:#?}", rules);

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
    // println!("Symbol table {:#?}", symbol_table);
    let atoms = infer::Atoms::big_step(&rules, infer::NegKnowledge::Empty, &symbol_table);
    println!("Atoms:");
    for atom in atoms.iter() {
        println!("{:?}", atom.externalize_concrete(&symbol_table));
    }
}
