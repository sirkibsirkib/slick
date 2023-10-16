use std::collections::HashSet;

mod ast;
// mod concrete;
mod debug;
mod externalize;
mod infer;
mod internalize;
mod ir;
mod parse;

fn get_source() -> Vec<u8> {
    let mut stdin = std::io::stdin().lock();
    let mut buffer = vec![];
    std::io::Read::read_to_end(&mut stdin, &mut buffer).expect("buffer overflow");
    buffer
}

fn main() {
    // println!("{:?}", parse::wsr(parse::rule)(b"x iff if iff y."));
    // return;

    let source = get_source();
    let rules = parse::wsr(parse::rules)(&source);
    let rules = match rules {
        Err(e) => return println!("PARSE ERROR {:#?}", e),
        Ok((rest, rules)) => {
            println!("REST {:?}", rest);
            rules
        }
    };
    println!("RULES: {:#?}", rules);

    for (ridx, rule) in rules.iter().enumerate() {
        if rule.wildcards_in_consequents() {
            println!(
                "ERROR: rule #{:?}: {:?} has wildcard in consequents",
                ridx, rule
            );
            return;
        }
        let mut buf = std::collections::HashSet::default();
        rule.unbound_variables(&mut buf);
        if !buf.is_empty() {
            println!(
                "ERROR: rule #{:?}: {:?} has unbound vars {:?}",
                ridx, rule, buf
            );
            return;
        }
    }
    let (rules, symbol_table) = internalize::internalize_rules(&rules);
    // println!("Symbol table {:#?}", symbol_table);
    // let atoms = infer::Atoms::big_step(&rules, infer::NegKnowledge::Empty, &symbol_table);
    // println!("Atoms:");
    // for atom in atoms.iter() {
    //     println!("{:?}", atom.externalize_concrete(&symbol_table));
    // }

    let [t, u] = infer::Atoms::alternating_fixpoint(&rules, &symbol_table);
    let f = |x: &HashSet<ir::Atom>| {
        let mut vec = x
            .iter()
            .map(|x| x.externalize_concrete(&symbol_table))
            .collect::<Vec<_>>();
        vec.sort();
        vec
    };
    let p = |aa: &[ast::Atom]| {
        for a in aa {
            println!("{:?}", a);
        }
    };
    println!("\nTRUE:");
    p(&f(&t));

    println!("\nUNKNOWN:");
    p(&f(&u));
}
