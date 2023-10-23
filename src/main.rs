use std::collections::HashSet;

mod ast;
mod debug;
mod externalize;
mod infer;
mod internalize;
mod ir;
mod parse;
mod preprocess;

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
    let source = stdin_to_string();
    let source = preprocess::comments_removed(source);
    let rules = nom::combinator::all_consuming(parse::wsr(parse::rules))(&source);
    let rules = match rules {
        Err(nom::Err::Error(e)) => {
            return println!("{}", nom::error::convert_error(source.as_str(), e.clone()));
        }
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

    {
        let alt: Vec<_> = rules
            .iter()
            .map(|rule| ir::Rule {
                consequents: rule.consequents.clone(),
                pos_antecedents: rule.pos_antecedents.clone(),
                neg_antecedents: vec![],
            })
            .collect();
        // println!("TESTING RULES: {:#?}", alt);
        let res = infer::Atoms::termination_test(&alt, &symbol_table, 10);
        if let Err(counter_example) = res {
            let counter_example = counter_example.externalize_concrete(&symbol_table);
            println!("Termination test violated by {:?}", counter_example);
            return;
        }
    }

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
