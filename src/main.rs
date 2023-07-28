mod ast;
mod debug;
mod externalize;
mod infer;
mod internalize;
mod ir;
mod parse;

fn get_source() -> Vec<u8> {
    let arg = std::env::args().nth(1);
    println!("arg1 is {:?}", arg);
    let filename = arg.expect("no given path!");
    let mut f = std::fs::File::open(&filename).expect("no file found");
    let metadata = std::fs::metadata(&filename).expect("unable to read metadata");
    let mut buffer = vec![0; metadata.len() as usize];
    std::io::Read::read(&mut f, &mut buffer).expect("buffer overflow");
    buffer
}

fn main() {
    let source = get_source();
    let rules = parse::rules(&source);
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
