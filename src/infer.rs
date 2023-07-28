use crate::internalize::SymbolTable;
use crate::ir::{Atom, Rule, Variable};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct Atoms {
    atoms_iterable: Vec<Atom>,
    atoms_testable: HashSet<Atom>,
}

#[derive(Copy, Clone)]
pub enum NegKnowledge<'a> {
    Empty,
    ComplementOf(&'a Atoms),
}

impl NegKnowledge<'_> {
    fn known_false(self, atom: &Atom) -> bool {
        match self {
            Self::Empty => false,
            Self::ComplementOf(atoms) => !atoms.atoms_testable.contains(atom),
        }
    }
}

impl Atoms {
    fn insert(&mut self, atom: Atom) {
        let success = self.atoms_testable.insert(atom.clone());
        assert!(success);
        self.atoms_iterable.push(atom);
    }
    pub(crate) fn iter(&self) -> impl Iterator<Item = &Atom> {
        self.atoms_iterable.iter()
    }
}

impl Atom {
    fn find(&self, cond: &impl Fn(&Self) -> bool) -> Option<&Self> {
        match self {
            x if cond(x) => Some(x),
            Self::Tuple(args) => args.iter().flat_map(|arg| arg.find(cond)).next(),
            _ => None,
        }
    }
    fn consistently_assign<'a, 'b>(
        &'a self,
        concrete: &'a Self,
        var_assignments: &'b mut HashMap<Variable, &'a Atom>,
    ) -> bool {
        match [self, concrete] {
            [Self::Variable(v), x] => {
                if let Some(y) = var_assignments.get(v) {
                    &x == y
                } else {
                    var_assignments.insert(*v, x);
                    true
                }
            }
            [Self::Wildcard, _] => true,
            [Self::Constant(x), Self::Constant(y)] => x == y,
            [Self::Tuple(x), Self::Tuple(y)] if x.len() == y.len() => x
                .iter()
                .zip(y.iter())
                .all(|(x, y)| x.consistently_assign(y, var_assignments)),
            _ => false,
        }
    }

    fn concretize(&self, var_assignments: &HashMap<Variable, &Atom>) -> Self {
        match self {
            Self::Constant(c) => Self::Constant(*c),
            Self::Variable(v) => Self::clone(var_assignments.get(v).expect("unassigned!")),
            Self::Wildcard => unreachable!(),
            Self::Tuple(args) => Self::Tuple(
                args.iter()
                    .map(|arg| arg.concretize(var_assignments))
                    .collect(),
            ),
        }
    }
}

impl Atoms {
    pub fn big_step(rules: &[Rule], nk: NegKnowledge, symbol_table: &SymbolTable) -> Self {
        let mut atoms = Self::default();
        'restart: loop {
            let mut var_assignment = HashMap::<Variable, &Atom>::default();
            for (ridx, rule) in rules.iter().enumerate() {
                let mut ci = combo_iter::BoxComboIter::new(
                    &atoms.atoms_iterable,
                    rule.pos_antecedents.len() as usize,
                );
                'combos: while let Some(combo) = ci.next() {
                    var_assignment.clear();
                    assert_eq!(combo.len(), rule.pos_antecedents.len());
                    let f = |atom: &Atom| atom.externalize(symbol_table, ridx);
                    println!(
                        "{:?}",
                        combo
                            .iter()
                            .copied()
                            .map(f)
                            .zip(rule.pos_antecedents.iter().map(f))
                            .collect::<Vec<_>>()
                    );
                    for (atom, pos_antecedent) in
                        combo.iter().copied().zip(rule.pos_antecedents.iter())
                    {
                        if !atom.consistently_assign(pos_antecedent, &mut var_assignment) {
                            // failure to match
                            continue 'combos;
                        }
                    }
                    for neg_antecedent in &rule.neg_antecedents {
                        let atom = neg_antecedent.concretize(&var_assignment);
                        if !nk.known_false(&atom) {
                            // neg fails
                            continue 'combos;
                        }
                    }
                    for consequent in &rule.consequents {
                        let atom = consequent.concretize(&var_assignment);
                        // if let Some(atom) = atom.find(&|atom| !atoms.atoms_testable.contains(atom))
                        // {
                        //     // add first new subatom
                        //     atoms.insert(atom.clone());
                        //     continue 'restart;
                        // }
                        if !atoms.atoms_testable.contains(&atom) {
                            // add this atom
                            atoms.insert(atom);
                            continue 'restart;
                        }
                    }
                }
            }
            return atoms;
        }
    }
}
