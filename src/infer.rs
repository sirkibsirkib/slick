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
}

impl Atom {
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
    fn depth(&self) -> usize {
        match self {
            Self::Tuple(args) => args
                .iter()
                .map(Self::depth)
                .max()
                .map(|x| x + 1)
                .unwrap_or(0),
            _ => 0,
        }
    }
}
impl Atoms {
    pub fn termination_test(
        rules: &[Rule],
        symbol_table: &SymbolTable,
        max_depth: usize,
    ) -> Result<(), Atom> {
        let mut result = Ok(());
        let store_counterexample = &mut |atom: &Atom| {
            if max_depth < atom.depth() {
                result = Err(atom.clone());
                true
            } else {
                false
            }
        };
        Self::big_step(
            rules,
            NegKnowledge::Empty,
            symbol_table,
            store_counterexample,
        );
        result
    }
    pub fn alternating_fixpoint(rules: &[Rule], symbol_table: &SymbolTable) -> [HashSet<Atom>; 2] {
        let mut vec = vec![Self::big_step(
            rules,
            NegKnowledge::Empty,
            symbol_table,
            &mut |_| false,
        )];
        loop {
            match &mut vec[..] {
                [] => unreachable!(),
                [prefix @ .., a, b, c]
                    if prefix.len() % 2 == 0 && &a.atoms_testable == &c.atoms_testable =>
                {
                    let trues = std::mem::take(&mut a.atoms_testable);
                    let mut unknowns = std::mem::take(&mut b.atoms_testable);
                    unknowns.retain(|x| !trues.contains(x));
                    return [trues, unknowns];
                }
                [.., a] => {
                    let b = Self::big_step(
                        rules,
                        NegKnowledge::ComplementOf(a),
                        symbol_table,
                        &mut |_| false,
                    );
                    vec.push(b);
                }
            }
        }
    }
    pub fn big_step(
        rules: &[Rule],
        nk: NegKnowledge,
        _symbol_table: &SymbolTable,
        halter: &mut impl FnMut(&Atom) -> bool,
    ) -> Self {
        let mut atoms = Self::default();
        'restart: loop {
            let mut var_assignment = HashMap::<Variable, &Atom>::default();
            for (_ridx, rule) in rules.iter().enumerate() {
                let mut ci = combo_iter::BoxComboIter::new(
                    &atoms.atoms_iterable,
                    rule.pos_antecedents.len() as usize,
                );
                'combos: while let Some(combo) = ci.next() {
                    var_assignment.clear();
                    assert_eq!(combo.len(), rule.pos_antecedents.len());
                    // let r = |atom: &Rule| atom.externalize(symbol_table, ridx);
                    // let f = |atom: &Atom| atom.externalize_concrete(symbol_table);
                    // let g = |var: &Variable| var.externalize(symbol_table, ridx);
                    // println!(
                    //     "COMBO {:?}",
                    //     combo
                    //         .iter()
                    //         .copied()
                    //         .map(f)
                    //         .zip(rule.pos_antecedents.iter().map(f))
                    //         .collect::<Vec<_>>()
                    // );
                    for (atom, pos_antecedent) in
                        combo.iter().copied().zip(rule.pos_antecedents.iter())
                    {
                        let consistent =
                            pos_antecedent.consistently_assign(atom, &mut var_assignment);
                        // println!(
                        //     "after concretize with vars {:?}",
                        //     var_assignment
                        //         .iter()
                        //         .map(|(v, a)| (g(v), f(a)))
                        //         .collect::<Vec<_>>()
                        // );
                        if !consistent {
                            // failure to match
                            continue 'combos;
                        }
                    }
                    // println!(
                    //     "success with vars {:?}",
                    //     var_assignment
                    //         .iter()
                    //         .map(|(v, a)| (g(v), f(a)))
                    //         .collect::<Vec<_>>()
                    // );
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
                            if halter(&atom) {
                                return atoms;
                            }
                            // add this atom
                            // println!(
                            //     "new addition using rule ridx={}! {:?} using rule",
                            //     ridx,
                            //     f(&atom)
                            // );
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
