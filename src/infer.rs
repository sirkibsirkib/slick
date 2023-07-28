use crate::ir::{Atom, Rule, Variable};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Default)]
pub struct Atoms {
    atoms_iterable: Vec<Atom>,
    atoms_testable: HashSet<Atom>,
}

pub enum NegKnowledge {
    Empty,
    ComplementOf(Atoms),
}

impl NegKnowledge {
    fn known_false(&self, atom: &Atom) -> bool {
        match self {
            Self::Empty => false,
            Self::ComplementOf(atoms) => !atoms.atoms_testable.contains(atom),
        }
    }
}

impl Atoms {
    fn insert(&mut self, atom: Atom) {
        self.atoms_testable.insert(atom.clone());
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
}

impl Atoms {
    pub fn big_step(rules: &[Rule], nn: &NegKnowledge) -> Self {
        let mut atoms = Self::default();
        let mut var_assignment = HashMap::<Variable, &Atom>::default();
        'rules: for rule in rules {
            let mut ci = combo_iter::BoxComboIter::new(
                &atoms.atoms_iterable,
                rule.pos_antecedents.len() as usize,
            );
            'combos: while let Some(combo) = ci.next() {
                var_assignment.clear();
                assert_eq!(combo.len(), rule.pos_antecedents.len());
                for (atom, pos_antecedent) in combo.iter().zip(rule.pos_antecedents.iter()) {
                    if !atom.consistently_assign(pos_antecedent, &mut var_assignment) {
                        // failure to match
                        continue 'combos;
                    }
                }
                for neg_antecedent in &rule.neg_antecedents {
                    let atom = neg_antecedent.concretize(&var_assignment);
                    if !nn.known_false(&atom) {
                        // neg fails
                        continue 'combos;
                    }
                }
                for consequent in &rule.consequents {
                    let atom = consequent.concretize(&var_assignment);
                    if !atoms.atoms_testable.contains(&atom) {
                        // success! new knowledge
                        atoms.insert(atom);
                        continue 'rules;
                    }
                }
            }
        }
        atoms
    }
}
