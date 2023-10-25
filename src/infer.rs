use crate::ast::{Atom, Rule, Variable};
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

#[derive(Default)]
struct VarAssignments {
    // invariant: if (var,atom1) occurs before (var,atom2),
    //  atom1 is more general than atom2
    vec: Vec<(Variable, Atom)>,
}
struct VarAssignState(usize);

////////////////////////

impl VarAssignments {
    fn save_state(&self) -> VarAssignState {
        VarAssignState(self.vec.len())
    }
    fn reset_state(&mut self, state: VarAssignState) {
        self.vec.truncate(state.0)
    }
    fn try_assign(&mut self, var: &Variable, new: &Atom) -> bool {
        if let Some(old) = self.get(var) {
            if old == new {
                true
            } else if let Some(refined) = Self::refine(old, new) {
                self.vec.push((var.clone(), refined));
                true
            } else {
                false
            }
        } else {
            self.vec.push((var.clone(), new.clone()));
            true
        }
    }
    fn get(&self, var: &Variable) -> Option<&Atom> {
        self.vec
            .iter()
            .rev()
            .filter_map(|(var2, val)| if var == var2 { Some(val) } else { None })
            .next()
    }
    fn refine(x: &Atom, y: &Atom) -> Option<Atom> {
        match (x, y) {
            (Atom::Wildcard, _) => Some(y.clone()),
            (x, Atom::Wildcard) => Some(x.clone()),
            (Atom::Constant(a), Atom::Constant(b)) => {
                if a == b {
                    Some(Atom::Constant(a.clone()).clone())
                } else {
                    None
                }
            }
            (Atom::Tuple(a), Atom::Tuple(b)) => {
                if a.len() != b.len() {
                    return None;
                }
                a.iter()
                    .zip(b.iter())
                    .map(|(a, b)| Self::refine(a, b))
                    .collect::<Option<Vec<_>>>()
                    .map(Atom::Tuple)
            }
            (Atom::Tuple(_), Atom::Constant(_)) | (Atom::Constant(_), Atom::Tuple(_)) => None,
            (Atom::Variable(_), _) | (_, Atom::Variable(_)) => unreachable!(),
        }
    }
}

fn pairs<T>(slice: &[T]) -> impl Iterator<Item = [&T; 2]> {
    (0..(slice.len() - 1)).flat_map(move |i| {
        ((i + 1)..slice.len()).map(move |j| unsafe {
            // safe! i and j bounds-checked
            [slice.get_unchecked(i), slice.get_unchecked(j)]
        })
    })
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
            [Self::Variable(var), new] => {
                if let Some(old) = var_assignments.get(var) {
                    old == &new
                } else {
                    var_assignments.insert(var.clone(), new);
                    true
                }
            }
            [Self::Wildcard, _] => true,
            [_, Self::Wildcard] => true,
            [Self::Constant(x), Self::Constant(y)] => x == y,
            [Self::Tuple(x), Self::Tuple(y)] if x.len() == y.len() => {
                x.iter().zip(y.iter()).all(|(x, y)| x.consistently_assign(y, var_assignments))
            }
            _ => false,
        }
    }

    fn concretize(&self, var_assignments: &HashMap<Variable, &Atom>) -> Self {
        match self {
            Self::Constant(c) => Self::Constant(c.clone()),
            Self::Variable(v) => {
                Self::clone(var_assignments.get(v).expect(&format!("unassigned {v:?}")))
            }
            Self::Wildcard => Self::Wildcard,
            Self::Tuple(args) => {
                Self::Tuple(args.iter().map(|arg| arg.concretize(var_assignments)).collect())
            }
        }
    }
    fn depth(&self) -> usize {
        match self {
            Self::Tuple(args) => args.iter().map(Self::depth).max().map(|x| x + 1).unwrap_or(0),
            _ => 0,
        }
    }

    fn diff<'a>(
        &'a self,
        other: &'a Self,
        var_assignments: &'a HashMap<Variable, &'a Atom>,
    ) -> bool {
        let f = move |atom: &'a Atom| match atom {
            Self::Variable(v) => var_assignments.get(&v).copied().unwrap(),
            _ => atom,
        };
        match [f(self), f(other)] {
            [Self::Variable(_), _] | [_, Self::Variable(_)] => unreachable!(),
            [Self::Constant(a), Self::Constant(b)] => a != b,
            [Self::Tuple(a), Self::Tuple(b)] => {
                a.len() != b.len()
                    || a.iter().zip(b.iter()).any(|(a, b)| a.diff(b, var_assignments))
            }
            [Self::Wildcard, _] | [_, Self::Wildcard] => false,
            _ => true,
        }
    }

    fn same<'a>(
        &'a self,
        other: &'a Self,
        var_assignments: &'a HashMap<Variable, &'a Atom>,
    ) -> bool {
        let f = move |atom: &'a Atom| match atom {
            Self::Variable(v) => var_assignments.get(&v).copied().unwrap(),
            x => x,
        };
        match [f(self), f(other)] {
            [Self::Variable(_), _] | [_, Self::Variable(_)] => unreachable!(),
            [Self::Constant(a), Self::Constant(b)] => a == b,
            [Self::Tuple(a), Self::Tuple(b)] => {
                a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(a, b)| a.same(b, var_assignments))
            }
            [Self::Wildcard, _] | [_, Self::Wildcard] => false,
            _ => false,
        }
    }
}
impl Atoms {
    pub fn termination_test(rules: &[Rule], max_depth: usize) -> Result<(), Atom> {
        let mut result = Ok(());
        let store_counterexample = &mut |atom: &Atom| {
            if max_depth < atom.depth() {
                result = Err(atom.clone());
                true
            } else {
                false
            }
        };
        Self::big_step(rules, NegKnowledge::Empty, store_counterexample);
        result
    }
    pub fn alternating_fixpoint(rules: &[Rule]) -> [HashSet<Atom>; 2] {
        let mut vec = vec![Self::big_step(rules, NegKnowledge::Empty, &mut |_| false)];
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
                    let b = Self::big_step(rules, NegKnowledge::ComplementOf(a), &mut |_| false);
                    vec.push(b);
                }
            }
        }
    }
    pub fn big_step(
        rules: &[Rule],
        nk: NegKnowledge,
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
                    let pos_antecedents = combo.iter().copied().zip(rule.pos_antecedents.iter());
                    for (atom, pos_antecedent) in pos_antecedents {
                        let consistent =
                            pos_antecedent.consistently_assign(atom, &mut var_assignment);
                        if !consistent {
                            // failure to match
                            continue 'combos;
                        }
                    }
                    match nk {
                        NegKnowledge::Empty => {
                            if !rule.neg_antecedents.is_empty() {
                                continue 'combos;
                            }
                        }
                        NegKnowledge::ComplementOf(kb) => {
                            if !rule.neg_antecedents.iter().all(|must_neg| {
                                kb.atoms_iterable
                                    .iter()
                                    .any(|is_neg| !must_neg.diff(is_neg, &var_assignment))
                            }) {
                                continue 'combos;
                            }
                        }
                    }
                    for diff_set in &rule.diff_sets {
                        if !pairs(diff_set.as_slice()).all(|[a, b]| a.diff(b, &var_assignment)) {
                            continue 'combos;
                        }
                    }
                    for same_set in &rule.same_sets {
                        if !pairs(same_set.as_slice()).all(|[a, b]| a.same(b, &var_assignment)) {
                            continue 'combos;
                        }
                    }
                    for consequent in &rule.consequents {
                        let atom = consequent.concretize(&var_assignment);
                        if !atoms.atoms_testable.contains(&atom) {
                            if halter(&atom) {
                                return atoms;
                            }
                            atoms.insert(atom);
                            continue 'restart;
                        }
                    }
                }
            }
            return atoms;
        }
    }

    // pub fn rec(
    //     &self,
    //     rule: &Rule,
    //     pos_antecedent_buf: &mut Vec<&Atom>,
    //     nk: NegKnowledge,
    // ) -> Option<Atom> {
    //     if let Some(rule_atom) = rule.pos_antecedents.get(pos_antecedent_buf.len()) {
    //         for kb_atom in self.atoms_iterable.iter() {
    //             pos_antecedent_buf.push(kb_atom);
    //             self.rec(rule, pos_antecedent_buf, nk);
    //             pos_antecedent_buf.pop();
    //         }
    //     } else {
    //         // got all them antecedents!
    //     }
    // }
}
