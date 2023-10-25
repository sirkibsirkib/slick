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

#[derive(Default, Debug)]
struct Assignments {
    // invariant: if (var,atom1) occurs before (var,atom2),
    //  atom1 is more general than atom2
    // invariant: no wildcard at root. absence of tuple is same thing
    vec: Vec<(Variable, Atom)>,
}
struct VarAssignState(usize);
#[derive(Debug)]
pub struct Denotation {
    trues: Atoms,
    prev_trues: Atoms,
}

////////////////////////

impl Assignments {
    fn save_state(&self) -> VarAssignState {
        VarAssignState(self.vec.len())
    }
    fn reset_state(&mut self, state: VarAssignState) {
        self.vec.truncate(state.0)
    }
    fn try_assign(&mut self, var: &Variable, new: &Atom) -> bool {
        if let Atom::Wildcard = new {
            return true;
        }
        let old = self.get(var);
        let refined = match old {
            Atom::Wildcard => new.clone(),
            _ => {
                if old == new {
                    return true;
                } else if let Some(refined) = Self::refine(old, new) {
                    refined
                } else {
                    return false;
                }
            }
        };
        self.vec.push((var.clone(), refined));
        true
    }
    fn get(&self, var: &Variable) -> &Atom {
        self.vec
            .iter()
            .rev()
            .filter_map(|(var2, val)| if var == var2 { Some(val) } else { None })
            .next()
            .unwrap_or(&Atom::Wildcard)
    }
    fn clear(&mut self) {
        self.vec.clear()
    }
    fn refine(x: &Atom, y: &Atom) -> Option<Atom> {
        // no variables on either side!
        match (x, y) {
            (Atom::Variable(_), _) | (_, Atom::Variable(_)) => unreachable!(),
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
    // write assignments
    fn consistently_assign<'a, 'b>(
        &'a self,
        concrete: &'a Self,
        assignments: &'b mut Assignments,
    ) -> bool {
        match [self, concrete] {
            [Self::Variable(var), new] => assignments.try_assign(var, new),
            [Self::Wildcard, _] | [_, Self::Wildcard] => true,
            [Self::Constant(x), Self::Constant(y)] => x == y,
            [Self::Tuple(x), Self::Tuple(y)] if x.len() == y.len() => {
                x.iter().zip(y.iter()).all(|(x, y)| x.consistently_assign(y, assignments))
            }
            _ => false,
        }
    }

    fn consistent_with(&self, concrete: &Self, assignments: &Assignments) -> bool {
        match [self, concrete] {
            [Self::Variable(var), new] => {
                assignments.get(var).consistent_with(concrete, assignments)
            }
            [Self::Wildcard, _] | [_, Self::Wildcard] => true,
            [Self::Constant(x), Self::Constant(y)] => x == y,
            [Self::Tuple(x), Self::Tuple(y)] if x.len() == y.len() => {
                x.iter().zip(y.iter()).all(|(x, y)| x.consistent_with(y, assignments))
            }
            _ => false,
        }
    }

    // read assignments
    fn concretize(&self, assignments: &Assignments) -> Self {
        match self {
            Self::Constant(c) => Self::Constant(c.clone()),
            Self::Variable(v) => Self::clone(assignments.get(v)),
            Self::Wildcard => Self::Wildcard,
            Self::Tuple(args) => {
                Self::Tuple(args.iter().map(|arg| arg.concretize(assignments)).collect())
            }
        }
    }
    fn depth(&self) -> usize {
        match self {
            Self::Tuple(args) => args.iter().map(Self::depth).max().map(|x| x + 1).unwrap_or(0),
            _ => 0,
        }
    }

    fn diff<'a>(&'a self, other: &'a Self, assignments: &Assignments) -> bool {
        let f = move |atom: &'a Atom| match atom {
            Self::Variable(v) => assignments.get(&v),
            _ => atom,
        };
        match [f(self), f(other)] {
            [Self::Variable(_), _] | [_, Self::Variable(_)] => unreachable!(),
            [Self::Constant(a), Self::Constant(b)] => a != b,
            [Self::Tuple(a), Self::Tuple(b)] => {
                a.len() != b.len() || a.iter().zip(b.iter()).any(|(a, b)| a.diff(b, assignments))
            }
            [Self::Wildcard, _] | [_, Self::Wildcard] => false,
            _ => true,
        }
    }

    fn same<'a>(&'a self, other: &'a Self, assignments: &Assignments) -> bool {
        let f = move |atom: &'a Atom| match atom {
            Self::Variable(v) => assignments.get(&v),
            x => x,
        };
        match [f(self), f(other)] {
            [Self::Variable(_), _] | [_, Self::Variable(_)] => unreachable!(),
            [Self::Constant(a), Self::Constant(b)] => a == b,
            [Self::Tuple(a), Self::Tuple(b)] => {
                a.len() == b.len() && a.iter().zip(b.iter()).all(|(a, b)| a.same(b, assignments))
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
    pub fn alternating_fixpoint(rules: &[Rule]) -> Denotation {
        let mut vec = vec![Self::big_step(rules, NegKnowledge::Empty, &mut |_| false)];
        loop {
            match &mut vec[..] {
                [] => unreachable!(),
                [prefix @ .., a, b, c]
                    if prefix.len() % 2 == 0 && &a.atoms_testable == &c.atoms_testable =>
                {
                    let trues = std::mem::take(a);
                    let prev_trues = std::mem::take(b);
                    // unknowns.retain(|x| !trues.contains(x));
                    return Denotation { trues, prev_trues };
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
            let mut assignments = Assignments::default();
            for (_ridx, rule) in rules.iter().enumerate() {
                let mut ci = combo_iter::BoxComboIter::new(
                    &atoms.atoms_iterable,
                    rule.pos_antecedents.len() as usize,
                );
                'combos: while let Some(combo) = ci.next() {
                    println!("\ncombo: {combo:?}");
                    assignments.clear();
                    assert_eq!(combo.len(), rule.pos_antecedents.len());
                    let pos_antecedents = combo.iter().copied().zip(rule.pos_antecedents.iter());
                    for (atom, pos_antecedent) in pos_antecedents {
                        let consistent = pos_antecedent.consistently_assign(atom, &mut assignments);
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
                            for x in rule.neg_antecedents.iter() {
                                // falsity check on x passes if it "is absent from previous kb"
                                for atom in kb.atoms_iterable.iter() {
                                    if x.consistent_with(atom, &assignments) {
                                        continue 'combos;
                                    }
                                }
                            }
                        }
                    }
                    for diff_set in &rule.diff_sets {
                        if !pairs(diff_set.as_slice()).all(|[a, b]| a.diff(b, &assignments)) {
                            continue 'combos;
                        }
                    }
                    for same_set in &rule.same_sets {
                        if !pairs(same_set.as_slice()).all(|[a, b]| a.same(b, &assignments)) {
                            continue 'combos;
                        }
                    }
                    for consequent in &rule.consequents {
                        let atom = consequent.concretize(&assignments);
                        if !atoms.atoms_testable.contains(&atom) {
                            println!("{assignments:?}");
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
