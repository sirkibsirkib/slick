use crate::atomise::Atomise;
use crate::{text::Text, *};
use core::cmp::Ordering;
use std::collections::{HashMap, HashSet};

use Atom as A;

/////////////////////

impl GroundAtom {
    pub fn error() -> Self {
        Self::Constant(Constant::from_str("error"))
    }
    pub fn flatten_then_ord_constants_by(
        &self,
        other: &Self,
        func: impl Fn(&Constant, &Constant) -> Ordering + Copy,
    ) -> Ordering {
        // example: a a a < a b < a b a
        use {std::slice::from_ref, GroundAtom as Ga};
        let [left, right]: [&[Ga]; 2] = match [self, other] {
            [Ga::Constant(a), Ga::Constant(b)] => {
                // base case: both are constants
                return func(a, b);
            }
            [Ga::Tuple(a), Ga::Tuple(b)] => [a, b],
            [a @ Ga::Constant(..), Ga::Tuple(b)] => [from_ref(a), b],
            [Ga::Tuple(a), b @ Ga::Constant(..)] => [a, from_ref(b)],
        };
        // inductive step, both are slices
        left.iter()
            .zip(right)
            .map(|(a, b)| a.flatten_then_ord_constants_by(b, func))
            .fold(Ordering::Equal, Ordering::then)
            .then(left.len().cmp(&right.len()))
    }
}
impl Atom {
    pub fn subsumed_by(&self, patt: &Self) -> bool {
        match [self, patt] {
            [_, A::Variable(..)] => unreachable!(),
            [_, A::Wildcard] => true,
            [A::Constant(a), A::Constant(b)] => a == b,
            [A::Tuple(a), A::Tuple(b)] => {
                a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.subsumed_by(b))
            }
            _ => false,
        }
    }
    pub fn visit_atoms<'a: 'b, 'b>(&'a self, visitor: &'b mut impl FnMut(&'a Self)) {
        visitor(self);
        if let A::Tuple(args) = self {
            for arg in args {
                arg.visit_atoms(visitor)
            }
        }
    }
    pub fn visit_atoms_mut(&mut self, visitor: &mut impl FnMut(&mut Self)) {
        visitor(self);
        if let A::Tuple(args) = self {
            for arg in args {
                arg.visit_atoms_mut(visitor)
            }
        }
    }
    pub fn is_tuple(&self) -> bool {
        if let A::Tuple(_) = self {
            true
        } else {
            false
        }
    }
    pub fn has_wildcard(&self) -> bool {
        match self {
            A::Constant(_) | A::Variable(_) => false,
            A::Wildcard => true,
            A::Tuple(args) => args.iter().any(A::has_wildcard),
        }
    }
    pub fn is_ground(&self) -> bool {
        match self {
            A::Constant(_) => true,
            A::Variable(_) | A::Wildcard => false,
            A::Tuple(args) => args.iter().all(A::is_ground),
        }
    }
    pub fn try_as_ground_atom(&self) -> Option<&GroundAtom> {
        if self.is_ground() {
            unsafe {
                // checked that the repr is the same
                std::mem::transmute(self)
            }
        } else {
            None
        }
    }
}

impl Program {
    pub fn preprocess(&mut self) {
        let antecedent_text = Text::from_str("?");
        let mut var_counts = HashMap::default();
        for rule in self.rules.iter_mut() {
            rule.replace_constants_with_reflected_consequents(antecedent_text);

            // optimisation: discard consequents that are already pos antecedents
            rule.consequents
                .retain(|consequent| !rule.rule_body.pos_antecedents.contains(consequent));

            var_counts.clear();
            rule.count_var_occurrences(&mut var_counts);
            rule.wildcardify_vars(|var| var_counts.get(var) == Some(&1));
        }
        // drop rules with no consequents
        self.rules.retain(|rule| !rule.consequents.is_empty());
    }
    pub fn pos_antecedent_patterns(&self) -> HashSet<Atom> {
        self.rules
            .iter()
            .flat_map(|rule| {
                rule.rule_body.pos_antecedents.iter().cloned().filter_map(|mut atom| {
                    let mut has_var = false;
                    atom.visit_atoms_mut(&mut |atom| match atom {
                        A::Variable(..) | A::Wildcard => {
                            has_var = true;
                            *atom = A::Wildcard;
                        }
                        _ => {}
                    });
                    if has_var {
                        Some(atom)
                    } else {
                        None
                    }
                })
            })
            .collect()
    }
}

impl RuleBody {
    pub fn without_neg_antecedents(&self) -> Self {
        Self { neg_antecedents: vec![], ..self.clone() }
    }
}
impl Rule {
    pub fn replace_constants_with_reflected_consequents(&mut self, replaced: Text) {
        enum MaybeAtomised<'a> {
            ToCompute(&'a RuleBody),
            UseCached(Atom),
        }
        fn rec(ma: &mut MaybeAtomised, replaced: Text, visit: &mut Atom) {
            match visit {
                Atom::Constant(c) if *c == replaced => {
                    *visit = match ma {
                        MaybeAtomised::ToCompute(rule_body) => {
                            let a = rule_body.atomise();
                            *ma = MaybeAtomised::UseCached(a.clone());
                            a
                        }
                        MaybeAtomised::UseCached(a) => a.clone(),
                    };
                }
                Atom::Tuple(atoms) => {
                    for atom in atoms {
                        rec(ma, replaced, atom)
                    }
                }
                _ => {}
            }
        }

        let Self { rule_body, consequents, .. } = self;
        let ma = &mut MaybeAtomised::ToCompute(rule_body);
        for atom in consequents.iter_mut() {
            rec(ma, replaced, atom)
        }
    }
    pub fn misplaced_wildcards(&self) -> bool {
        // wildcards can only occur in pos antecedents
        let Self { consequents, rule_body, .. } = self;
        let RuleBody { neg_antecedents, .. } = rule_body;
        consequents.iter().chain(neg_antecedents).any(A::has_wildcard)
    }
    pub fn wildcardify_vars(&mut self, test: impl Fn(&Variable) -> bool) {
        let Self { consequents, rule_body, .. } = self;
        let RuleBody { pos_antecedents, neg_antecedents, checks, .. } = rule_body;
        let iter = consequents
            .iter_mut()
            .chain(pos_antecedents)
            .chain(neg_antecedents)
            .chain(checks.iter_mut().flat_map(|check| check.atoms.iter_mut()));
        for atom in iter {
            atom.visit_atoms_mut(&mut |atom| {
                if let A::Variable(var) = atom {
                    if test(var) {
                        *atom = A::Wildcard;
                    }
                }
            });
        }
    }
    pub fn count_var_occurrences(&self, counts: &mut HashMap<Variable, u32>) {
        counts.clear();

        let Self { consequents, rule_body, .. } = self;
        let RuleBody { pos_antecedents, neg_antecedents, checks, .. } = rule_body;
        let iter = consequents
            .iter()
            .chain(pos_antecedents)
            .chain(neg_antecedents)
            .chain(checks.iter().flat_map(|check| check.atoms.iter()));
        for atom in iter {
            atom.visit_atoms(&mut |atom| {
                if let A::Variable(var) = atom {
                    *counts.entry(var.clone()).or_default() += 1;
                }
            });
        }
    }
    pub fn without_neg_antecedents(&self) -> Self {
        Self { rule_body: self.rule_body.without_neg_antecedents(), ..self.clone() }
    }
    pub fn unbound_variables<'a, 'b>(&'a self, buf: &'b mut HashSet<&'a Variable>) {
        // starting from an empty buffer
        buf.clear();

        // buffer vars in atoms whose vars must be bound
        let Self { consequents, rule_body, .. } = self;
        let RuleBody { pos_antecedents, neg_antecedents, checks, .. } = rule_body;
        let need = consequents
            .iter()
            .chain(neg_antecedents)
            .chain(checks.iter().flat_map(|check| check.atoms.iter()));
        for atom in need {
            atom.visit_atoms(&mut |atom| {
                if let A::Variable(var) = atom {
                    buf.insert(var);
                }
            });
        }

        // unbuffer vars in atoms binding vars
        for pa in pos_antecedents {
            pa.visit_atoms(&mut |atom| {
                if let A::Variable(var) = atom {
                    buf.remove(&var);
                }
            });
        }
    }
}