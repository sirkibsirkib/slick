use std::collections::HashSet;

#[derive(Hash, PartialOrd, Ord, Eq, PartialEq, Clone)]
pub enum Atom {
    Wildcard,
    Constant(Constant),
    Variable(Variable),
    Tuple(Vec<Atom>),
}

#[derive(PartialOrd, Ord, Clone, Eq, PartialEq, Hash)]
pub struct Constant(pub Vec<u8>);

#[derive(PartialOrd, Ord, Clone, Eq, PartialEq, Hash)]
pub struct Variable(pub Vec<u8>);

pub struct Rule {
    pub consequents: Vec<Atom>,
    pub pos_antecedents: Vec<Atom>,
    pub neg_antecedents: Vec<Atom>,
    pub diff_sets: Vec<Vec<Atom>>,
}

impl Atom {
    fn visit_atoms<'a: 'b, 'b>(&'a self, visitor: &'b mut impl FnMut(&'a Self)) {
        visitor(self);
        if let Self::Tuple(args) = self {
            for arg in args {
                arg.visit_atoms(visitor)
            }
        }
    }
    pub fn is_tuple(&self) -> bool {
        if let Self::Tuple(_) = self {
            true
        } else {
            false
        }
    }
}

impl Rule {
    pub fn enforce_subconsequence(rules: &mut Vec<Rule>) {
        let mut buf: Vec<Atom> = vec![];
        for rule in rules {
            buf.extend(rule.consequents.iter().cloned());
            while let Some(atom) = buf.pop() {
                if !rule.pos_antecedents.contains(&atom) {
                    if let Atom::Tuple(args) = &atom {
                        buf.extend(args.iter().cloned());
                    }
                    if !rule.consequents.contains(&atom) {
                        rule.consequents.push(atom);
                    }
                }
            }
        }
    }
    pub fn without_neg_antecedents(&self) -> Self {
        Self {
            consequents: self.consequents.clone(),
            pos_antecedents: self.pos_antecedents.clone(),
            diff_sets: self.diff_sets.clone(),
            neg_antecedents: vec![],
        }
    }
    pub fn wildcards_in_consequents(&self) -> bool {
        self.consequents.iter().any(|consequent| {
            let mut any_wildcards = false;
            consequent.visit_atoms(&mut |atom| {
                if let Atom::Wildcard = atom {
                    any_wildcards = true;
                }
            });
            any_wildcards
        })
    }
    pub fn unbound_variables<'a, 'b>(&'a self, buf: &'b mut HashSet<&'a Variable>) {
        buf.clear();

        // buffer consequent vars
        for consequent in &self.consequents {
            consequent.visit_atoms(&mut |atom| {
                if let Atom::Variable(var) = atom {
                    buf.insert(var);
                }
            });
        }

        // drop antecedent vars
        for pa in &self.pos_antecedents {
            pa.visit_atoms(&mut |atom| {
                if let Atom::Variable(var) = atom {
                    buf.remove(&var);
                }
            });
        }
    }
}
