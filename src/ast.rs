use crate::text::Text;
use std::collections::{HashMap, HashSet};
use std::fmt::Debug;

pub trait AtomLike {
    fn as_atom(&self) -> &Atom;
    fn to_atom(self) -> Atom;
}

#[derive(Hash, PartialOrd, Ord, Eq, PartialEq, Clone)]
#[repr(u8)]
pub enum Atom {
    Constant(Text) = 0,
    Tuple(Vec<Atom>) = 1,
    Wildcard = 2,
    Variable(Text) = 3,
}

#[derive(Hash, PartialOrd, Ord, Eq, PartialEq, Clone)]
#[repr(u8)]
pub enum GroundAtom {
    Constant(Text) = 0,
    Tuple(Vec<GroundAtom>) = 1,
}

pub type Variable = Text;
pub type Constant = Text;

pub struct Rule {
    pub consequents: Vec<Atom>,
    pub pos_antecedents: Vec<Atom>,
    pub neg_antecedents: Vec<Atom>,
    pub diff_sets: Vec<Vec<Atom>>,
    pub same_sets: Vec<Vec<Atom>>,
    pub part_name: Option<GroundAtom>,
}

#[derive(Debug)]
pub struct Program {
    pub rules: Vec<Rule>,
}

/////////////////////

impl AtomLike for Atom {
    fn as_atom(&self) -> &Atom {
        self
    }
    fn to_atom(self) -> Atom {
        self
    }
}
impl AtomLike for GroundAtom {
    fn to_atom(self) -> Atom {
        unsafe {
            // identical in-memory representation
            std::mem::transmute(self)
        }
    }
    fn as_atom(&self) -> &Atom {
        unsafe {
            // identical in-memory representation
            std::mem::transmute(self)
        }
    }
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
    fn visit_atoms_mut(&mut self, visitor: &mut impl FnMut(&mut Self)) {
        visitor(self);
        if let Self::Tuple(args) = self {
            for arg in args {
                arg.visit_atoms_mut(visitor)
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
    pub fn has_wildcard(&self) -> bool {
        match self {
            Self::Constant(_) | Self::Variable(_) => false,
            Self::Wildcard => true,
            Self::Tuple(args) => args.iter().any(Self::has_wildcard),
        }
    }
}

impl Program {
    pub fn preprocess(&mut self) {
        let mut var_counts = HashMap::default();
        for rule in self.rules.iter_mut() {
            // drop consequents that are also antecedents
            rule.count_var_occurrences(&mut var_counts);
            rule.wildcardify_vars(|var| var_counts.get(var) == Some(&1));
            rule.consequents.retain(|consequent| !rule.pos_antecedents.contains(consequent))
        }
        // drop rules with no consequents
        self.rules.retain(|rule| !rule.consequents.is_empty());
    }
}

impl Rule {
    pub fn wildcard_in_consequent(&self) -> bool {
        // wildcards can only occur in pos antecedents
        self.consequents.iter().any(Atom::has_wildcard)
    }
    pub fn wildcardify_vars(&mut self, test: impl Fn(&Variable) -> bool) {
        let iter = self
            .consequents
            .iter_mut()
            .chain(self.pos_antecedents.iter_mut())
            .chain(self.neg_antecedents.iter_mut())
            .chain(self.diff_sets.iter_mut().flat_map(|set| set.iter_mut()))
            .chain(self.same_sets.iter_mut().flat_map(|set| set.iter_mut()));
        for atom in iter {
            atom.visit_atoms_mut(&mut |atom| {
                if let Atom::Variable(var) = atom {
                    if test(var) {
                        *atom = Atom::Wildcard;
                    }
                }
            });
        }
    }
    pub fn count_var_occurrences(&self, counts: &mut HashMap<Variable, u32>) {
        let iter = self
            .consequents
            .iter()
            .chain(self.pos_antecedents.iter())
            .chain(self.neg_antecedents.iter())
            .chain(self.diff_sets.iter().flat_map(|set| set.iter()))
            .chain(self.same_sets.iter().flat_map(|set| set.iter()));

        for atom in iter {
            atom.visit_atoms(&mut |atom| {
                if let Atom::Variable(var) = atom {
                    *counts.entry(var.clone()).or_default() += 1;
                }
            });
        }
    }
    pub fn without_neg_antecedents(&self) -> Self {
        Self {
            consequents: self.consequents.clone(),
            pos_antecedents: self.pos_antecedents.clone(),
            diff_sets: self.diff_sets.clone(),
            same_sets: self.same_sets.clone(),
            part_name: self.part_name.clone(),
            neg_antecedents: vec![],
        }
    }
    pub fn unbound_variables<'a, 'b>(&'a self, buf: &'b mut HashSet<&'a Variable>) {
        buf.clear();

        // buffer consequent vars
        let need = self
            .consequents
            .iter()
            .chain(self.diff_sets.iter().flat_map(|set| set.iter()))
            .chain(self.same_sets.iter().flat_map(|set| set.iter()));
        for atom in need {
            atom.visit_atoms(&mut |atom| {
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
