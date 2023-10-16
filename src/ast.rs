use std::collections::HashSet;

#[derive(PartialOrd, Ord, Eq, PartialEq)]
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

#[derive(Debug)]
pub struct Literal {
    pub sign: Sign,
    pub atom: Atom,
}

#[derive(Debug)]
pub enum Sign {
    Pos,
    Neg,
}

pub struct Rule {
    pub consequents: Vec<Atom>,
    pub antecedents: Vec<Literal>,
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
        for antecedent in &self.antecedents {
            if let Sign::Pos = antecedent.sign {
                antecedent.atom.visit_atoms(&mut |atom| {
                    if let Atom::Variable(var) = atom {
                        buf.remove(var);
                    }
                });
            }
        }
    }
}
