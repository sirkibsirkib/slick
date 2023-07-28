use std::collections::HashSet;

#[derive(Debug)]
pub enum Atom {
    Wildcard,
    Constant(Constant),
    Variable(Variable),
    Tuple(Vec<Atom>),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct AsciiString(pub Vec<u8>);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Constant(pub AsciiString);

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Variable(pub AsciiString);

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

#[derive(Debug)]
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
