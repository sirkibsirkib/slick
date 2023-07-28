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
    fn visit_variables<'a: 'b, 'b>(&'a self, visitor: &'b mut impl FnMut(&'a Variable)) {
        match self {
            Self::Variable(v) => visitor(v),
            Self::Tuple(args) => {
                for arg in args {
                    arg.visit_variables(visitor)
                }
            }
            _ => {}
        }
    }
}

impl Rule {
    pub fn unbound_variables<'a, 'b>(&'a self, buf: &'b mut HashSet<&'a Variable>) {
        buf.clear();

        // buffer consequent vars
        for consequent in &self.consequents {
            consequent.visit_variables(&mut |var| {
                buf.insert(var);
            });
        }

        // drop antecedent vars
        for antecedent in &self.antecedents {
            if let Sign::Pos = antecedent.sign {
                antecedent.atom.visit_variables(&mut |var| {
                    buf.remove(var);
                });
            }
        }
    }
}
