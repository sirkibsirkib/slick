use crate::ast::{Atom, Constant, Rule, Variable};
use std::fmt::{Debug, Formatter, Result as FmtResult};

struct Atoms<'a, T: IntoIterator<Item = &'a Atom> + Clone>(T);

impl<'a, T: IntoIterator<Item = &'a Atom> + Clone> Debug for Atoms<'a, T> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for (i, arg) in self.0.clone().into_iter().enumerate() {
            if i > 0 {
                write!(f, " ")?;
            }
            if arg.is_tuple() {
                write!(f, "(")?;
            }
            arg.fmt(f)?;

            if arg.is_tuple() {
                write!(f, ")")?;
            }
        }
        Ok(())
    }
}

impl Debug for Atom {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Wildcard => write!(f, "_"),
            Self::Constant(c) => c.fmt(f),
            Self::Variable(v) => v.fmt(f),
            Self::Tuple(args) => Atoms(args).fmt(f),
        }
    }
}

impl Debug for Constant {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", &self.0)
    }
}
impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", &self.0)
        // write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}

impl Debug for Rule {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for (i, consequent) in self.consequents.iter().enumerate() {
            if i > 0 {
                write!(f, " and ")?;
            } else {
                write!(f, " ")?;
            }
            write!(f, "{consequent:?}",)?;
        }
        if !(self.pos_antecedents.is_empty()
            && self.neg_antecedents.is_empty()
            && self.diff_sets.is_empty())
        {
            let mut delim = Some(" if ").into_iter().chain(std::iter::repeat(" and "));
            for atom in &self.pos_antecedents {
                write!(f, "{}{atom:?}", delim.next().unwrap())?;
            }
            for atom in &self.neg_antecedents {
                write!(f, "{}not {atom:?}", delim.next().unwrap())?;
            }
            for atoms in self.diff_sets.iter().map(Atoms) {
                write!(f, "{}diff: {atoms:?}", delim.next().unwrap())?;
            }
            for atoms in self.same_sets.iter().map(Atoms) {
                write!(f, "{}same: {atoms:?}", delim.next().unwrap())?;
            }
        }
        Ok(())
    }
}
