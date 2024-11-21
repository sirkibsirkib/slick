use crate::{atomlike::AtomLike, infer::GroundAtoms, Atom, CheckKind, GroundAtom, Pattern, Rule};
use std::fmt::{Debug, Formatter, Result as FmtResult};

struct AtomSeq<'a, T: IntoIterator<Item = &'a Atom> + Clone>(T);

impl<'a, T: IntoIterator<Item = &'a Atom> + Clone> Debug for AtomSeq<'a, T> {
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
            Self::Tuple(args) => AtomSeq(args).fmt(f),
        }
    }
}
impl Debug for GroundAtom {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        self.as_atom().fmt(f)
    }
}
impl Debug for Pattern {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        self.as_atom().fmt(f)
    }
}

impl Debug for Rule {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for (i, consequent) in self.consequents.iter().enumerate() {
            if i > 0 {
                write!(f, " and ")?;
            }
            write!(f, "{consequent:?}")?;
        }
        if !(self.rule_body.pos_antecedents.is_empty()
            && self.rule_body.neg_antecedents.is_empty()
            && self.rule_body.checks.is_empty())
        {
            let mut delim = Some(" if ").into_iter().chain(std::iter::repeat(" and "));
            for atom in &self.rule_body.pos_antecedents {
                let d = delim.next().unwrap();
                write!(f, "{d}{atom:?}")?;
            }
            for atom in &self.rule_body.neg_antecedents {
                let d = delim.next().unwrap();
                write!(f, "{d}not {atom:?}")?;
            }
            for check in &self.rule_body.checks {
                let d = delim.next().unwrap();
                let prefix = match check.positive {
                    true => "",
                    false => "not ",
                };
                let op = match check.kind {
                    CheckKind::Diff => "diff",
                    CheckKind::Same => "same",
                };
                let atoms = AtomSeq(&check.atoms);
                write!(f, "{d}{prefix}{op} {{ {atoms:?} }}")?;
            }
        }
        Ok(())
    }
}

impl Debug for GroundAtoms {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        let iter = self.vec_set.as_slice().iter().map(AtomLike::as_atom);
        f.debug_set().entries(iter).finish()
    }
}
