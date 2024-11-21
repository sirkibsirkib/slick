use crate::{Atom, Check, CheckKind, RuleBody, Text};

pub trait Atomise {
    fn atomise(&self) -> Atom;
}

impl Atomise for Atom {
    fn atomise(&self) -> Atom {
        self.clone()
    }
}
impl Atomise for Check {
    fn atomise(&self) -> Atom {
        let s = match self.kind {
            CheckKind::Same => "same",
            CheckKind::Diff => "diff",
        };
        let c = Atom::Constant(Text::from_str(s));
        let a = Atom::Tuple(vec![c, Atom::Tuple(self.atoms.clone())]);
        match self.positive {
            true => a,
            false => Atom::Tuple(vec![Atom::Constant(Text::from_str("not")), a]),
        }
    }
}
impl Atomise for RuleBody {
    fn atomise(&self) -> Atom {
        let iter = self
            .pos_antecedents
            .iter()
            .map(Atomise::atomise)
            .chain(
                self.neg_antecedents
                    .iter()
                    .map(Atomise::atomise)
                    .map(|a| Atom::Tuple(vec![Atom::Constant(Text::from_str("not")), a])),
            )
            .chain(self.checks.iter().map(Atomise::atomise));
        Atom::Tuple(iter.collect())
    }
}
