use crate::{Atom, Pattern};

impl Atom {
    pub fn as_pattern(&self) -> Pattern {
        match self {
            Atom::Tuple(v) => Pattern::Tuple(v.iter().map(Self::as_pattern).collect()),
            Atom::Constant(c) => Pattern::Constant(*c),
            _ => Pattern::Wildcard,
        }
    }
}
