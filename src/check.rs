use crate::ast::{Atom, Constant, Rule};
use crate::HashSet;

pub(crate) enum Pattern {
    Tuple(Vec<Pattern>),
    Constant(Constant),
    Wildcard,
}

impl Atom {
    fn most_general_pattern(&self) -> Pattern {
        match self {
            Self::Variable(_) | Self::Wildcard => Pattern::Wildcard,
            Self::Constant(c) => Pattern::Constant(c.clone()),
            Self::Tuple(args) => {
                Pattern::Tuple(args.iter().map(Self::most_general_pattern).collect())
            }
        }
    }
}

fn compute_patterns(rules: &[Rule]) -> HashSet<Pattern> {
    let mut patterns = Default::default();
    for rule in rules {}
    patterns
}
