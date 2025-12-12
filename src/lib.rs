pub mod atomise;
pub mod atomlike;
pub mod debug;
pub mod infer;
pub mod lang_misc;
pub mod lexicographic;
pub mod parse;
pub mod patterns;
pub mod reflect;
pub mod text;
pub mod util;

#[cfg(test)]
pub mod test;

use text::Text;

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
pub enum Pattern {
    Constant(Text) = 0,
    Wildcard = 2,
    Tuple(Vec<Pattern>) = 1,
}

#[derive(Hash, PartialOrd, Ord, Eq, PartialEq, Clone)]
#[repr(u8)]
pub enum GroundAtom {
    Constant(Text) = 0,
    Tuple(Vec<GroundAtom>) = 1,
}

pub type Variable = Text;
pub type Constant = Text;

#[derive(Hash, Clone, Eq, PartialEq)]
pub struct Rule {
    // pub rule_within: Option<GroundAtom>,
    pub consequents: Vec<Atom>,
    pub rule_body: RuleBody,
}

#[derive(Hash, Clone, Eq, PartialEq)]
pub struct RuleBody {
    pub pos_antecedents: Vec<Atom>,
    pub neg_antecedents: Vec<Atom>,
    pub checks: Vec<Check>,
}

#[derive(Hash, Clone, Eq, PartialEq, Copy)]
pub enum CheckKind {
    Diff,
    Same,
}

#[derive(Hash, Clone, Eq, PartialEq)]
pub struct Check {
    pub kind: CheckKind,
    pub atoms: Vec<Atom>,
    pub positive: bool,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Program {
    pub rules: Vec<Rule>,
}
