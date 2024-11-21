pub mod atomise;
pub mod atomlike;
pub mod debug;
pub mod infer;
pub mod lexicographic;
pub mod parse;
pub mod smores;
pub mod text;
pub mod util;

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
pub enum GroundAtom {
    Constant(Text) = 0,
    Tuple(Vec<GroundAtom>) = 1,
}

pub type Variable = Text;
pub type Constant = Text;

#[derive(Clone)]
pub struct Rule {
    pub part_name: Option<GroundAtom>,
    pub consequents: Vec<Atom>,
    pub rule_body: RuleBody,
}

#[derive(Clone)]
pub struct RuleBody {
    pub pos_antecedents: Vec<Atom>,
    pub neg_antecedents: Vec<Atom>,
    pub checks: Vec<Check>,
}

#[derive(Clone)]
pub enum CheckKind {
    Diff,
    Same,
}

#[derive(Clone)]
pub struct Check {
    pub kind: CheckKind,
    pub atoms: Vec<Atom>,
    pub positive: bool,
}

#[derive(Debug)]
pub struct Program {
    pub rules: Vec<Rule>,
}
