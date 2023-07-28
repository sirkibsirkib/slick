#[derive(Debug, Hash, PartialEq, Eq, Clone)]
pub enum Atom {
    Wildcard,
    Constant(Constant),
    Variable(Variable),
    Tuple(Vec<Atom>),
}

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Constant(pub(crate) u16);

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy)]
pub struct Variable(pub(crate) u16);

#[derive(Debug)]
pub struct Rule {
    // invariant: all vars in consequences are also in pos_antecedents
    pub(crate) consequents: Vec<Atom>,
    pub(crate) pos_antecedents: Vec<Atom>,
    pub(crate) neg_antecedents: Vec<Atom>,
}
