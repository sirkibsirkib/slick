use crate::ast;
use std::collections::HashMap;

#[derive(Debug)]
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
    pub(crate) consequents: Vec<Atom>,
    pub(crate) pos_antecedents: Vec<Atom>,
    pub(crate) neg_antecedents: Vec<Atom>,
}
