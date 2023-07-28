#[derive(Debug)]
pub enum Atom {
    Constant(Constant),
    MaybeVariable(Option<Variable>),
    Tuple(Vec<Atom>),
}

#[derive(Debug)]
pub struct AsciiString(pub Vec<u8>);

#[derive(Debug)]
pub struct Constant(pub AsciiString);

#[derive(Debug)]
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
    pub antecedents: Option<Vec<Literal>>,
}
