use crate::ast::{AsciiString, Atom, Constant, Literal, Rule, Sign, Variable};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{char as nomchar, multispace0, satisfy},
    combinator::{map as nommap, opt, recognize},
    error::ParseError,
    multi::{many0, many_m_n, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
    IResult,
};

pub fn wsl<'a, F, O, E>(inner: F) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    E: ParseError<&'a [u8]>,
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E> + 'a,
{
    preceded(multispace0, inner)
}

pub fn wsr<'a, F, O, E>(inner: F) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
where
    E: ParseError<&'a [u8]>,
    F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E> + 'a,
{
    terminated(inner, multispace0)
}

pub fn ident_suffix(s: &[u8]) -> IResult<&[u8], &[u8]> {
    recognize(many0(satisfy(|c| c.is_alphanumeric() || c == '_')))(s)
}
pub fn constant(s: &[u8]) -> IResult<&[u8], Constant> {
    nommap(
        recognize(pair(satisfy(char::is_lowercase), ident_suffix)),
        |s| Constant(AsciiString(s.to_vec())),
    )(s)
}
pub fn maybe_variable(s: &[u8]) -> IResult<&[u8], Option<Variable>> {
    let a = nommap(variable, Some);
    let b = nommap(nomchar('_'), |_| None);
    alt((a, b))(s)
}
pub fn variable(s: &[u8]) -> IResult<&[u8], Variable> {
    nommap(
        recognize(pair(satisfy(char::is_uppercase), ident_suffix)),
        |s| Variable(AsciiString(s.to_vec())),
    )(s)
}
pub fn atom(s: &[u8]) -> IResult<&[u8], Atom> {
    let tuple = nommap(many_m_n(2, usize::MAX, inner_atom), Atom::Tuple);
    wsl(alt((tuple, inner_atom)))(s)
}

pub fn inner_atom(s: &[u8]) -> IResult<&[u8], Atom> {
    let parenthesized = delimited(nomchar('('), atom, nomchar(')'));
    let wil = nommap(wsl(nomchar('_')), |_| Atom::Wildcard);
    let var = nommap(variable, Atom::Variable);
    let con = nommap(constant, Atom::Constant);
    wsl(alt((parenthesized, wil, var, con)))(s)
}

pub fn literal(s: &[u8]) -> IResult<&[u8], Literal> {
    let sign = nommap(opt(wsl(nomchar('!'))), |x| match x {
        Some(_) => Sign::Neg,
        None => Sign::Pos,
    });
    nommap(pair(sign, atom), |(sign, atom)| Literal { sign, atom })(s)
}

pub fn rules(s: &[u8]) -> IResult<&[u8], Vec<Rule>> {
    wsr(many0(rule))(s)
}

pub fn rule(s: &[u8]) -> IResult<&[u8], Rule> {
    let c = separated_list0(wsl(nomchar(',')), atom);
    let a = nommap(
        opt(preceded(
            wsl(tag(":-")),
            separated_list0(wsl(nomchar(',')), literal),
        )),
        Option::unwrap_or_default,
    );
    nommap(
        terminated(pair(c, a), wsl(nomchar('.'))),
        |(consequents, antecedents)| Rule {
            consequents,
            antecedents,
        },
    )(s)
}
