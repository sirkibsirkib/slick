use crate::ast::{Atom, Constant, Literal, Rule, Sign, Variable};
use nom::{
    branch::alt,
    bytes::complete::{tag, take, take_till, take_while},
    character::complete::{char as nomchar, multispace0, satisfy},
    combinator::{eof, map as nommap, opt, peek, recognize, verify},
    error::ParseError,
    multi::{many0, many0_count, many_m_n, separated_list0},
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

// pub fn gapped<'a, F, O, E>(inner: F) -> impl FnMut(&'a [u8]) -> IResult<&'a [u8], O, E>
// where
//     E: ParseError<&'a [u8]>,
//     F: FnMut(&'a [u8]) -> IResult<&'a [u8], O, E> + 'a,
// {
//     terminated(
//         inner,
//         peek(alt((
//             eof,
//             verify(take(1usize), |x: &[u8]| {
//                 x.iter().all(u8::is_ascii_whitespace)
//             }),
//         ))),
//     )
// }

pub fn ident_ok(s: &[u8]) -> bool {
    // println!("SUFFIX {:?}", String::from_utf8_lossy(s));
    s.len() > 0
        && (alt((
            recognize(variable),
            recognize(neg),
            recognize(sep),
            recognize(turnstile),
        ))(s))
        .is_err()
}

pub fn ident_suffix(s: &[u8]) -> IResult<&[u8], &[u8]> {
    take_while(|c: u8| !c.is_ascii_whitespace() && c != b'(' && c != b')' && c != b'.' && c != b',')(
        s,
    )
}
pub fn constant(s: &[u8]) -> IResult<&[u8], Constant> {
    let p = wsl(ident_suffix);
    nommap(verify(p, ident_ok), |s| Constant(s.to_vec()))(s)
}
pub fn variable(s: &[u8]) -> IResult<&[u8], Variable> {
    let p = wsl(recognize(pair(satisfy(char::is_uppercase), ident_suffix)));
    nommap(p, |s| Variable(s.to_vec()))(s)
}

pub fn inner_atom(s: &[u8]) -> IResult<&[u8], Atom> {
    let parenthesized = delimited(wsl(nomchar('(')), atom, wsl(nomchar(')')));
    let var = nommap(variable, Atom::Variable);
    let con = nommap(constant, Atom::Constant);
    let wil = nommap(wsl(nomchar('_')), |_| Atom::Wildcard);
    alt((parenthesized, var, con, wil))(s)
}

pub fn atom(s: &[u8]) -> IResult<&[u8], Atom> {
    let tuple = nommap(many_m_n(2, usize::MAX, inner_atom), Atom::Tuple);
    alt((tuple, inner_atom))(s)
}

pub fn neg(s: &[u8]) -> IResult<&[u8], &[u8]> {
    wsl(alt((tag("!"), tag("not"))))(s)
}

pub fn literal(s: &[u8]) -> IResult<&[u8], Literal> {
    let sign = nommap(opt(neg), |x| match x {
        Some(_) => Sign::Neg,
        None => Sign::Pos,
    });
    nommap(pair(sign, atom), |(sign, atom)| Literal { sign, atom })(s)
}

pub fn sep(s: &[u8]) -> IResult<&[u8], &[u8]> {
    wsl(alt((tag(","), tag("and"))))(s)
}

pub fn rulesep(s: &[u8]) -> IResult<&[u8], &[u8]> {
    wsl(recognize(nomchar('.')))(s)
}

pub fn turnstile(s: &[u8]) -> IResult<&[u8], &[u8]> {
    wsl(alt((tag(":-"), tag("if"))))(s)
}

pub fn rule(s: &[u8]) -> IResult<&[u8], Rule> {
    let c = separated_list0(sep, atom);
    let a = nommap(
        opt(preceded(turnstile, separated_list0(sep, literal))),
        Option::unwrap_or_default,
    );
    nommap(
        terminated(pair(c, a), rulesep),
        |(consequents, antecedents)| Rule {
            consequents,
            antecedents,
        },
    )(s)
}

pub fn rules(s: &[u8]) -> IResult<&[u8], Vec<Rule>> {
    many0(rule)(s)
}
