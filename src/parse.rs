use crate::ast::{Atom, Constant, Literal, Rule, Sign, Variable};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char as nomchar, multispace0, satisfy},
    combinator::{map as nommap, opt, recognize, verify},
    error::ParseError,
    multi::{many0, many_m_n, separated_list0},
    sequence::{delimited, pair, preceded, terminated},
};
pub type IResult<I, O, E = nom::error::VerboseError<I>> = Result<(I, O), nom::Err<E>>;

type In<'a> = &'a str;
// type In<'a> = &'a [u8];

pub fn wsl<'a, F, O, E>(inner: F) -> impl FnMut(In<'a>) -> IResult<In<'a>, O, E>
where
    E: ParseError<In<'a>>,
    F: FnMut(In<'a>) -> IResult<In<'a>, O, E> + 'a,
{
    preceded(multispace0, inner)
}

pub fn wsr<'a, F, O, E>(inner: F) -> impl FnMut(In<'a>) -> IResult<In<'a>, O, E>
where
    E: ParseError<In<'a>>,
    F: FnMut(In<'a>) -> IResult<In<'a>, O, E> + 'a,
{
    terminated(inner, multispace0)
}

pub fn ident_ok(s: In) -> bool {
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

pub fn ident_suffix(s: In) -> IResult<In, In> {
    take_while(|c| !(c as char).is_whitespace() && !"(.,)".contains(c))(s)
}
pub fn constant(s: In) -> IResult<In, Constant> {
    let p = wsl(ident_suffix);
    nommap(verify(p, ident_ok), |s| Constant(s.into()))(s)
}
pub fn variable(s: In) -> IResult<In, Variable> {
    let p = wsl(recognize(pair(satisfy(char::is_uppercase), ident_suffix)));
    nommap(p, |s| Variable(s.into()))(s)
}

pub fn inner_atom(s: In) -> IResult<In, Atom> {
    let parenthesized = delimited(wsl(nomchar('(')), atom, wsl(nomchar(')')));
    let var = nommap(variable, Atom::Variable);
    let con = nommap(constant, Atom::Constant);
    let wil = nommap(wsl(nomchar('_')), |_| Atom::Wildcard);
    alt((parenthesized, var, con, wil))(s)
}

pub fn atom(s: In) -> IResult<In, Atom> {
    let tuple = nommap(many_m_n(2, usize::MAX, inner_atom), Atom::Tuple);
    alt((tuple, inner_atom))(s)
}

pub fn neg(s: In) -> IResult<In, In> {
    wsl(alt((tag("!"), tag("not"))))(s)
}

pub fn literal(s: In) -> IResult<In, Literal> {
    let sign = nommap(opt(neg), |x| match x {
        Some(_) => Sign::Neg,
        None => Sign::Pos,
    });
    nommap(pair(sign, atom), |(sign, atom)| Literal { sign, atom })(s)
}

pub fn sep(s: In) -> IResult<In, In> {
    wsl(alt((tag(","), tag("and"))))(s)
}

pub fn rulesep(s: In) -> IResult<In, In> {
    wsl(recognize(nomchar('.')))(s)
}

pub fn turnstile(s: In) -> IResult<In, In> {
    wsl(alt((tag(":-"), tag("if"))))(s)
}

pub fn rule(s: In) -> IResult<In, Rule> {
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

pub fn rules(s: In) -> IResult<In, Vec<Rule>> {
    many0(rule)(s)
}
