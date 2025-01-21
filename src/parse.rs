use crate::{Atom, Check, CheckKind, Constant, GroundAtom, Program, Rule, RuleBody, Variable};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{anychar, char as nomchar, satisfy},
    combinator::{eof, map as nommap, not, opt, peek, recognize, verify},
    multi::{many0, many0_count, many1_count, many_m_n},
    sequence::{delimited, pair, preceded, terminated, tuple},
};

pub type IResult<I, O, E = nom::error::VerboseError<I>> = Result<(I, O), nom::Err<E>>;
pub enum Antecedent {
    Pos(Atom),
    Neg(Atom),
    Check(Check),
}

//////////////////////////////////////

type In<'a> = &'a str;

pub fn block_comment(s: In) -> IResult<In, In> {
    recognize(tuple((
        // enter block comment
        tag("/*"),
        // establish loop invariant I: '*' or '/' or 'eof' is next
        take_while(|x| x != '*' && x != '/'),
        many0_count(tuple((
            // Only if next is "*/", exit this loop
            not(tag("*/")),
            // recuses if next is "/*". otherwise only fails if 'eof', otherwise eats next char
            alt((block_comment, recognize(anychar))),
            // restore loop invariant I
            take_while(|x| x != '*' && x != '/'),
        ))),
        // next is "*/" or 'eof'
        alt((tag("*/"), eof)),
    )))(s)
}

pub fn line_comment(s: In) -> IResult<In, In> {
    recognize(preceded(tag("//"), take_while(|x| x != '\n')))(s)
}
pub fn wschar(s: In) -> IResult<In, In> {
    recognize(satisfy(char::is_whitespace))(s)
}

pub fn gapafter<'a, F, O>(inner: F) -> impl FnMut(In<'a>) -> IResult<In<'a>, O>
where
    F: FnMut(In<'a>) -> IResult<In<'a>, O> + 'a,
{
    let crud = alt((wschar, eof, line_comment, block_comment));
    terminated(inner, peek(crud))
}

pub fn wsl<'a, F, O>(inner: F) -> impl FnMut(In<'a>) -> IResult<In<'a>, O>
where
    F: FnMut(In<'a>) -> IResult<In<'a>, O> + 'a,
{
    let crud = many0_count(alt((wschar, line_comment, block_comment)));
    preceded(crud, inner)
}

pub fn ended<'a, F, O>(inner: F) -> impl FnMut(In<'a>) -> IResult<In<'a>, O>
where
    F: FnMut(In<'a>) -> IResult<In<'a>, O> + 'a,
{
    terminated(inner, wsl(eof))
}

pub fn ident_ok(s: In) -> bool {
    s.len() > 0
        && (alt((recognize(variable), neg, sep, turnstile, diff, same, wildcard, is))(s)).is_err()
}

pub fn ident_suffix(s: In) -> IResult<In, In> {
    take_while(|c| !(c as char).is_whitespace() && !"\"{(.,)}".contains(c))(s)
}
pub fn constant(s: In) -> IResult<In, Constant> {
    // identifier that isn't confusable with something else
    let a = verify(ident_suffix, ident_ok);
    // anything in double quotes without line breaks
    let b = delimited(nomchar('"'), take_while(|x| x != '"' && x != '\n'), nomchar('"'));
    nommap(wsl(alt((a, b))), Constant::from_str)(s)
}
pub fn variable(s: In) -> IResult<In, Variable> {
    let tup = tuple((many0_count(nomchar('_')), satisfy(char::is_uppercase), ident_suffix));
    let p = wsl(recognize(tup));
    nommap(p, Variable::from_str)(s)
}
pub fn wildcard(s: In) -> IResult<In, In> {
    wsl(recognize(many1_count(nomchar('_'))))(s)
}

pub fn argument(s: In) -> IResult<In, Atom> {
    let parenthesized = delimited(wsl(nomchar('(')), atom, wsl(nomchar(')')));
    let var = nommap(variable, Atom::Variable);
    let con = nommap(constant, Atom::Constant);
    let wil = nommap(wildcard, |_| Atom::Wildcard);
    alt((parenthesized, var, con, wil))(s)
}

pub fn atom(s: In) -> IResult<In, Atom> {
    let tuple = nommap(many_m_n(2, usize::MAX, argument), Atom::Tuple);
    alt((tuple, argument))(s)
}

pub fn ground_argument(s: In) -> IResult<In, GroundAtom> {
    let parenthesized = delimited(wsl(nomchar('(')), ground_atom, wsl(nomchar(')')));
    let con = nommap(constant, GroundAtom::Constant);
    alt((parenthesized, con))(s)
}

pub fn ground_atom(s: In) -> IResult<In, GroundAtom> {
    let tuple = nommap(many_m_n(2, usize::MAX, ground_argument), GroundAtom::Tuple);
    alt((tuple, ground_argument))(s)
}

pub fn neg(s: In) -> IResult<In, In> {
    wsl(gapafter(alt((tag("!"), tag("not")))))(s)
}

pub fn negated_atom(s: In) -> IResult<In, Atom> {
    preceded(neg, atom)(s)
}

pub fn sep(s: In) -> IResult<In, In> {
    wsl(gapafter(alt((tag(","), tag("and")))))(s)
}

pub fn is(s: In) -> IResult<In, In> {
    wsl(gapafter(alt((tag("="), tag("is")))))(s)
}

pub fn rulesep(s: In) -> IResult<In, In> {
    wsl(gapafter(recognize(nomchar('.'))))(s)
}

pub fn turnstile(s: In) -> IResult<In, In> {
    wsl(gapafter(alt((tag(":-"), tag("if")))))(s)
}

pub fn diff(s: In) -> IResult<In, In> {
    wsl(gapafter(tag("diff")))(s)
}
pub fn same(s: In) -> IResult<In, In> {
    wsl(gapafter(tag("same")))(s)
}
pub fn block_open(s: In) -> IResult<In, In> {
    wsl(gapafter(tag("{")))(s)
}
pub fn block_close(s: In) -> IResult<In, In> {
    wsl(gapafter(tag("}")))(s)
}

pub fn check_kind(s: In) -> IResult<In, CheckKind> {
    let sa = nommap(same, |_| CheckKind::Same);
    let di = nommap(diff, |_| CheckKind::Diff);
    alt((sa, di))(s)
}

pub fn check(s: In) -> IResult<In, Check> {
    let args = delimited(block_open, many0(argument), block_close);
    let old = nommap(tuple((opt(neg), check_kind, args)), |(maybe_not, kind, atoms)| Check {
        positive: maybe_not.is_none(),
        kind,
        atoms,
    });
    let new = nommap(tuple((opt(neg), atom, is, atom)), |(maybe_not, lhs, _, rhs)| Check {
        positive: maybe_not.is_none(),
        kind: CheckKind::Same,
        atoms: vec![lhs, rhs],
    });
    alt((new, old))(s)
}

pub fn antecedent(s: In) -> IResult<In, Antecedent> {
    let po = nommap(atom, Antecedent::Pos);
    let ne = nommap(negated_atom, Antecedent::Neg);
    let ch = nommap(check, Antecedent::Check);
    alt((ch, po, ne))(s)
}

pub fn rule(s: In) -> IResult<In, Rule> {
    let sep_atoms = many0(preceded(opt(sep), atom));
    let sep_antecedents = many0(preceded(opt(sep), antecedent));
    let body = nommap(opt(preceded(turnstile, sep_antecedents)), Option::unwrap_or_default);
    fn to_rule((consequents, antecedents): (Vec<Atom>, Vec<Antecedent>)) -> Rule {
        let mut pos_antecedents = vec![];
        let mut neg_antecedents = vec![];
        let mut checks = vec![];
        for antecedent in antecedents {
            match antecedent {
                Antecedent::Pos(atom) => pos_antecedents.push(atom),
                Antecedent::Neg(atom) => neg_antecedents.push(atom),
                Antecedent::Check(check) => checks.push(check),
            }
        }
        Rule {
            // rule_within: None,
            consequents,
            rule_body: RuleBody { pos_antecedents, neg_antecedents, checks },
        }
    }
    nommap(terminated(pair(sep_atoms, body), rulesep), to_rule)(s)
}

pub fn program(s: In) -> IResult<In, Program> {
    nommap(many0(rule), |rules| Program { rules })(s)
}
