use crate::ast::{Atom, Constant, Program, Rule, Variable};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while},
    character::complete::{char as nomchar, satisfy},
    combinator::{eof, map as nommap, opt, recognize, verify},
    error::ParseError,
    multi::{many0, many0_count, many1, many1_count, many_m_n, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
};
pub type IResult<I, O, E = nom::error::VerboseError<I>> = Result<(I, O), nom::Err<E>>;
pub enum Antecedent {
    Pos(Atom),
    Neg(Atom),
    DiffSet(Vec<Atom>),
    SameSet(Vec<Atom>),
}

//////////////////////////////////////

type In<'a> = &'a str;
// type In<'a> = &'a [u8];

pub fn wsl<'a, F, O, E>(inner: F) -> impl FnMut(In<'a>) -> IResult<In<'a>, O, E>
where
    E: ParseError<In<'a>>,
    F: FnMut(In<'a>) -> IResult<In<'a>, O, E> + 'a,
{
    let ws = recognize(satisfy(char::is_whitespace));
    let line_comment = recognize(tuple((
        tag("//"), //fst
        take_while(|x| x != '\n'),
    )));
    let block_comment = recognize(tuple((
        tag("/*"),                // enter block comment
        take_while(|x| x != '*'), // walk to first star
        nomchar('*'),
        many0_count(tuple((
            // false alarm. wasn't block end. walk to next star
            satisfy(|x| x != '/'),
            take_while(|x| x != '*'),
            nomchar('*'),
        ))),
        // exit block comment
        nomchar('/'),
    )));
    let crud = many0_count(alt((ws, line_comment, block_comment)));
    preceded(crud, inner)
}

pub fn ended<'a, F, O, E>(inner: F) -> impl FnMut(In<'a>) -> IResult<In<'a>, O, E>
where
    E: ParseError<In<'a>> + 'a,
    F: FnMut(In<'a>) -> IResult<In<'a>, O, E> + 'a,
{
    terminated(inner, wsl(eof))
}

pub fn ident_ok(s: In) -> bool {
    // println!("SUFFIX {:?}", String::from_utf8_lossy(s));
    s.len() > 0
        && (alt((recognize(variable), neg, sep, turnstile, diff, same, wildcard))(s)).is_err()
}

pub fn ident_suffix(s: In) -> IResult<In, In> {
    take_while(|c| !(c as char).is_whitespace() && !"{(.,)}".contains(c))(s)
}
pub fn constant(s: In) -> IResult<In, Constant> {
    let p = wsl(ident_suffix);
    nommap(verify(p, ident_ok), |s| Constant(s.into()))(s)
}
pub fn variable(s: In) -> IResult<In, Variable> {
    let tup = tuple((many0_count(nomchar('_')), satisfy(char::is_uppercase), ident_suffix));
    let p = wsl(recognize(tup));
    nommap(p, |s| Variable(s.into()))(s)
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

pub fn ground_argument(s: In) -> IResult<In, Atom> {
    let parenthesized = delimited(wsl(nomchar('(')), ground_atom, wsl(nomchar(')')));
    let con = nommap(constant, Atom::Constant);
    let wil = nommap(wsl(nomchar('_')), |_| Atom::Wildcard);
    alt((parenthesized, con, wil))(s)
}

pub fn ground_atom(s: In) -> IResult<In, Atom> {
    let tuple = nommap(many_m_n(2, usize::MAX, ground_argument), Atom::Tuple);
    alt((tuple, ground_argument))(s)
}

pub fn neg(s: In) -> IResult<In, In> {
    wsl(alt((tag("!"), tag("not"))))(s)
}

pub fn negated_atom(s: In) -> IResult<In, Atom> {
    preceded(neg, atom)(s)
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

pub fn diff(s: In) -> IResult<In, In> {
    wsl(tag("diff"))(s)
}
pub fn same(s: In) -> IResult<In, In> {
    wsl(tag("same"))(s)
}
pub fn block_open(s: In) -> IResult<In, In> {
    wsl(tag("{"))(s)
}
pub fn block_close(s: In) -> IResult<In, In> {
    wsl(tag("}"))(s)
}

pub fn diff_set(s: In) -> IResult<In, Vec<Atom>> {
    delimited(pair(diff, block_open), many1(argument), block_close)(s)
}
pub fn same_set(s: In) -> IResult<In, Vec<Atom>> {
    delimited(pair(same, block_open), many1(argument), block_close)(s)
}

pub fn part(s: In) -> IResult<In, (Atom, Vec<Rule>)> {
    let rules = delimited(block_open, many0(rule), block_close);
    nommap(pair(ground_atom, rules), |(c, mut rules)| {
        for rule in rules.iter_mut() {
            rule.part_name = Some(c.clone());
        }
        (c, rules)
    })(s)
}

pub fn antecedent(s: In) -> IResult<In, Antecedent> {
    let po = nommap(atom, Antecedent::Pos);
    let ne = nommap(negated_atom, Antecedent::Neg);
    let di = nommap(diff_set, Antecedent::DiffSet);
    let sa = nommap(same_set, Antecedent::SameSet);
    alt((po, ne, di, sa))(s)
}

pub fn rule(s: In) -> IResult<In, Rule> {
    let c = separated_list0(sep, atom);
    let a = nommap(
        opt(preceded(turnstile, separated_list0(sep, antecedent))),
        Option::unwrap_or_default,
    );
    fn to_rule((consequents, antecedents): (Vec<Atom>, Vec<Antecedent>)) -> Rule {
        let mut pos_antecedents = vec![];
        let mut neg_antecedents = vec![];
        let mut diff_sets = vec![];
        let mut same_sets = vec![];
        for antecedent in antecedents {
            match antecedent {
                Antecedent::Pos(atom) => pos_antecedents.push(atom),
                Antecedent::Neg(atom) => neg_antecedents.push(atom),
                Antecedent::DiffSet(atoms) => diff_sets.push(atoms),
                Antecedent::SameSet(atoms) => same_sets.push(atoms),
            }
        }
        Rule {
            consequents,
            pos_antecedents,
            neg_antecedents,
            diff_sets,
            same_sets,
            part_name: None,
        }
    }
    nommap(terminated(pair(c, a), rulesep), to_rule)(s)
}

pub fn program(s: In) -> IResult<In, Program> {
    enum PartOrRule {
        Part((Atom, Vec<Rule>)),
        Rule(Rule),
    }
    let p = alt((nommap(part, PartOrRule::Part), nommap(rule, PartOrRule::Rule)));
    nommap(many0(p), |pors| {
        let mut rules = vec![];
        for por in pors {
            match por {
                PartOrRule::Rule(rule) => rules.push(rule),
                PartOrRule::Part((_, part_rules)) => rules.extend(part_rules),
            }
        }
        Program { rules }
    })(s)
}
