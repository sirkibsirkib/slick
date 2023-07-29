use crate::ir::{Constant, Variable};
use std::collections::HashMap;

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
enum ConcreteAtomContents {
    Constant(Constant),
    Pair([ConcreteAtomIdx; 2]),
}

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
struct ConcreteAtomIdx(u16);

enum RuleAtom {
    Constant(Constant),
    Wildcard,
    Variable(Variable),
    Pair(Box<[RuleAtom; 2]>),
}

#[derive(Debug, Default)]
struct ConcreteAtoms {
    iterable: Vec<ConcreteAtomContents>,
    findable: HashMap<ConcreteAtomContents, ConcreteAtomIdx>,
}

// #[derive(Clone)]
// enum RuleAtom {
//     Constant(Constant),
//     ConcreteAtomIdx(ConcreteAtomIdx),
//     Pair(Box<[RuleAtom; 2]>),
// }

impl RuleAtom {
    fn try_concretize(
        &self,
        ca: &mut ConcreteAtoms,
        cai: ConcreteAtomIdx,
        varmap: &mut HashMap<Variable, ConcreteAtomIdx>,
    ) -> Option<ConcreteAtomIdx> {
        match self {
            RuleAtom::Wildcard => return Some(cai),
            RuleAtom::Variable(v) => {
                let cai2 = *varmap.entry(*v).or_insert(cai);
                return if cai == cai2 { Some(cai) } else { None };
            }
            _ => {}
        }
        match (self, &ca.iterable[cai.0 as usize]) {
            (RuleAtom::Constant(c1), ConcreteAtomContents::Constant(c2)) => {
                return if c1 == c2 { Some(cai) } else { None }
            }
            (RuleAtom::Variable(_) | RuleAtom::Wildcard, _) => unreachable!(),
            (RuleAtom::Pair(x), ConcreteAtomContents::Pair(y)) => {
                let [xa, xb] = x.as_ref();
                let [ya, yb] = *y;
                let za = xa.try_concretize(ca, ya, varmap)?;
                let zb = xb.try_concretize(ca, yb, varmap)?;
                Some(ca.insert_cac(ConcreteAtomContents::Pair([za, zb])).1)
            }
            _ => None,
        }
    }
}

impl ConcreteAtoms {
    fn insert_concrete_ra(&mut self, ra: &RuleAtom) -> (bool, ConcreteAtomIdx) {
        let cac = match ra {
            RuleAtom::Wildcard | RuleAtom::Variable(_) => unreachable!(),
            RuleAtom::Constant(c) => ConcreteAtomContents::Constant(c.clone()),
            RuleAtom::Pair(pair) => ConcreteAtomContents::Pair([
                self.insert_concrete_ra(&pair[0]).1,
                self.insert_concrete_ra(&pair[1]).1,
            ]),
        };
        self.insert_cac(cac)
        // let mut new = false;
        // let cai = *self.findable.entry(cac.clone()).or_insert_with(|| {
        //     let cai = ConcreteAtomIdx(self.iterable.len().try_into().expect("overflow!"));
        //     self.iterable.push(cac);
        //     new = true;
        //     cai
        // });
        // (new, cai)
    }
    fn insert_cac(&mut self, cac: ConcreteAtomContents) -> (bool, ConcreteAtomIdx) {
        let mut new = false;
        let cai = *self.findable.entry(cac.clone()).or_insert_with(|| {
            let cai = ConcreteAtomIdx(self.iterable.len().try_into().expect("overflow!"));
            self.iterable.push(cac);
            new = true;
            cai
        });
        (new, cai)
    }
}

#[test]
fn whee() {
    let mut ca = ConcreteAtoms::default();
    let sca = RuleAtom::Pair(Box::new([
        RuleAtom::Constant(Constant(0)),
        RuleAtom::Constant(Constant(1)),
    ]));
    let got = ca.insert_concrete_ra(&sca);
    println!(
        "{:#?}\n{:#?}\nbytes: {:?}",
        got,
        ca,
        ca.iterable.len() * std::mem::size_of::<ConcreteAtomContents>()
    );
}
