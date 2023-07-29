use crate::ir::Constant;
use std::collections::HashMap;

#[derive(Hash, Clone, PartialEq, Eq, Debug)]
enum ConcreteAtomContents {
    Constant(Constant),
    Pair(ConcreteAtomIdx, ConcreteAtomIdx),
}

#[derive(Debug, Hash, Clone, Copy, Eq, PartialEq)]
struct ConcreteAtomIdx(u16);

#[derive(Debug, Default)]
struct ConcreteAtoms {
    iterable: Vec<ConcreteAtomContents>,
    findable: HashMap<ConcreteAtomContents, ConcreteAtomIdx>,
}

#[derive(Clone)]
enum SemiConcreteAtom {
    ConcreteAtomIdx(ConcreteAtomIdx),
    Constant(Constant),
    Pair(Box<[SemiConcreteAtom; 2]>),
}

impl ConcreteAtoms {
    fn insert(&mut self, sca: &SemiConcreteAtom) -> (bool, ConcreteAtomIdx) {
        let cac = match sca {
            SemiConcreteAtom::ConcreteAtomIdx(cai) => return (false, *cai),
            SemiConcreteAtom::Constant(c) => ConcreteAtomContents::Constant(*c),
            SemiConcreteAtom::Pair(pair) => {
                ConcreteAtomContents::Pair(self.insert(&pair[0]).1, self.insert(&pair[1]).1)
            }
        };
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
    let sca = SemiConcreteAtom::Pair(Box::new([
        SemiConcreteAtom::Pair(Box::new([
            SemiConcreteAtom::Constant(Constant(0)),
            SemiConcreteAtom::Constant(Constant(1)),
        ])),
        SemiConcreteAtom::Pair(Box::new([
            SemiConcreteAtom::Pair(Box::new([
                SemiConcreteAtom::Constant(Constant(0)),
                SemiConcreteAtom::Constant(Constant(1)),
            ])),
            SemiConcreteAtom::Pair(Box::new([
                SemiConcreteAtom::Constant(Constant(0)),
                SemiConcreteAtom::Constant(Constant(1)),
            ])),
        ])),
    ]));
    let got = ca.insert(&sca);
    println!(
        "{:#?}\n{:#?}\nbytes: {:?}",
        got,
        ca,
        ca.iterable.len() * std::mem::size_of::<ConcreteAtomContents>()
    );
}
