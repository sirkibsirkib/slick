use crate::atomlike::AtomLike;
use crate::{Atom, GroundAtom, Program, Text};

impl Program {
    pub fn add_per_head(&mut self, f: &mut impl FnMut(&Atom) -> Atom) {
        for rule in self.rules.iter_mut() {
            let new: Vec<_> = rule.consequents.iter().map(|c| f(c)).collect();
            rule.consequents.extend(new)
        }
    }
    pub fn reflect_within(&mut self, msg_id: &GroundAtom) {
        self.add_per_head(&mut |head| {
            Atom::Tuple(vec![
                head.clone(),
                Atom::Constant(Text::from_str("within")),
                msg_id.clone().to_atom(),
            ])
        })
    }
    pub fn reflect_says(&mut self, author: &GroundAtom) {
        self.add_per_head(&mut |head| {
            Atom::Tuple(vec![
                author.clone().to_atom(),
                Atom::Constant(Text::from_str("says")),
                head.clone(),
            ])
        })
    }
}
