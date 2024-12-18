use crate::atomlike::AtomLike;
use crate::{Atom, GroundAtom, Program, Rule, Text};

pub trait Withinify {
    fn withinify(self, msg_id: &GroundAtom) -> Self;
}

impl Withinify for Rule {
    fn withinify(mut self, msg_id: &GroundAtom) -> Self {
        let mut buf = Vec::with_capacity(self.consequents.len() * 2);
        for c in self.consequents.drain(..) {
            // add `c within msg_id`
            buf.push(Atom::Tuple(vec![
                c.clone(),
                Atom::Constant(Text::from_str("within")),
                msg_id.clone().to_atom(),
            ]));
            // add `c` again
            buf.push(c);
        }
        std::mem::swap(&mut self.consequents, &mut buf);
        self
    }
}

impl Withinify for Program {
    fn withinify(self, msg_id: &GroundAtom) -> Self {
        Self { rules: self.rules.into_iter().map(|rule| rule.withinify(msg_id)).collect() }
    }
}
