use crate::atomlike::AtomLike;
use crate::{Atom, GroundAtom, Program, Rule, Text};

impl Rule {
    fn reflect_within(&mut self, msg_id: &GroundAtom) {
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
    }
}

impl Program {
    pub fn reflect_within(&mut self, msg_id: &GroundAtom) {
        for rule in self.rules.iter_mut() {
            rule.reflect_within(msg_id);
        }
    }
}
