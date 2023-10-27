use crate::Atom;

use std::collections::HashSet;

#[derive(Default, Clone, Eq)]
pub struct Atoms {
    atoms_iterable: Vec<Atom>,
    atoms_testable: HashSet<Atom>,
}

impl Atoms {
    pub fn as_slice(&self) -> &[Atom] {
        self.atoms_iterable.as_slice()
    }
    pub fn insert(&mut self, atom: Atom) -> bool {
        let success = self.atoms_testable.insert(atom.clone());
        self.atoms_iterable.push(atom);
        success
    }
    pub fn contains(&self, atom: &Atom) -> bool {
        self.atoms_testable.contains(atom)
    }
}

impl PartialEq for Atoms {
    fn eq(&self, other: &Atoms) -> bool {
        self.atoms_iterable == other.atoms_iterable
    }
}
