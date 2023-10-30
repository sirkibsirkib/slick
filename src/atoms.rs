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

#[derive(Debug, Clone, Copy)]
struct AtomId {
    data: u32,
}

impl AtomId {
    pub const fn width(self) -> u8 {
        self.data.to_ne_bytes()[3]
    }
    pub const fn suffix(self) -> u32 {
        let mut b = self.data.to_ne_bytes();
        b[3] = 0;
        u32::from_ne_bytes(b)
    }
    pub const unsafe fn new_unsafe(width: u8, suffix: u32) -> Self {
        let mut bytes = suffix.to_ne_bytes();
        bytes[3] = width;
        Self { data: u32::from_ne_bytes(bytes) }
    }
    pub const fn new(width: u8, suffix: u32) -> Option<Self> {
        let me = unsafe {
            // check follows. no memory unsafety possible
            Self::new_unsafe(width, suffix)
        };
        if me.suffix() == suffix {
            Some(me)
        } else {
            None
        }
    }
}
struct AtomStore {
    const_suffix_to_display: Vec<String>,
    width_to_arg_buf: Vec<Vec<AtomId>>,
}
struct PattIndex {}

struct Rule {
    // var_indices:
}
