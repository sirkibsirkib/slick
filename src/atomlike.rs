use crate::{Atom, GroundAtom, Pattern};

pub trait AtomLike {
    fn as_atom(&self) -> &Atom;
    fn to_atom(self) -> Atom;
}

impl AtomLike for Atom {
    fn as_atom(&self) -> &Atom {
        self
    }
    fn to_atom(self) -> Atom {
        self
    }
}
impl AtomLike for GroundAtom {
    fn to_atom(self) -> Atom {
        unsafe {
            // identical in-memory representation
            std::mem::transmute(self)
        }
    }
    fn as_atom(&self) -> &Atom {
        unsafe {
            // identical in-memory representation
            std::mem::transmute(self)
        }
    }
}
impl AtomLike for Pattern {
    fn to_atom(self) -> Atom {
        unsafe {
            // identical in-memory representation
            std::mem::transmute(self)
        }
    }
    fn as_atom(&self) -> &Atom {
        unsafe {
            // identical in-memory representation
            std::mem::transmute(self)
        }
    }
}
