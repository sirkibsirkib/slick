use crate::{Constant, GroundAtom};
use core::cmp::Ordering;

pub trait Lexicographic {
    fn rightward_flat_constants(&self, other: &Self) -> Ordering;
    fn rightward_lexicographic(&self, other: &Self) -> Ordering;
    fn rightward_integer(&self, other: &Self) -> Ordering;
}

impl Lexicographic for GroundAtom {
    fn rightward_flat_constants(&self, other: &Self) -> Ordering {
        self.flatten_then_ord_constants_by(other, Constant::rightward_flat_constants)
    }
    fn rightward_lexicographic(&self, other: &Self) -> Ordering {
        use GroundAtom as Ga;
        match [self, other] {
            [Ga::Constant(a), Ga::Constant(b)] => a.rightward_flat_constants(b),
            [Ga::Tuple(a), Ga::Tuple(b)] => a
                .iter()
                .zip(b)
                .map(|(a, b)| a.rightward_flat_constants(b))
                .fold(Ordering::Equal, Ordering::then)
                .then(a.len().cmp(&b.len())),
            [Ga::Constant(..), Ga::Tuple(..)] => Ordering::Less,
            [Ga::Tuple(..), Ga::Constant(..)] => Ordering::Greater,
        }
    }
    fn rightward_integer(&self, other: &Self) -> Ordering {
        self.flatten_then_ord_constants_by(other, Constant::rightward_integer)
    }
}
