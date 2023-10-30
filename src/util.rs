use core::hash::Hash;
use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Clone, Eq)]
pub struct VecSet<T: Hash> {
    vec: Vec<T>,
    set: HashSet<T>,
}

pub fn pairs<T>(slice: &[T]) -> impl Iterator<Item = [&T; 2]> {
    (0..(slice.len() - 1)).flat_map(move |i| {
        ((i + 1)..slice.len()).map(move |j| unsafe {
            // safe! i and j bounds-checked
            [slice.get_unchecked(i), slice.get_unchecked(j)]
        })
    })
}

impl<T: Hash + Eq + Clone> VecSet<T> {
    pub fn insert(&mut self, element: T) -> bool {
        let success = self.set.insert(element.clone());
        self.vec.push(element);
        success
    }
    pub fn contains(&self, element: &T) -> bool {
        self.set.contains(element)
    }
    pub fn extend(&mut self, elements: impl IntoIterator<Item = T>) {
        for x in elements.into_iter() {
            self.insert(x);
        }
    }
}
impl<T: Hash + Eq + Clone> FromIterator<T> for VecSet<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut c = VecSet::default();
        c.extend(iter);
        c
    }
}
impl<T: Hash> VecSet<T> {
    pub fn as_slice(&self) -> &[T] {
        self.vec.as_slice()
    }
}

impl<T: Debug + Hash> Debug for VecSet<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.debug_list().entries(self.as_slice().iter()).finish()
    }
}

impl<T: Hash + PartialEq> PartialEq for VecSet<T> {
    fn eq(&self, other: &VecSet<T>) -> bool {
        self.vec == other.vec
    }
}

impl<T: Hash> Default for VecSet<T> {
    fn default() -> Self {
        Self { vec: Default::default(), set: Default::default() }
    }
}
