use core::cmp::Ordering;
use core::hash::Hash;
use std::collections::HashSet;
use std::fmt::Debug;

#[derive(Clone, Eq)]
pub struct VecSet<T: Hash> {
    vec: Vec<T>,
    set: HashSet<T>,
}

pub fn id<T>(t: T) -> T {
    t
}

pub fn lexicographic<T: Ord>(
    mut a: impl Iterator<Item = T>,
    mut b: impl Iterator<Item = T>,
) -> Ordering {
    loop {
        match [a.next(), b.next()] {
            [Some(a), Some(b)] => match a.cmp(&b) {
                Ordering::Equal => {} // continue loop
                otherwise => return otherwise,
            },
            [Some(..), None] => return Ordering::Greater,
            [None, Some(..)] => return Ordering::Less,
            [None, None] => return Ordering::Equal,
        }
    }
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
        if success {
            self.vec.push(element);
        }
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
    pub fn to_vec(self) -> Vec<T> {
        self.vec
    }
    pub fn retain(mut self, mut func: impl FnMut(&T) -> bool) {
        let adapter = move |t: &T| {
            let func_retain = func(t);
            if !func_retain {
                self.set.remove(t);
            }
            func_retain
        };
        self.vec.retain(adapter)
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
