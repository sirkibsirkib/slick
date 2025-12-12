//  HASH.rs
//    by Christopher Esterhuyse, Tim Müller
//
//  Description:
//!   Implements order-independent hashing for Slick types that are [`Rule`]s
//!   or up.
//

use std::hash::{DefaultHasher, Hash, Hasher};

use super::{Check, Program, Rule, RuleBody};

/***** HELPERS *****/
/// Defines order-independent equality and hashing on a vec of sorts.
struct UnorderedSlice<'a, T>(&'a [T]);
impl<'a, T: Eq> Eq for UnorderedSlice<'a, T> {}
impl<'a, T: Hash> Hash for UnorderedSlice<'a, T> {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        // The gimmick is: get some ordering on the elements that's purely a function on the
        // elements, not on their position in the list, then we can hash that safely to obtain an
        // order-independent list.

        // Solution: hash all items individually, sort the hashes, then hash it all together :)
        // NOTE: Obviously it's more efficient to hash everything individually first, then hash the
        // hashes. However, this is scary because `state` might contain randomness we're not aware
        // of. So it's non-trivial how every element should be hashed in the final product, I feel.
        // NOTE: It's important that the hashing done here is not random! Else, the ordering is not
        // the same.
        let mut elems: Vec<&T> = self.0.into_iter().collect();
        elems.sort_by_cached_key(|elem| {
            let mut hasher = DefaultHasher::new();
            elem.hash(&mut hasher);
            hasher.finish()
        });

        // Now hash the elements in that order
        elems.hash(state)
    }
}
impl<'a, T: PartialEq> PartialEq for UnorderedSlice<'a, T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        // Early quit if the lengths are different to begin with, saves some time
        if self.0.len() != other.0.len() {
            return false;
        }

        // Then we attempt to find all elements of `self` in `other`, crossing off those we've seen
        // to deal with duplicates.
        // NOTE: Constructing this list should be cheap, we only allocate the required space once
        // and then store pointers.
        let mut todo: Vec<&T> = other.0.into_iter().collect();
        'outer: for lhs in self.0 {
            for (i, &rhs) in todo.iter().enumerate() {
                if lhs == rhs {
                    // We found it, cross it off the list.
                    // NOTE: It's order-independent anyway, might as well remove cheaply
                    todo.swap_remove(i);
                    continue 'outer;
                }
            }
            // Element in `self` that is not in `other`!
            return false;
        }

        // Made it to the end; all elements in `self` are in `other`!
        // Since the lengths are equal, this should prove there are also no elements in `other` not
        // in `self. Still, couldn't resist the assert here.
        #[cfg(debug_assertions)]
        assert!(todo.is_empty());
        true
    }
}

/***** IMPLS *****/
impl Hash for Check {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { kind, atoms, positive } = self;
        kind.hash(state);
        UnorderedSlice(atoms).hash(state);
        positive.hash(state);
    }
}
impl PartialEq for Check {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let Self { kind: self_kind, atoms: self_atoms, positive: self_positive } = self;
        let Self { kind: other_kind, atoms: other_atoms, positive: other_positive } = other;

        self_kind == other_kind
            && UnorderedSlice(self_atoms) == UnorderedSlice(other_atoms)
            && self_positive == other_positive
    }
}

impl Hash for RuleBody {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { pos_antecedents, neg_antecedents, checks } = self;
        UnorderedSlice(pos_antecedents).hash(state);
        UnorderedSlice(neg_antecedents).hash(state);
        UnorderedSlice(checks).hash(state);
    }
}
impl PartialEq for RuleBody {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let Self {
            pos_antecedents: self_pos_antecedents,
            neg_antecedents: self_neg_antecedents,
            checks: self_checks,
        } = self;
        let Self {
            pos_antecedents: other_pos_antecedents,
            neg_antecedents: other_neg_antecedents,
            checks: other_checks,
        } = other;

        UnorderedSlice(self_pos_antecedents) == UnorderedSlice(other_pos_antecedents)
            && UnorderedSlice(self_neg_antecedents) == UnorderedSlice(other_neg_antecedents)
            && UnorderedSlice(self_checks) == UnorderedSlice(other_checks)
    }
}

impl Hash for Rule {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { consequents, rule_body } = self;
        UnorderedSlice(consequents).hash(state);
        rule_body.hash(state);
    }
}
impl PartialEq for Rule {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let Self { consequents: self_consequents, rule_body: self_rule_body } = self;
        let Self { consequents: other_consequents, rule_body: other_rule_body } = other;

        UnorderedSlice(self_consequents) == UnorderedSlice(other_consequents)
            && self_rule_body == other_rule_body
    }
}

impl Hash for Program {
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        let Self { rules } = self;
        UnorderedSlice(rules).hash(state);
    }
}
impl PartialEq for Program {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let Self { rules: self_rules } = self;
        let Self { rules: other_rules } = other;

        UnorderedSlice(self_rules) == UnorderedSlice(other_rules)
    }
}
