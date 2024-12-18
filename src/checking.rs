use std::collections::HashSet;

use crate::ast::{Program, Rule, Variable};

pub type RuleIndex = usize;

impl Program {
    pub fn misplaced_wildcards(&self) -> Option<&Rule> {
        self.rules.iter().filter(|rule| rule.misplaced_wildcards()).next()
    }
    pub fn unbound_variables(&self) -> Option<(&Rule, HashSet<&Variable>)> {
        let mut buf = HashSet::default();
        for rule in &self.rules {
            rule.unbound_variables(&mut buf);
            if !buf.is_empty() {
                return Some((rule, buf));
            }
        }
        None
    }
}
