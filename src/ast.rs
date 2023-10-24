use std::collections::HashSet;

#[derive(Hash, PartialOrd, Ord, Eq, PartialEq, Clone)]
pub enum Atom {
    Wildcard,
    Constant(Constant),
    Variable(Variable),
    Tuple(Vec<Atom>),
}

#[derive(PartialOrd, Ord, Clone, Eq, PartialEq, Hash)]
pub struct Constant(pub String);

#[derive(PartialOrd, Ord, Clone, Eq, PartialEq, Hash)]
pub struct Variable(pub String);

pub struct Rule {
    pub consequents: Vec<Atom>,
    pub pos_antecedents: Vec<Atom>,
    pub neg_antecedents: Vec<Atom>,
    pub diff_sets: Vec<Vec<Atom>>,
    pub same_sets: Vec<Vec<Atom>>,
    pub part_name: Option<Atom>, // ground
}

impl Atom {
    fn constant(s: &str) -> Self {
        Self::Constant(Constant(s.into()))
    }
    fn visit_atoms<'a: 'b, 'b>(&'a self, visitor: &'b mut impl FnMut(&'a Self)) {
        visitor(self);
        if let Self::Tuple(args) = self {
            for arg in args {
                arg.visit_atoms(visitor)
            }
        }
    }
    pub fn is_tuple(&self) -> bool {
        if let Self::Tuple(_) = self {
            true
        } else {
            false
        }
    }
    pub fn reflect(&self) -> Self {
        match self {
            Self::Wildcard => Self::constant("?").into(),
            Self::Constant(Constant(c)) => Self::constant(c),
            Self::Variable(Variable(v)) => {
                return Self::Tuple(vec![Self::constant("var"), Self::constant(v)])
            }
            Self::Tuple(args) => Self::Tuple(args.iter().map(Self::reflect).collect()),
        }
    }
}

impl Rule {
    pub fn static_reflect(rules: &mut Vec<Rule>) {
        let new_rules: Vec<_> = rules
            .iter()
            .enumerate()
            .map(|(i, rule)| {
                let rule_id = Atom::Tuple(vec![
                    Atom::constant("rule"),
                    Atom::Constant(Constant(format!("{i}"))),
                ]);
                let mut reflected_consequents = vec![];
                if let Some(pn) = &rule.part_name {
                    reflected_consequents.push(Atom::Tuple(vec![
                        rule_id.clone(),
                        Atom::Constant(Constant("has_author".into())),
                        pn.clone(),
                    ]));
                }
                for atom in &rule.consequents {
                    reflected_consequents.push(Atom::Tuple(vec![
                        rule_id.clone(),
                        Atom::constant("has_consequent"),
                        atom.reflect(),
                    ]));
                }
                for atom in &rule.pos_antecedents {
                    reflected_consequents.push(Atom::Tuple(vec![
                        rule_id.clone(),
                        Atom::constant("has_pos_antecedent"),
                        atom.reflect(),
                    ]));
                }
                for atom in &rule.pos_antecedents {
                    reflected_consequents.push(Atom::Tuple(vec![
                        rule_id.clone(),
                        Atom::constant("has_neg_antecedent"),
                        atom.reflect(),
                    ]));
                }

                for (i2, diff_set) in rule.diff_sets.iter().enumerate() {
                    for atom in diff_set {
                        reflected_consequents.push(Atom::Tuple(vec![
                            rule_id.clone(),
                            Atom::constant("has_diff_set"),
                            Atom::Tuple(vec![
                                Atom::constant("diff_set"),
                                Atom::Constant(Constant(format!("{i2}"))),
                            ]),
                            atom.reflect(),
                        ]));
                    }
                }
                for (i2, same_set) in rule.same_sets.iter().enumerate() {
                    for atom in same_set {
                        reflected_consequents.push(Atom::Tuple(vec![
                            rule_id.clone(),
                            Atom::constant("has_same_set"),
                            Atom::Tuple(vec![
                                Atom::constant("same_set"),
                                Atom::Constant(Constant(format!("{i2}"))),
                            ]),
                            atom.reflect(),
                        ]));
                    }
                }
                Rule {
                    consequents: reflected_consequents,
                    pos_antecedents: vec![],
                    neg_antecedents: vec![],
                    diff_sets: vec![],
                    same_sets: vec![],
                    part_name: rule.part_name.clone(),
                }
            })
            .collect();
        rules.extend(new_rules);
    }
    pub fn enforce_says<'a>(said_rules: impl Iterator<Item = (&'a mut Rule, Constant)>) {
        for (rule, sayer) in said_rules {
            let sayings: Vec<_> = rule
                .consequents
                .iter()
                .filter(|atom| {
                    if let Atom::Tuple(args) = atom {
                        if let [a, b, _] = &args[..] {
                            if let [Atom::Constant(a), Atom::Constant(b)] = [a, b] {
                                if a == &sayer && &b.0 == "says" {
                                    return false;
                                }
                            }
                        }
                    }
                    true
                })
                .cloned()
                .collect();
            rule.consequents.extend(sayings.into_iter().map(|atom| {
                Atom::Tuple(vec![
                    Atom::Constant(sayer.clone()),
                    Atom::Constant(Constant("says".into())),
                    atom,
                ])
            }))
        }
    }
    pub fn enforce_subconsequence(rules: &mut Vec<Rule>) {
        let mut buf: Vec<Atom> = vec![];
        for rule in rules {
            buf.extend(rule.consequents.iter().cloned());
            while let Some(atom) = buf.pop() {
                if !rule.pos_antecedents.contains(&atom) {
                    if let Atom::Tuple(args) = &atom {
                        buf.extend(args.iter().cloned());
                    }
                    if !rule.consequents.contains(&atom) {
                        rule.consequents.push(atom);
                    }
                }
            }
        }
    }
    pub fn without_neg_antecedents(&self) -> Self {
        Self {
            consequents: self.consequents.clone(),
            pos_antecedents: self.pos_antecedents.clone(),
            diff_sets: self.diff_sets.clone(),
            same_sets: self.same_sets.clone(),
            part_name: self.part_name.clone(),
            neg_antecedents: vec![],
        }
    }
    pub fn wildcards_in_consequents(&self) -> bool {
        self.consequents.iter().any(|consequent| {
            let mut any_wildcards = false;
            consequent.visit_atoms(&mut |atom| {
                if let Atom::Wildcard = atom {
                    any_wildcards = true;
                }
            });
            any_wildcards
        })
    }
    pub fn unbound_variables<'a, 'b>(&'a self, buf: &'b mut HashSet<&'a Variable>) {
        buf.clear();

        // buffer consequent vars
        let iter = self
            .consequents
            .iter()
            .chain(self.neg_antecedents.iter())
            .chain(self.diff_sets.iter().flat_map(|set| set.iter()))
            .chain(self.same_sets.iter().flat_map(|set| set.iter()));
        for atom in iter {
            atom.visit_atoms(&mut |atom| {
                if let Atom::Variable(var) = atom {
                    buf.insert(var);
                }
            });
        }

        // drop antecedent vars
        for pa in &self.pos_antecedents {
            pa.visit_atoms(&mut |atom| {
                if let Atom::Variable(var) = atom {
                    buf.remove(&var);
                }
            });
        }
    }
}
