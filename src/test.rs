use crate::{Atom, Program, Rule, RuleBody, Text};

#[test]
#[should_panic]
fn test_slick_bug() {
    let left = Program {
        rules: vec![
            Rule {
                consequents: vec![Atom::Constant(Text::from_str("foo"))],
                rule_body: RuleBody {
                    pos_antecedents: Vec::new(),
                    neg_antecedents: Vec::new(),
                    checks: Vec::new(),
                },
            },
            Rule {
                consequents: vec![Atom::Constant(Text::from_str("bar"))],
                rule_body: RuleBody {
                    pos_antecedents: vec![Atom::Tuple(vec![
                        Atom::Constant(Text::from_str("baz")),
                        Atom::Variable(Text::from_str("A")),
                    ])],
                    neg_antecedents: Vec::new(),
                    checks: Vec::new(),
                },
            },
            Rule {
                consequents: vec![Atom::Tuple(vec![
                    Atom::Tuple(vec![Atom::Constant(Text::from_str("foo"))]),
                    Atom::Constant(Text::from_str("within")),
                    Atom::Tuple(vec![
                        Atom::Constant(Text::from_str("amy")),
                        Atom::Constant(Text::from_str("a")),
                    ]),
                ])],
                rule_body: RuleBody {
                    pos_antecedents: vec![Atom::Tuple(vec![Atom::Constant(Text::from_str("foo"))])],
                    neg_antecedents: Vec::new(),
                    checks: Vec::new(),
                },
            },
        ],
    };
    let right = Program {
        rules: vec![
            Rule {
                consequents: vec![Atom::Constant(Text::from_str("foo"))],
                rule_body: RuleBody {
                    pos_antecedents: Vec::new(),
                    neg_antecedents: Vec::new(),
                    checks: Vec::new(),
                },
            },
            Rule {
                consequents: vec![Atom::Constant(Text::from_str("bar"))],
                rule_body: RuleBody {
                    pos_antecedents: vec![Atom::Tuple(vec![
                        Atom::Constant(Text::from_str("baz")),
                        Atom::Variable(Text::from_str("A")),
                    ])],
                    neg_antecedents: Vec::new(),
                    checks: Vec::new(),
                },
            },
            Rule {
                consequents: vec![Atom::Tuple(vec![
                    Atom::Tuple(vec![Atom::Constant(Text::from_str("foo"))]),
                    Atom::Constant(Text::from_str("within")),
                    Atom::Tuple(vec![
                        Atom::Constant(Text::from_str("amy")),
                        Atom::Constant(Text::from_str("a")),
                    ]),
                ])],
                rule_body: RuleBody {
                    pos_antecedents: vec![Atom::Constant(Text::from_str("foo"))],
                    neg_antecedents: Vec::new(),
                    checks: Vec::new(),
                },
            },
        ],
    };
    assert_eq!(left, right);
}
