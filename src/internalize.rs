pub type RuleIndex = usize;

use crate::{ast, ir};
use std::collections::HashMap;

#[derive(Default, Debug)]
pub struct SymbolTable {
    pub(crate) variables: HashMap<RuleIndex, HashMap<ir::Variable, ast::Variable>>,
    pub(crate) constants: HashMap<ir::Constant, ast::Constant>,
}

struct SymbolTableBuilder {
    symbol_table: SymbolTable,
    previous_variables: HashMap<RuleIndex, HashMap<ast::Variable, ir::Variable>>,
    next_variable: HashMap<RuleIndex, ir::Variable>,
    previous_constants: HashMap<ast::Constant, ir::Constant>,
    next_constant: ir::Constant,
}

impl Default for SymbolTableBuilder {
    fn default() -> Self {
        Self {
            previous_variables: Default::default(),
            symbol_table: Default::default(),
            next_variable: Default::default(),
            previous_constants: Default::default(),
            next_constant: ir::Constant(0),
        }
    }
}

impl ast::Constant {
    fn internalize(&self, stb: &mut SymbolTableBuilder) -> ir::Constant {
        if let Some(c) = stb.previous_constants.get(self) {
            return *c;
        } else {
            let c = stb.next_constant;
            stb.symbol_table.constants.insert(c, self.clone());
            stb.previous_constants.insert(self.clone(), c);
            stb.next_constant.0 = stb.next_constant.0.checked_add(1).expect("OVERFLOW!");
            c
        }
    }
}

impl ast::Variable {
    fn internalize(&self, stb: &mut SymbolTableBuilder, ridx: RuleIndex) -> ir::Variable {
        let previous_variables = stb.previous_variables.entry(ridx).or_default();
        let next_variable = stb
            .next_variable
            .entry(ridx)
            .or_insert_with(|| ir::Variable(0));
        if let Some(c) = previous_variables.get(self) {
            return *c;
        } else {
            let v = *next_variable;
            stb.symbol_table
                .variables
                .entry(ridx)
                .or_default()
                .insert(v, self.clone());
            stb.previous_variables
                .entry(ridx)
                .or_default()
                .insert(self.clone(), v);
            next_variable.0 = next_variable.0.checked_add(1).expect("OVERFLOW!");
            v
        }
    }
}

impl ast::Atom {
    fn internalize(&self, stb: &mut SymbolTableBuilder, ridx: RuleIndex) -> ir::Atom {
        match self {
            ast::Atom::Wildcard => ir::Atom::Wildcard,
            ast::Atom::Constant(c) => ir::Atom::Constant(c.internalize(stb)),
            ast::Atom::Variable(c) => ir::Atom::Variable(c.internalize(stb, ridx)),
            ast::Atom::Tuple(args) => {
                ir::Atom::Tuple(args.iter().map(|arg| arg.internalize(stb, ridx)).collect())
            }
        }
    }
}

impl ast::Rule {
    fn internalize(&self, stb: &mut SymbolTableBuilder, ridx: RuleIndex) -> ir::Rule {
        let consequents = self
            .consequents
            .iter()
            .map(|atom| atom.internalize(stb, ridx))
            .collect();
        let [mut pos_antecedents, mut neg_antecedents] = <[Vec<_>; 2]>::default();
        for antecedent in &self.antecedents {
            let antecedents = match antecedent.sign {
                ast::Sign::Pos => &mut pos_antecedents,
                ast::Sign::Neg => &mut neg_antecedents,
            };
            antecedents.push(antecedent.atom.internalize(stb, ridx))
        }
        ir::Rule {
            consequents,
            pos_antecedents,
            neg_antecedents,
        }
    }
}

pub fn internalize_rules(rules: &[ast::Rule]) -> (Vec<ir::Rule>, SymbolTable) {
    let mut stb = SymbolTableBuilder::default();
    let rules = rules
        .iter()
        .enumerate()
        .map(|(ridx, rule)| rule.internalize(&mut stb, ridx))
        .collect();
    (rules, stb.symbol_table)
}
