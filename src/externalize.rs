use crate::internalize::RuleIndex;
use crate::internalize::SymbolTable;
use crate::{ast, ir};

impl ir::Atom {
    pub(crate) fn externalize_concrete(&self, symbol_table: &SymbolTable) -> ast::Atom {
        match self {
            Self::Wildcard => ast::Atom::Wildcard,
            Self::Tuple(args) => ast::Atom::Tuple(
                args.iter()
                    .map(|arg| arg.externalize_concrete(symbol_table))
                    .collect(),
            ),
            Self::Constant(c) => ast::Atom::Constant(
                symbol_table
                    .constants
                    .get(c)
                    .expect("unmapped construct")
                    .clone(),
            ),
            Self::Variable(_) => unreachable!(),
        }
    }
    pub(crate) fn externalize(&self, symbol_table: &SymbolTable, ridx: RuleIndex) -> ast::Atom {
        match self {
            Self::Tuple(args) => ast::Atom::Tuple(
                args.iter()
                    .map(|arg| arg.externalize(symbol_table, ridx))
                    .collect(),
            ),
            Self::Variable(c) => ast::Atom::Variable(
                symbol_table
                    .variables
                    .get(&ridx)
                    .expect("unmapped ridx")
                    .get(c)
                    .expect("unmapped variable")
                    .clone(),
            ),
            _ => self.externalize_concrete(symbol_table),
        }
    }
}
