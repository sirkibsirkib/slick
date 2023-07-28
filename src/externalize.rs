use crate::internalize::RuleIndex;
use crate::internalize::SymbolTable;
use crate::{ast, ir};

impl ir::Constant {
    fn externalize(&self, symbol_table: &SymbolTable) -> ast::Constant {
        symbol_table
            .constants
            .get(self)
            .expect("unmapped construct")
            .clone()
    }
}
impl ir::Variable {
    pub(crate) fn externalize(&self, symbol_table: &SymbolTable, ridx: RuleIndex) -> ast::Variable {
        symbol_table
            .variables
            .get(&ridx)
            .expect("unmapped ridx")
            .get(self)
            .expect("unmapped variable")
            .clone()
    }
}
impl ir::Atom {
    pub(crate) fn externalize_concrete(&self, symbol_table: &SymbolTable) -> ast::Atom {
        match self {
            Self::Wildcard => ast::Atom::Wildcard,
            Self::Tuple(args) => ast::Atom::Tuple(
                args.iter()
                    .map(|arg| arg.externalize_concrete(symbol_table))
                    .collect(),
            ),
            Self::Constant(c) => ast::Atom::Constant(c.externalize(symbol_table)),
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
            Self::Variable(v) => ast::Atom::Variable(v.externalize(symbol_table, ridx)),
            _ => self.externalize_concrete(symbol_table),
        }
    }
}
