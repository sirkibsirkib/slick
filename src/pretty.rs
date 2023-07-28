use crate::{infer::Atoms, internalize::SymbolTable, ir::Atom};
use std::fmt::{Debug, Formatter, Result as FmtResult};

pub(crate) struct Pretty<'a, T> {
    pub(crate) t: &'a T,
    pub(crate) symbol_table: &'a SymbolTable,
}

impl Debug for Pretty<'_, Atoms> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        f.debug_set()
            .entries(self.t.iter().map(|atom| Pretty {
                t: atom,
                symbol_table: self.symbol_table,
            }))
            .finish()
    }
}

impl Debug for Pretty<'_, Atom> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self.t {
            Atom::Wildcard => write!(f, "_"),
            Atom::Constant(c) => match self.symbol_table.constants.get(c) {
                Some(c) => write!(f, "{}", c),
                None => write!(f, "?"),
            },
            Atom::Variable(_) => unreachable!(),
            Atom::Tuple(args) => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(
                        f,
                        "{:?}",
                        Pretty {
                            t: arg,
                            symbol_table: self.symbol_table,
                        }
                    )?;
                }
                write!(f, ")")
            }
        }
    }
}
