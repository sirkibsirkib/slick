use crate::ast;
use std::fmt::{Debug, Formatter, Result as FmtResult};

impl Debug for ast::Atom {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        match self {
            Self::Wildcard => write!(f, "_"),
            Self::Constant(c) => c.fmt(f),
            Self::Variable(v) => v.fmt(f),
            Self::Tuple(args) => f.debug_list().entries(args).finish(),
        }
    }
}

impl Debug for ast::Constant {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}
impl Debug for ast::Variable {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        write!(f, "{}", String::from_utf8_lossy(&self.0))
    }
}

impl Debug for ast::Rule {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for (i, consequent) in self.consequents.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            } else {
                write!(f, " ")?;
            }
            write!(f, "{:?}", consequent)?;
        }
        if !self.antecedents.is_empty() {
            write!(f, " :- ")?;
            for (i, antecedent) in self.antecedents.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                if let ast::Sign::Neg = antecedent.sign {
                    write!(f, "!")?;
                }
                write!(f, "{:?}", antecedent.atom)?;
            }
        }
        Ok(())
    }
}
