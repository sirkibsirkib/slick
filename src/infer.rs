use crate::ast::AtomLike;
use crate::ast::{Atom, GroundAtom, Program, Rule};
use crate::ast::{Constant, Variable};
use crate::util::pairs;

#[derive(Default, Clone, Eq, PartialEq)]
pub struct GroundAtoms {
    pub vec_set: crate::util::VecSet<GroundAtom>,
}

#[derive(Copy, Clone)]
pub enum NegKnowledge<'a> {
    Empty,
    ComplementOf(&'a GroundAtoms),
}

#[derive(Default, Debug)]
struct Assignments {
    // invariant: each variable occurs at most once
    vec: Vec<(Variable, GroundAtom)>,
}

#[derive(Clone)]
struct VarAssignState(usize);
#[derive(Debug)]
pub struct Denotation {
    pub trues: GroundAtoms,
    pub prev_trues: GroundAtoms,
}

////////////////////////
impl Assignments {
    fn save_state(&self) -> VarAssignState {
        VarAssignState(self.vec.len())
    }
    fn restore_state(&mut self, state: VarAssignState) {
        self.vec.truncate(state.0)
    }
    fn try_assign(&mut self, var: &Variable, new: &GroundAtom) -> bool {
        match self.get(var) {
            Some(old) => old == new,
            None => {
                self.vec.push((var.clone(), new.clone()));
                true
            }
        }
    }
    fn get(&self, var: &Variable) -> Option<&GroundAtom> {
        self.vec.iter().filter_map(|(var2, val)| if var == var2 { Some(val) } else { None }).next()
    }
    fn same(&self, atoms: &[Atom]) -> bool {
        pairs(atoms).all(|[a, b]| a.same(b, self))
    }
    fn diff(&self, atoms: &[Atom]) -> bool {
        pairs(atoms).all(|[a, b]| b.diff(a, self))
    }
}

impl GroundAtom {
    fn says(part_name: GroundAtom, saying: Self) -> Self {
        Self::Tuple(vec![part_name, Self::Constant(Constant::from_str("says")), saying.clone()])
    }
    fn depth(&self) -> usize {
        match self {
            Self::Tuple(args) => args.iter().map(Self::depth).max().map(|x| x + 1).unwrap_or(0),
            _ => 0,
        }
    }
}

impl Atom {
    fn same(&self, other: &Self, assignments: &Assignments) -> bool {
        use Atom::*;
        match [self, other] {
            [Variable(var), x] | [x, Variable(var)] => {
                assignments.get(var).map(AtomLike::as_atom).unwrap().same(x, assignments)
            }
            [Wildcard, _] | [_, Wildcard] => true,
            [Constant(x), Constant(y)] => x == y,
            [Tuple(x), Tuple(y)] => {
                x.len() == y.len() && x.iter().zip(y.iter()).all(|(x, y)| x.same(y, assignments))
            }
            _ => true,
        }
    }
    fn diff(&self, other: &Self, assignments: &Assignments) -> bool {
        use Atom::*;
        match [self, other] {
            [Variable(var), x] | [x, Variable(var)] => {
                assignments.get(var).map(AtomLike::as_atom).unwrap().same(x, assignments)
            }
            [Wildcard, _] | [_, Wildcard] => true,
            [Constant(x), Constant(y)] => x != y,
            [Tuple(x), Tuple(y)] => {
                x.len() != y.len() || x.iter().zip(y.iter()).any(|(x, y)| x.diff(y, assignments))
            }
            _ => true,
        }
    }

    // write assignments
    fn consistently_assign<'a, 'b>(
        &'a self,
        ga: &'a GroundAtom,
        assignments: &'b mut Assignments,
    ) -> bool {
        match (self, ga) {
            (Self::Variable(var), _) => assignments.try_assign(var, ga),
            (Self::Wildcard, _) => true,
            (Self::Constant(x), GroundAtom::Constant(y)) => x == y,
            (Self::Tuple(x), GroundAtom::Tuple(y)) if x.len() == y.len() => {
                x.iter().zip(y.iter()).all(|(x, y)| x.consistently_assign(y, assignments))
            }
            _ => false,
        }
    }

    // read assignments
    fn concretize(&self, assignments: &Assignments) -> GroundAtom {
        match self {
            Self::Constant(c) => GroundAtom::Constant(c.clone()),
            Self::Variable(v) => assignments.get(v).expect("gotta").clone(),
            Self::Wildcard => unreachable!(),
            Self::Tuple(args) => {
                GroundAtom::Tuple(args.iter().map(|arg| arg.concretize(assignments)).collect())
            }
        }
    }
}

impl GroundAtoms {
    fn infer_new(
        &self,
        write_buf: &mut Vec<GroundAtom>,
        ga: GroundAtom,
        part_name: Option<&GroundAtom>,
    ) {
        if let Some(part_name) = part_name {
            let ga2 = GroundAtom::says(part_name.clone(), ga.clone());
            if !self.vec_set.contains(&ga2) {
                write_buf.push(ga2);
            } else {
                return;
            }
        }

        if !self.vec_set.contains(&ga) {
            write_buf.push(ga);
        }
    }
}

impl Program {
    pub fn extract_facts(&mut self) -> GroundAtoms {
        let mut facts = GroundAtoms::default();
        let mut write_buf = vec![];
        let assignments = Assignments::default();
        self.rules.retain(|rule| {
            let rule_is_fact = rule.pos_antecedents.is_empty() && rule.neg_antecedents.is_empty();
            if rule_is_fact {
                if !rule.same_sets.iter().all(|x| assignments.same(x)) {
                    return false;
                }
                if !rule.diff_sets.iter().all(|x| assignments.diff(x)) {
                    return false;
                }
                for consequent in &rule.consequents {
                    let ga = consequent.concretize(&assignments);
                    facts.infer_new(&mut write_buf, ga, rule.part_name.as_ref());
                }
            }
            !rule_is_fact
        });
        facts.vec_set.extend(write_buf);
        facts
    }
    pub fn termination_test(&self, max_depth: usize) -> Result<(), GroundAtom> {
        let mut result = Ok(());
        let store_counterexample = &mut |atom: &GroundAtom| {
            if max_depth < atom.depth() {
                result = Err(atom.clone());
                true
            } else {
                false
            }
        };
        let mut test_program =
            Self { rules: self.rules.iter().map(Rule::without_neg_antecedents).collect() };
        let test_facts = test_program.extract_facts();
        println!("TEST FAX {:#?}", test_facts);
        println!("TEST RLZ {:#?}", test_program.rules);
        let mut write_buf = Default::default();
        let mut assignments = Default::default();
        test_program.big_step(
            test_facts,
            NegKnowledge::Empty,
            &mut write_buf,
            &mut assignments,
            store_counterexample,
        );
        result
    }

    pub fn alternating_fixpoint(mut self) -> Denotation {
        let facts = self.extract_facts();
        let program = &self;
        println!("FAX {:#?}", facts);
        println!("RLZ {:#?}", program.rules);
        let mut write_buf = Default::default();
        let mut assignments = Default::default();
        let mut vec = vec![self.big_step(
            facts.clone(),
            NegKnowledge::Empty,
            &mut write_buf,
            &mut assignments,
            &mut |_| false,
        )];
        loop {
            match &mut vec[..] {
                [] => unreachable!(),
                [prefix @ .., a, b, c] if prefix.len() % 2 == 0 && a == c => {
                    let trues = std::mem::take(a);
                    let prev_trues = std::mem::take(b);
                    // unknowns.retain(|x| !trues.contains(x));
                    return Denotation { trues, prev_trues };
                }
                [.., a] => {
                    let b = self.big_step(
                        facts.clone(),
                        NegKnowledge::ComplementOf(a),
                        &mut write_buf,
                        &mut assignments,
                        &mut |_| false,
                    );
                    vec.push(b);
                }
            }
        }
    }

    fn big_step(
        &self,
        mut atoms: GroundAtoms,
        nk: NegKnowledge,
        write_buf: &mut Vec<GroundAtom>,
        assignments: &mut Assignments,
        halter: &mut impl FnMut(&GroundAtom) -> bool,
    ) -> GroundAtoms {
        assert!(write_buf.is_empty());
        assert!(assignments.vec.is_empty());
        loop {
            for rule in &self.rules {
                rule.big_step_rec(
                    &atoms,
                    write_buf,
                    nk,
                    assignments,
                    rule.pos_antecedents.as_slice(),
                    halter,
                )
            }
            if write_buf.is_empty() {
                return atoms;
            }
            atoms.vec_set.extend(write_buf.drain(..))
        }
    }
}

/*

struct RecData<'a, H: FnMut(&GroundAtom) -> bool> {
    rule: &'a Rule,
    read: &'a GroundAtoms,
    write_buf: &'a mut Vec<GroundAtom>,
    nk: NegKnowledge<'a>,
    assignments: &'a mut Assignments,
    halter: H,
}

impl<H: FnMut(&GroundAtom) -> bool> RecData<'_, H> {
    fn big_step(mut self, mut atoms: GroundAtoms) -> GroundAtoms {
        // check invariant
        assert!(self.write_buf.is_empty());
        assert!(self.assignments.vec.is_empty());
        loop {
            for rule in &self.rules {
                self.big_step_rec(&atoms, rule.pos_antecedents.as_slice())
            }
            if write_buf.is_empty() {
                return atoms;
            }
            for x in write_buf.drain(..) {
                assert!(atoms.vec_set.insert(x));
            }
        }
        atoms
    }
    fn big_step_rec(&mut self, read: &GroundAtoms, pos_antecedents_to_go: &[Atom]) {
        if let [next, rest @ ..] = pos_antecedents_to_go {
            // continue case
            let state = self.assignments.save_state();
            for ga in self.read.vec_set.as_slice() {
                if next.consistently_assign(ga, self.assignments) {
                    self.big_step_rec(read, rest);
                }
                self.assignments.restore_state(state.clone());
            }
            return;
        }
        // stop condition!

        if !self.rule.same_sets.iter().all(|x| self.assignments.same(x)) {
            return;
        }
        if !self.rule.diff_sets.iter().all(|x| self.assignments.diff(x)) {
            return;
        }
        let false_check_ok = match self.nk {
            NegKnowledge::Empty => self.rule.neg_antecedents.is_empty(),
            NegKnowledge::ComplementOf(kb) => self
                .rule
                .neg_antecedents
                .iter()
                .all(|test| kb.vec_set.contains(&test.concretize(&self.assignments))),
        };
        if !false_check_ok {
            return;
        }
        for consequent in &self.rule.consequents {
            let atom = consequent.concretize(&self.assignments);
            if (self.halter)(&atom) {
                return;
            }
            let atom2 = GroundAtom::says(self.rule.part_name.as_ref(), atom.clone());
            if !self.read.vec_set.contains(&atom) {
                self.write_buf.push(atom);
                if !self.read.vec_set.contains(&atom2) {
                    self.write_buf.push(atom2)
                }
            }
        }
    }
}
*/
impl Rule {
    fn big_step_rec(
        &self,
        read: &GroundAtoms,
        write_buf: &mut Vec<GroundAtom>,
        nk: NegKnowledge,
        assignments: &mut Assignments,
        pos_antecedents_to_go: &[Atom],
        halter: &mut impl FnMut(&GroundAtom) -> bool,
    ) {
        if let [next, rest @ ..] = pos_antecedents_to_go {
            // continue case
            let state = assignments.save_state();
            for ga in read.vec_set.as_slice() {
                if next.consistently_assign(ga, assignments) {
                    self.big_step_rec(read, write_buf, nk, assignments, rest, halter);
                }
                assignments.restore_state(state.clone());
            }
            return;
        }
        // stop condition!

        if !self.same_sets.iter().all(|x| assignments.same(x)) {
            return;
        }
        if !self.diff_sets.iter().all(|x| assignments.diff(x)) {
            return;
        }
        let false_check_ok = match nk {
            NegKnowledge::Empty => self.neg_antecedents.is_empty(),
            NegKnowledge::ComplementOf(kb) => self
                .neg_antecedents
                .iter()
                .all(|test| kb.vec_set.contains(&test.concretize(&assignments))),
        };
        if !false_check_ok {
            return;
        }
        for consequent in &self.consequents {
            let ga = consequent.concretize(&assignments);
            if halter(&ga) {
                return;
            }
            read.infer_new(write_buf, ga, self.part_name.as_ref());
            // let atom2 = GroundAtom::says(self.part_name.as_ref(), atom.clone());
            // if !read.vec_set.contains(&atom) {
            //     write_buf.push(atom);
            //     if !read.vec_set.contains(&atom2) {
            //         write_buf.push(atom2)
            //     }
            // }
        }
    }
}
