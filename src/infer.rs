use crate::ast::AtomLike;
use crate::ast::Lexicographic;
use crate::ast::{
    Atom as A, Atom, Constant, GroundAtom, GroundAtom as Ga, Program, Rule, Variable,
};
use crate::{util::pairs, RUN_CONFIG};

use core::fmt::{Debug, Formatter, Result as FmtResult};

#[derive(Default, Clone, Eq, PartialEq)]
pub struct GroundAtoms {
    pub vec_set: crate::util::VecSet<GroundAtom>,
}

#[derive(Copy, Clone)]
pub enum NegKnowledge<'a> {
    Empty,
    ComplementOf(&'a GroundAtoms),
}

#[derive(Default)]
struct Assignments {
    // invariant: each variable occurs at most once
    vec: Vec<(Variable, GroundAtom)>,
}

#[derive(Clone)]
struct VarAssignState(usize);

#[derive(Debug)]
pub struct Denotation {
    pub trues: Vec<GroundAtom>,
    pub unknowns: Vec<GroundAtom>,
}

#[derive(Debug)]
pub struct RawDenotation {
    trues: GroundAtoms,
    prev_trues: GroundAtoms,
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum InferenceMode {
    TerminationTest,
    AlternatingFixpoint,
}

#[derive(Debug)]
pub enum InfereceError {
    InferredAtomExceededMaxDepth(GroundAtom),
    KnowledgeBaseExceededMaxCapacity,
    AlternatingRoundsExceededCap,
}

////////////////////////
impl RawDenotation {
    pub fn test(&self, ga: &GroundAtom) -> Option<bool> {
        if self.trues.vec_set.contains(ga) {
            Some(true)
        } else if self.prev_trues.vec_set.contains(ga) {
            None
        } else {
            Some(false)
        }
    }
    pub fn to_denotation(self) -> Denotation {
        let mut unknowns = self.prev_trues.vec_set.to_vec();
        unknowns.retain(|ga| !self.trues.vec_set.contains(ga));
        unknowns.sort_by(GroundAtom::rightward_flat_constants);
        let mut trues = self.trues.vec_set.to_vec();
        trues.sort_by(GroundAtom::rightward_flat_constants);
        Denotation { trues, unknowns }
    }
}
impl Denotation {
    pub fn hide_unshown(mut self) -> Self {
        let show = GroundAtom::Constant(Constant::from_str("show"));
        let f = |ga: GroundAtom| {
            if let Ga::Tuple(mut args) = ga {
                if args.len() == 2 && args[0] == show {
                    return args.pop();
                }
            }
            None
        };
        Self {
            trues: self.trues.drain(..).filter_map(f).collect(),
            unknowns: self.unknowns.drain(..).filter_map(f).collect(),
        }
    }
}
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
        // println!("ASS DIFFS {self:?} {atoms:?}");
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
        match [self, other] {
            [A::Variable(var), x] | [x, A::Variable(var)] => {
                assignments.get(var).map(AtomLike::as_atom).unwrap().same(x, assignments)
            }
            [A::Wildcard, _] | [_, A::Wildcard] => true,
            [A::Constant(x), A::Constant(y)] => x == y,
            [A::Tuple(x), A::Tuple(y)] => {
                x.len() == y.len() && x.iter().zip(y).all(|(x, y)| x.same(y, assignments))
            }
            _ => false,
        }
    }
    fn diff(&self, other: &Self, assignments: &Assignments) -> bool {
        // println!("DIFF {self:?} {other:?} {assignments:?}");
        match [self, other] {
            [A::Variable(var), x] | [x, A::Variable(var)] => {
                assignments.get(var).map(AtomLike::as_atom).unwrap().diff(x, assignments)
            }
            [A::Wildcard, _] | [_, A::Wildcard] => true,
            [A::Constant(x), A::Constant(y)] => x != y,
            [A::Tuple(x), A::Tuple(y)] => {
                x.len() != y.len() || x.iter().zip(y).any(|(x, y)| x.diff(y, assignments))
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
            (A::Variable(var), _) => assignments.try_assign(var, ga),
            (A::Wildcard, _) => true,
            (A::Constant(x), Ga::Constant(y)) => x == y,
            (A::Tuple(x), Ga::Tuple(y)) if x.len() == y.len() => {
                x.iter().zip(y).all(|(x, y)| x.consistently_assign(y, assignments))
            }
            _ => false,
        }
    }

    // read assignments
    fn concretize(&self, assignments: &Assignments) -> GroundAtom {
        match self {
            A::Constant(c) => Ga::Constant(c.clone()),
            A::Variable(v) => assignments.get(v).expect("missing variable!").clone(),
            A::Wildcard => unreachable!(),
            A::Tuple(args) => {
                Ga::Tuple(args.iter().map(|arg| arg.concretize(assignments)).collect())
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
            let ga2 = Ga::says(part_name.clone(), ga.clone());
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
    // an optimization
    pub fn extract_facts(&mut self) -> GroundAtoms {
        // TODO generalize this to all rules with no vars in consequent AND no positive antecedents
        // e.g. `nice if not X cow` should theoretically be just fine
        let mut facts = GroundAtoms::default();
        let mut write_buf = vec![];
        let assignments = Assignments::default();
        self.rules.retain(|rule| {
            let depends_on_kb =
                !rule.pos_antecedents.is_empty() || !rule.neg_antecedents.is_empty();
            if !depends_on_kb {
                // this rule will be discarded after its fact is extracted
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
            depends_on_kb
        });
        facts.vec_set.extend(write_buf);
        facts
    }
    pub fn termination_test(&self) -> Result<(), InfereceError> {
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
            InferenceMode::TerminationTest,
        )?;
        Ok(())
    }

    pub fn alternating_fixpoint(mut self) -> Result<RawDenotation, InfereceError> {
        let facts = self.extract_facts();
        let program = &self;
        println!("FAX {:#?}", facts);
        println!("RLZ {:#?}", program.rules);
        let mode = InferenceMode::AlternatingFixpoint;
        let mut write_buf = Default::default();
        let mut assignments = Default::default();
        let mut vec = vec![self.big_step(
            facts.clone(),
            NegKnowledge::Empty,
            &mut write_buf,
            &mut assignments,
            mode,
        )?];
        while vec.len() < (RUN_CONFIG.max_alt_rounds as usize) {
            // println!("\nnext inference round");
            match &mut vec[..] {
                [] => unreachable!(),
                [prefix @ .., a, b, c] if prefix.len() % 2 == 0 && a == c => {
                    let trues = std::mem::take(a);
                    let prev_trues = std::mem::take(b);
                    // unknowns.retain(|x| !trues.contains(x));
                    return Ok(RawDenotation { trues, prev_trues });
                }
                [.., a] => {
                    let b = self.big_step(
                        facts.clone(),
                        NegKnowledge::ComplementOf(a),
                        &mut write_buf,
                        &mut assignments,
                        mode,
                    )?;
                    vec.push(b);
                }
            }
        }
        Err(InfereceError::AlternatingRoundsExceededCap)
    }

    fn big_step(
        &self,
        mut atoms: GroundAtoms,
        nk: NegKnowledge,
        write_buf: &mut Vec<GroundAtom>,
        assignments: &mut Assignments,
        mode: InferenceMode,
    ) -> Result<GroundAtoms, InfereceError> {
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
                    mode,
                )?
            }
            let done = write_buf.is_empty();
            atoms.vec_set.extend(write_buf.drain(..));
            if mode == InferenceMode::TerminationTest {
                if (RUN_CONFIG.max_known_atoms as usize) < atoms.vec_set.as_slice().len() {
                    return Err(InfereceError::KnowledgeBaseExceededMaxCapacity);
                }
            }
            // println!("ATOMS {atoms:#?}");
            if done {
                return Ok(atoms);
            }
        }
    }
}

impl Rule {
    fn big_step_rec(
        &self,
        read: &GroundAtoms,
        write_buf: &mut Vec<GroundAtom>,
        nk: NegKnowledge,
        assignments: &mut Assignments,
        pos_antecedents_to_go: &[Atom],
        mode: InferenceMode,
    ) -> Result<(), InfereceError> {
        if let [next, rest @ ..] = pos_antecedents_to_go {
            if let Some(ga) = next.try_as_ground_atom() {
                // optimization!
                if read.vec_set.contains(ga) {
                    self.big_step_rec(read, write_buf, nk, assignments, rest, mode)?
                }
                return Ok(());
            }
            // continue case
            let state = assignments.save_state();
            for ga in read.vec_set.as_slice().iter().rev() {
                if next.consistently_assign(ga, assignments) {
                    self.big_step_rec(read, write_buf, nk, assignments, rest, mode)?
                }
                assignments.restore_state(state.clone());
            }
            return Ok(());
        }
        // stop condition!

        if !self.same_sets.iter().all(|x| assignments.same(x)) {
            return Ok(());
        }
        if !self.diff_sets.iter().all(|x| assignments.diff(x)) {
            return Ok(());
        }
        let false_check_ok = match nk {
            // ga is false if it was not previously true
            NegKnowledge::Empty => self.neg_antecedents.is_empty(),
            NegKnowledge::ComplementOf(kb) => {
                // neg ok
                self.neg_antecedents
                    .iter()
                    .all(|atom| !kb.vec_set.contains(&atom.concretize(&assignments)))
            }
        };
        if !false_check_ok {
            return Ok(());
        }
        for consequent in &self.consequents {
            let ga = consequent.concretize(&assignments);
            if mode == InferenceMode::TerminationTest {
                if (RUN_CONFIG.max_atom_depth as usize) < ga.depth() {
                    return Err(InfereceError::InferredAtomExceededMaxDepth(ga));
                }
            }
            read.infer_new(write_buf, ga, self.part_name.as_ref());
        }
        Ok(())
    }
}

impl Debug for Assignments {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_map().entries(self.vec.iter().map(|(a, b)| (a, b))).finish()
    }
}
