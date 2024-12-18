use crate::{
    atomlike::AtomLike, lexicographic::Lexicographic, util::pairs, Atom as A, Atom, Check,
    CheckKind, Constant, GroundAtom, GroundAtom as Ga, Program, Rule, Variable,
};

use core::fmt::{Debug, Formatter, Result as FmtResult};

#[derive(Debug)]
pub struct Config {
    pub max_alt_rounds: u16,
    pub max_atom_depth: u16,
    pub max_known_atoms: u32,
    // pub static_rounds: u8,
}

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
pub struct Assignments {
    // invariant: each variable occurs at most once
    vec: Vec<(Variable, GroundAtom)>,
}

#[derive(Clone)]
pub struct VarAssignState(usize);

#[derive(Debug)]
pub struct Denotation {
    pub trues: Vec<GroundAtom>,
    pub unknowns: Vec<GroundAtom>,
}

#[derive(Debug)]
pub struct Interpretation {
    pub trues: GroundAtoms,
    pub prev_trues: GroundAtoms,
}

// #[derive(Clone, Copy, Eq, PartialEq)]
// pub enum InferenceMode {
//     TerminationTest,
//     AlternatingFixpoint,
//     ExtractFacts,
// }

#[derive(Debug)]
pub enum InfereceError {
    InferredAtomExceededMaxDepth(GroundAtom),
    KnowledgeBaseExceededMaxCapacity,
    AlternatingRoundsExceededCap,
}

////////////////////////

impl Default for Config {
    fn default() -> Self {
        Config { max_alt_rounds: 10, max_atom_depth: 10, max_known_atoms: 30_000 }
    }
}

impl Interpretation {
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
    pub fn test(&self, ga: &GroundAtom) -> Option<bool> {
        if self.trues.contains(ga) {
            Some(true)
        } else if self.unknowns.contains(ga) {
            None
        } else {
            Some(false)
        }
    }
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
    // fn same(&self, atoms: &[Atom]) -> bool {
    //     pairs(atoms).all(|[a, b]| a.same(b, self))
    // }
    // fn diff(&self, atoms: &[Atom]) -> bool {
    //     // println!("ASS DIFFS {self:?} {atoms:?}");
    //     pairs(atoms).all(|[a, b]| b.diff(a, self))
    // }
    fn check(&self, check: &Check) -> bool {
        // println!("start {check:?}",);
        let success = match check.kind {
            CheckKind::Same => pairs(&check.atoms).all(|[a, b]| a.same(b, self)),
            CheckKind::Diff => pairs(&check.atoms).all(|[a, b]| a.diff(b, self)),
        };
        // println!("end {check:?}. success? {success}");
        success == check.positive
    }
}

impl GroundAtom {
    fn reflect_within(part_name: GroundAtom, saying: Self) -> Self {
        Self::Tuple(vec![saying.clone(), Self::Constant(Constant::from_str("within")), part_name])
    }
    fn assumes_false(part_name: GroundAtom, atom: Self) -> Self {
        Self::Tuple(vec![
            part_name,
            Self::Constant(Constant::from_str("assumes")),
            atom.clone(),
            Self::Constant(Constant::from_str("is")),
            Self::Constant(Constant::from_str("false")),
        ])
    }
    fn depth(&self) -> usize {
        match self {
            Self::Tuple(args) => args.iter().map(Self::depth).max().map(|x| x + 1).unwrap_or(0),
            _ => 0,
        }
    }
}

impl Atom {
    pub fn same(&self, other: &Self, assignments: &Assignments) -> bool {
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
    pub fn diff(&self, other: &Self, assignments: &Assignments) -> bool {
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
    pub fn consistently_assign<'a, 'b>(
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

    pub fn matches_ground_atom(&self, ga: &GroundAtom) -> bool {
        self.consistently_assign(ga, &mut Default::default())
    }

    pub fn try_concretize(&self, assignments: &Assignments) -> Option<GroundAtom> {
        Some(match self {
            A::Constant(c) => Ga::Constant(c.clone()),
            A::Variable(v) => assignments.get(v)?.clone(),
            A::Wildcard => return None,
            A::Tuple(args) => Ga::Tuple(
                args.iter().map(|arg| arg.try_concretize(assignments)).collect::<Option<_>>()?,
            ),
        })
    }

    // read assignments
    pub fn concretize(&self, assignments: &Assignments) -> GroundAtom {
        match self {
            A::Constant(c) => Ga::Constant(c.clone()),
            A::Variable(v) => assignments.get(v).expect("missing variable!").clone(),
            A::Wildcard => panic!("Cannot concretise wildcard"),
            A::Tuple(args) => {
                Ga::Tuple(args.iter().map(|arg| arg.concretize(assignments)).collect())
            }
        }
    }
}

impl GroundAtoms {
    fn if_new_add_to(&self, ga: GroundAtom, write_buf: &mut Vec<GroundAtom>) -> bool {
        if !self.vec_set.contains(&ga) {
            write_buf.push(ga);
            true
        } else {
            false
        }
    }
}

impl Program {
    // an optimization
    // pub fn extract_facts(&mut self, config: &Config) -> GroundAtoms {
    //     // TODO generalize this to all rules with no vars in consequent AND no positive antecedents
    //     // e.g. `nice if not X cow` should theoretically be just fine
    //     let mut facts = GroundAtoms::default();
    //     let mut write_buf = vec![];
    //     let assignments = Assignments::default();
    //     let mode = InferenceMode::ExtractFacts;
    //     self.rules.retain(|rule| {
    //         let depends_on_kb = !rule.rule_body.pos_antecedents.is_empty()
    //             || !rule.rule_body.neg_antecedents.is_empty();
    //         if !depends_on_kb {
    //             // this rule will be discarded after its fact is extracted
    //             if !rule.rule_body.checks.iter().all(|check| assignments.check(check)) {
    //                 return false;
    //             }
    //             rule.infer_consequents(config, &facts, &assignments, mode, &mut write_buf)
    //                 .expect("no errs");
    //             // for consequent in &rule.consequents {
    //             //     let ga = consequent.concretize(&assignments);
    //             //     facts.infer_new(&mut write_buf, ga, rule.part_name.as_ref());
    //             // }
    //         }
    //         depends_on_kb
    //     });
    //     facts.vec_set.extend(write_buf);
    //     facts
    // }
    // pub fn termination_test(&self, config: &Config) -> Result<(), InfereceError> {
    //     let mut test_program =
    //         Self { rules: self.rules.iter().map(Rule::without_neg_antecedents).collect() };
    //     let test_facts = test_program.extract_facts(config);
    //     println!("TEST FAX {:#?}", test_facts);
    //     println!("TEST RLZ {:#?}", test_program.rules);
    //     let mut write_buf = Default::default();
    //     let mut assignments = Default::default();
    //     test_program.big_step(
    //         config,
    //         test_facts,
    //         NegKnowledge::Empty,
    //         &mut write_buf,
    //         &mut assignments,
    //         InferenceMode::TerminationTest,
    //     )?;
    //     Ok(())
    // }

    // pub fn pseudo_static_error_test(&self, config: &Config) -> Result<(), (u8, GroundAtoms)> {
    //     let mut static_program = Self {
    //         rules: self
    //             .rules
    //             .iter()
    //             .filter(|rule| rule.rule_body.neg_antecedents.is_empty())
    //             .cloned()
    //             .collect(),
    //     };
    //     let mut atoms = static_program.extract_facts(config);
    //     println!("STATIC FAX {:#?}", atoms);
    //     println!("STATIC RLZ {:#?}", static_program.rules);
    //     let mut write_buf = vec![];
    //     let mut assignments = Assignments::default();
    //     for round in 0..config.static_rounds {
    //         for rule in &self.rules {
    //             let _ = rule.big_step_rec(
    //                 config,
    //                 &atoms,
    //                 &mut write_buf,
    //                 NegKnowledge::Empty,
    //                 &mut assignments,
    //                 rule.rule_body.pos_antecedents.as_slice(),
    //                 InferenceMode::TerminationTest,
    //             );
    //         }
    //         if write_buf.is_empty() {
    //             return Ok(());
    //         }
    //         atoms.vec_set.extend(write_buf.drain(..));
    //         if atoms.vec_set.contains(&GroundAtom::error()) {
    //             return Err((round, atoms));
    //         }
    //     }
    //     Ok(())
    // }

    pub fn denotation(self, config: &Config) -> Result<Denotation, InfereceError> {
        self.alternating_fixpoint(config).map(Interpretation::to_denotation)
    }

    /// Implements an adaptation of the alternating fixpoint semantics of Van Gelder et al.
    pub fn alternating_fixpoint(&self, config: &Config) -> Result<Interpretation, InfereceError> {
        // let facts = self.extract_facts(config);
        let program = &self;
        // println!("FAX {:#?}", facts);
        println!("RLZ {:#?}", program.rules);
        // let mode = InferenceMode::AlternatingFixpoint;
        let mut write_buf = Default::default();
        let mut assignments = Default::default();
        let mut vec = Vec::with_capacity(8);
        while vec.len() < (config.max_alt_rounds as usize) {
            // println!("\nnext inference round");
            let nk = match &mut vec[..] {
                [prefix @ .., a, b, c] if prefix.len() % 2 == 0 && a == c => {
                    let trues = std::mem::take(a);
                    let prev_trues = std::mem::take(b);
                    // unknowns.retain(|x| !trues.contains(x));
                    return Ok(Interpretation { trues, prev_trues });
                }
                [] => NegKnowledge::Empty,
                [.., a] => NegKnowledge::ComplementOf(a),
            };
            let b = self.big_step(
                config,
                // facts.clone(),
                GroundAtoms::default(),
                nk,
                &mut write_buf,
                &mut assignments,
                // mode,
            )?;
            vec.push(b);
        }
        Err(InfereceError::AlternatingRoundsExceededCap)
    }

    fn big_step(
        &self,
        config: &Config,
        mut atoms: GroundAtoms,
        nk: NegKnowledge,
        write_buf: &mut Vec<GroundAtom>,
        assignments: &mut Assignments,
        // mode: InferenceMode,
    ) -> Result<GroundAtoms, InfereceError> {
        assert!(write_buf.is_empty());
        assert!(assignments.vec.is_empty());
        loop {
            for rule in &self.rules {
                rule.big_step_rec(
                    config,
                    &atoms,
                    write_buf,
                    nk,
                    assignments,
                    rule.rule_body.pos_antecedents.as_slice(),
                    // mode,
                )?
            }
            let done = write_buf.is_empty();
            atoms.vec_set.extend(write_buf.drain(..));
            // if mode == InferenceMode::TerminationTest {
            if (config.max_known_atoms as usize) < atoms.vec_set.as_slice().len() {
                return Err(InfereceError::KnowledgeBaseExceededMaxCapacity);
            }
            // }
            // println!("ATOMS {atoms:#?}");
            if done {
                return Ok(atoms);
            }
        }
    }
}

impl Rule {
    fn infer_consequents(
        &self,
        config: &Config,
        read: &GroundAtoms,
        assignments: &Assignments,
        // mode: InferenceMode,
        write_buf: &mut Vec<GroundAtom>,
    ) -> Result<(), InfereceError> {
        for consequent in &self.consequents {
            let ga = consequent.concretize(&assignments);
            if (config.max_atom_depth as usize) < ga.depth() {
                return Err(InfereceError::InferredAtomExceededMaxDepth(ga));
            }
            // if let Some(rule_within) = &self.rule_within {
            //     let ga2 = Ga::reflect_within(rule_within.clone(), ga.clone());
            //     read.if_new_add_to(ga2, write_buf);

            //     for atom in &self.rule_body.neg_antecedents {
            //         let n_ga = atom.concretize(&assignments);
            //         let ga2 = Ga::assumes_false(rule_within.clone(), n_ga);
            //         read.if_new_add_to(ga2, write_buf);
            //     }
            // }
            read.if_new_add_to(ga, write_buf);
        }
        Ok(())
    }
    fn big_step_rec(
        &self,
        config: &Config,
        read: &GroundAtoms,
        write_buf: &mut Vec<GroundAtom>,
        nk: NegKnowledge,
        assignments: &mut Assignments,
        pos_antecedents_to_go: &[Atom],
    ) -> Result<(), InfereceError> {
        if let [next, rest @ ..] = pos_antecedents_to_go {
            if let Some(ga) = next.try_as_ground_atom() {
                // optimization!
                if read.vec_set.contains(ga) {
                    self.big_step_rec(config, read, write_buf, nk, assignments, rest)?
                }
                return Ok(());
            }
            // continue case
            let state = assignments.save_state();
            for ga in read.vec_set.as_slice().iter().rev() {
                if next.consistently_assign(ga, assignments) {
                    self.big_step_rec(config, read, write_buf, nk, assignments, rest)?
                }
                assignments.restore_state(state.clone());
            }
            return Ok(());
        }
        // stop condition!

        if !self.rule_body.checks.iter().all(|check| assignments.check(check)) {
            return Ok(());
        }
        let false_check_ok = match nk {
            // ga is false if it was not previously true
            NegKnowledge::Empty => self.rule_body.neg_antecedents.is_empty(),
            NegKnowledge::ComplementOf(kb) => {
                // neg ok
                self.rule_body
                    .neg_antecedents
                    .iter()
                    .all(|atom| !kb.vec_set.contains(&atom.concretize(&assignments)))
            }
        };
        if !false_check_ok {
            return Ok(());
        }
        self.infer_consequents(config, read, assignments, write_buf)
    }
}

impl Debug for Assignments {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        f.debug_map().entries(self.vec.iter().map(|(a, b)| (a, b))).finish()
    }
}
