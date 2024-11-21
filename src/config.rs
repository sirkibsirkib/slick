#[derive(Debug)]
pub struct RunConfig {
    pub max_alt_rounds: u16,
    pub max_atom_depth: u16,
    pub max_known_atoms: u32,
    pub static_rounds: u8,
}

impl Default for RunConfig {
    fn default() -> Self {
        RunConfig {
            max_alt_rounds: 10,
            max_atom_depth: 10,
            max_known_atoms: 30_000,
            static_rounds: 3,
        }
    }
}
