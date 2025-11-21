#[derive(Debug)]
pub struct RunConfig {
    pub max_alt_rounds: u16,
    pub max_atom_depth: u16,
    pub max_known_atoms: u32,
    pub static_rounds: u8,
}
pub const RUN_CONFIG: RunConfig = RunConfig {
    max_alt_rounds: 8,
    max_atom_depth: u16::MAX,
    max_known_atoms: 30_000,
    static_rounds: 3,
};
