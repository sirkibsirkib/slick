use crate::text::Text;

struct GroundAtoms {
    tuple_len_to_arg_buf: Vec<Vec<TupleArg>>,
}
enum TupleArg {
    Constant(Text),
    Tuple { width: u8, suffix: u16 },
}
