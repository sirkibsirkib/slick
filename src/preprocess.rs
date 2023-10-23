pub fn comments_removed(mut s: String) -> String {
    #[derive(Copy, Clone)]
    enum State {
        Outside,
        LineComment,
        BlockComment,
    }
    use State::*;
    let mut state = Outside;
    s.retain(|c| {
        let (new_state, retain) = match (state, &c) {
            (Outside, '#') => (LineComment, false),
            (Outside, '<') => (BlockComment, false),
            (LineComment, '\n') => (Outside, true),
            (BlockComment, '>') => (Outside, false),
            (Outside, _) => (Outside, true),
            (s, _) => (
                s,
                match s {
                    Outside => true,
                    LineComment | BlockComment => false,
                },
            ),
        };
        state = new_state;
        retain
    });
    s
}
