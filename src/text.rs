use crate::ast::Lexicographic;
use crate::util::lexicographic;
use core::cmp::Ordering;
use core::fmt::{Debug, Formatter};

use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};

type TextIndex = u16;

#[derive(Hash, PartialOrd, Ord, Eq, PartialEq, Copy, Clone)]
pub struct Text(TextIndex);

struct AnnotatedString {
    string: String,
    contains_whitespace: bool,
}

#[derive(Default)]
pub struct TextMap {
    string_to_index: HashMap<String, TextIndex>,
    index_to_annotated_string: Vec<AnnotatedString>,
}

impl AnnotatedString {
    fn new(string: String) -> Self {
        Self { contains_whitespace: string.chars().any(char::is_whitespace), string }
    }
}

impl TextMap {
    fn insert_str(&mut self, val: &str) -> TextIndex {
        if let Some(&idx) = self.string_to_index.get(val) {
            idx
        } else {
            let idx =
                self.index_to_annotated_string.len().try_into().expect("insufficient keysize!");
            self.string_to_index.insert(val.to_string(), idx);
            self.index_to_annotated_string.push(AnnotatedString::new(val.to_string()));
            idx
        }
    }
    fn get_annotated_string(&self, idx: TextIndex) -> &AnnotatedString {
        &self.index_to_annotated_string[idx as usize]
    }
    fn get_string(&self, idx: TextIndex) -> &str {
        &self.get_annotated_string(idx).string
    }
}

static TEXT_MAP: OnceLock<RwLock<TextMap>> = OnceLock::new();

impl Text {
    pub fn from_str(s: &str) -> Self {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &mut TextMap = &mut lock.write().expect("poisoned");
        Self(map.insert_str(s))
    }
    pub fn print_text_table() {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &TextMap = &lock.read().expect("poisoned");
        for (i, annotated_string) in map.index_to_annotated_string.iter().enumerate() {
            println!("{i:>5}  {annotated_string:?}");
        }
    }
}

impl Debug for Text {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &TextMap = &lock.read().expect("poisoned");
        map.get_annotated_string(self.0).fmt(f)
    }
}

impl Debug for AnnotatedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.contains_whitespace {
            write!(f, "{:?}", &self.string)
        } else {
            write!(f, "{}", &self.string)
        }
    }
}

impl Lexicographic for Text {
    fn rightward_flat_constants(&self, other: &Self) -> Ordering {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &TextMap = &lock.read().expect("poisoned");
        map.get_string(self.0).cmp(map.get_string(other.0))
    }
    fn rightward_lexicographic(&self, other: &Self) -> Ordering {
        self.rightward_flat_constants(other)
    }
    fn rightward_integer(&self, other: &Self) -> Ordering {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &TextMap = &lock.read().expect("poisoned");
        let [a, b] = [map.get_string(self.0), map.get_string(other.0)];
        let [la, lb] = [a.chars().count(), b.chars().count()];
        if la <= lb {
            let a_prefix = std::iter::repeat('0').take(lb - la);
            lexicographic(a_prefix.chain(a.chars()), b.chars())
        } else {
            let b_prefix = std::iter::repeat('0').take(la - lb);
            lexicographic(a.chars(), b_prefix.chain(a.chars()))
        }
    }
}
