use core::fmt::{Debug, Formatter};
use std::collections::HashMap;
use std::sync::{OnceLock, RwLock};

type TextIndex = u16;

#[derive(Hash, PartialOrd, Ord, Eq, PartialEq, Clone)]
pub struct Text(TextIndex);

#[derive(Default)]
pub struct TextMap {
    val_to_idx: HashMap<String, TextIndex>,
    idx_to_val: Vec<String>,
}

impl TextMap {
    fn insert_string(&mut self, val: String) -> TextIndex {
        if let Some(&idx) = self.val_to_idx.get(&val) {
            idx
        } else {
            let idx = self.idx_to_val.len() as TextIndex;
            self.val_to_idx.insert(val.clone(), idx);
            self.idx_to_val.push(val);
            idx
        }
    }
    fn insert_str(&mut self, val: &str) -> TextIndex {
        if let Some(&idx) = self.val_to_idx.get(val) {
            idx
        } else {
            let idx = self.idx_to_val.len() as TextIndex;
            self.val_to_idx.insert(val.to_string(), idx);
            self.idx_to_val.push(val.to_string());
            idx
        }
    }
    fn get_str(&self, idx: TextIndex) -> &str {
        &self.idx_to_val[idx as usize]
    }
}

static TEXT_MAP: OnceLock<RwLock<TextMap>> = OnceLock::new();

impl Text {
    pub fn from_string(s: String) -> Self {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &mut TextMap = &mut lock.write().expect("poisoned");
        Self(map.insert_string(s))
    }
    pub fn from_str(s: &str) -> Self {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &mut TextMap = &mut lock.write().expect("poisoned");
        Self(map.insert_str(s))
    }
    pub fn print_text_table() {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &TextMap = &lock.read().expect("poisoned");
        for (i, stuff) in map.idx_to_val.iter().enumerate() {
            println!("{i:>5}  {stuff}");
        }
    }
}

impl Debug for Text {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let lock: &RwLock<TextMap> = TEXT_MAP.get_or_init(Default::default);
        let map: &TextMap = &lock.read().expect("poisoned");
        write!(f, "{}", map.get_str(self.0))
    }
}
