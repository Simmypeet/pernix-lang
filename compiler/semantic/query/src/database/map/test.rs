use std::sync::{atomic::AtomicBool, Arc};

use pernixc_query_derive::Key;

use super::Map;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[value(i32)]
#[pernixc_query(crate)]
struct I32Key(i32);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[value(i32)]
#[pernixc_query(crate)]
struct I64Key(i64);

#[test]
fn basic() {
    let map = Map::default();

    assert!(map.insert(I32Key(1), 1).is_none());
    assert_eq!(map.insert(I32Key(1), 2), Some(1));
    assert!(map.insert(I32Key(10), 10).is_none());

    assert!(map.insert(I64Key(1), 1).is_none());
    assert_eq!(map.insert(I64Key(1), 2), Some(1));
    assert!(map.insert(I64Key(10), 10).is_none());
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key)]
#[value(DropCheck)]
#[pernixc_query(crate)]
pub struct DropKey(i32);

#[derive(Debug, Clone, Default)]
pub struct DropCheck(pub Arc<AtomicBool>);

impl PartialEq for DropCheck {
    fn eq(&self, other: &Self) -> bool {
        let this = self.0.load(std::sync::atomic::Ordering::SeqCst);
        let other = other.0.load(std::sync::atomic::Ordering::SeqCst);

        this == other
    }
}

impl Eq for DropCheck {}

impl Drop for DropCheck {
    fn drop(&mut self) {
        self.0.store(true, std::sync::atomic::Ordering::SeqCst);
    }
}

#[test]
fn drop() {
    let map = Map::default();

    let first_check = Arc::new(AtomicBool::new(false));
    let second_check = Arc::new(AtomicBool::new(false));
    let fake_check = Arc::new(AtomicBool::new(false));

    map.insert(DropKey(1), DropCheck(first_check.clone()));
    map.insert(DropKey(2), DropCheck(fake_check.clone()));

    // store in the variable to ensure the value is not dropped directly after.
    let fake_check_replace =
        map.insert(DropKey(2), DropCheck(second_check.clone())).unwrap();

    assert!(Arc::ptr_eq(&fake_check_replace.0, &fake_check));

    // skipcq: RS-E1011 false positive
    std::mem::drop(map);

    assert!(first_check.load(std::sync::atomic::Ordering::SeqCst));
    assert!(second_check.load(std::sync::atomic::Ordering::SeqCst));
    assert!(!fake_check.load(std::sync::atomic::Ordering::SeqCst));
}
