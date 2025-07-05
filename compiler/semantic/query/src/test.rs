use pernixc_arena::ID;
use pernixc_query_derive::Key;
use pernixc_stable_hash::StableHash;
use pernixc_target::{Global, TargetID};

use crate::key::{Dynamic, KeySmallBox};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Key, StableHash,
)]
#[pernixc_query(crate)]
#[value(())]
struct Test(Global<ID<()>>);

#[test]
fn stack_smallbox() {
    let a: KeySmallBox<dyn Dynamic> =
        smallbox::smallbox!(Test(TargetID::Local.make_global(ID::new(0))));

    assert!(!a.is_heap());

    let b = a.smallbox_clone();

    assert!(!b.is_heap());
}
