use pernixc_arena::ID;
use pernixc_target::{Global, TargetID};

use super::{DynamicKey, SmallBox};
use crate::Key;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct Test;

impl Key for Global<ID<Test>> {
    type Value = ();
}

#[test]
fn stack_smallbox() {
    let a: SmallBox<dyn DynamicKey> =
        smallbox::smallbox!(TargetID::Local.make_global(ID::<Test>::new(0)));

    assert!(!a.is_heap());

    let b = a.smallbox_clone();

    assert!(!b.is_heap());
}
