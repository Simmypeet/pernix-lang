use pernixc_arena::ID;
use pernixc_query_derive::Key;
use pernixc_stable_type_id::Identifiable;
use pernixc_target::{Global, TargetID};
use serde::{Deserialize, Serialize};

use crate::key::{Dynamic, SmallBox};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Identifiable,
    Serialize,
    Deserialize,
)]
#[pernixc_query(crate)]
#[value(())]
struct Test(Global<ID<()>>);

#[test]
fn stack_smallbox() {
    let a: SmallBox<dyn Dynamic> =
        smallbox::smallbox!(Test(TargetID::Local.make_global(ID::new(0))));

    assert!(!a.is_heap());

    let b = a.smallbox_clone();

    assert!(!b.is_heap());
}
