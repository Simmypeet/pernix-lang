use insta::{assert_ron_snapshot, Settings};
use serde::{Deserialize, Serialize};

use crate::Key;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct Variable(String);

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Key,
    Serialize,
    Deserialize,
)]
#[pernixc_query(crate)]
#[value(i32)]
pub struct NegateVariable(String);

#[test]
fn seriaizable() {
    let mut db = crate::Database::default();

    db.register_reflector::<Variable>();
    db.register_reflector::<NegateVariable>();

    db.set_input(&Variable("x".to_string()), 32);
    db.set_input(&NegateVariable("x".to_string()), -32);
    db.set_input(&Variable("y".to_string()), 64);
    db.set_input(&NegateVariable("y".to_string()), -64);

    let map = db.map.serializable(&db.reflector);

    let mut settings = Settings::clone_current();
    settings.set_sort_maps(true);
    settings.bind(|| assert_ron_snapshot!(map));
}
