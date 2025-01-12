use derive_more::{Deref, DerefMut};
use pernixc_base::handler::Handler;
use pernixc_table::{
    component::Derived, diagnostic::Diagnostic, GlobalID, Table,
};
use serde::{Deserialize, Serialize};

use crate::type_system::{model::Default, term::r#type::Type};

/// A **presistent-derived** component representing the type alias values for
/// various `type IDENT = TYPE` symbols.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    Default,
    Deref,
    DerefMut,
    Serialize,
    Deserialize,
)]
pub struct TypeAlias(pub Type<Default>);

impl Derived for TypeAlias {
    fn compute(
        global_id: GlobalID,
        table: &Table,
        handler: &dyn Handler<Box<dyn Diagnostic>>,
    ) -> Option<Self>
    where
        Self: Sized,
    {
        todo!()
    }

    fn component_name() -> &'static str { "type alias" }
}
