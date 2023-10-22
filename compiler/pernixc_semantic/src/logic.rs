//! Contains code related to logic applied to the entities.

use std::{borrow::Cow, collections::HashMap};

use crate::entity::{constant::Constant, r#type::Type, region::Region, Model};

pub mod substitution;
pub mod unification;

/// Represents a substitution of terms.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
#[allow(missing_docs)]
pub struct Substitution<'a, S: Model> {
    pub types: HashMap<Cow<'a, Type<S>>, Cow<'a, Type<S>>>,
    pub constants: HashMap<Cow<'a, Constant<S>>, Cow<'a, Constant<S>>>,
    pub regions: HashMap<Cow<'a, Region<S>>, Cow<'a, Region<S>>>,
}
