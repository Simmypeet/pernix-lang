use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser as _, ast},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use super::Body;
use crate::{
    AccessModifier, Identifier, Keyword,
    item::generic_parameters::GenericParameters, r#type::Type,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub enum_keyword: Keyword = expect::Keyword::Enum,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters =
            ast::<GenericParameters>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        StableHash,
        Serialize,
        Deserialize,
    )]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct VariantAssociation {
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Variant {
        pub identifier: Identifier = expect::Identifier,
        pub association: VariantAssociation =
            ast::<VariantAssociation>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Enum {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: Signature = ast::<Signature>(),
        pub body: Body<Variant> = ast::<Body<Variant>>(),
    }
}

#[cfg(test)]
mod test;
