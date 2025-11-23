use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser as _, ast},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::StableHash;

use crate::{
    AccessModifier, Identifier, Keyword, Punctuation,
    item::{Body, generic_parameters::GenericParameters},
    r#type::Type,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub struct_keyword: Keyword = expect::Keyword::Struct,
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
    pub struct Field {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub identifier: Identifier = expect::Identifier,
        pub colon: Punctuation = ':',
        pub r#type: Type = ast::<Type>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Struct {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: Signature = ast::<Signature>(),
        pub body: Body<Field> = ast::<Body<Field>>(),
    }
}

#[cfg(test)]
mod test;
