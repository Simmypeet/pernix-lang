use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser, ast},
};

use crate::{
    AccessModifier, Identifier, Keyword,
    item::{
        Body, TrailingWhereClause, function,
        generic_parameters::GenericParameters,
    },
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub effect_traieyword: Keyword = expect::Keyword::Effect,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Operation {
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
        pub parameters: function::Parameters = ast::<function::Parameters>(),
        pub return_type: function::ReturnType
            = ast::<function::ReturnType>().optional(),
        pub trailing_where_clause: TrailingWhereClause
            = ast::<TrailingWhereClause>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Effect {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: Signature = ast::<Signature>(),
        pub body: Body<Operation> = ast::<Body<Operation>>(),
    }
}

#[cfg(test)]
mod test;
