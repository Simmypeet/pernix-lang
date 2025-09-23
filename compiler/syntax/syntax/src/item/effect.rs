use pernixc_parser::{
    abstract_tree, expect,
    parser::{ast, Parser},
};

use crate::{
    item::{function, generic_parameters::GenericParameters, Body},
    AccessModifier, Identifier, Keyword,
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
        pub do_keyword: Keyword = expect::Keyword::Do,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
        pub parameters: function::Parameters = ast::<function::Parameters>(),
        pub return_type: function::ReturnType
            = ast::<function::ReturnType>().optional(),
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
