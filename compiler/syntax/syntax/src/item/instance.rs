use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect,
    parser::{ParserExt, ast},
};

use crate::{
    Identifier, Keyword,
    item::{
        Body, TraitRef, constant, function,
        generic_parameters::GenericParameters, r#type,
    },
};

abstract_tree::abstract_tree! {

    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub instance_keyword: Keyword = expect::Keyword::Instance,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
        pub trait_ref: TraitRef = ast::<TraitRef>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct MemberTemplate<
        S: 'static + AbstractTree,
        B: 'static + AbstractTree
    > {
        pub signature: S = ast::<S>(),
        pub body: B = ast::<B>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub enum Member {
        Constant(
            MemberTemplate<constant::Signature, constant::Body>
                = ast::<MemberTemplate<constant::Signature, constant::Body>>()
        ),
        Function(
            MemberTemplate<function::Signature, function::Body>
                = ast::<MemberTemplate<function::Signature, function::Body>>()
        ),
        Type(
            MemberTemplate<r#type::Signature, r#type::Body>
                = ast::<MemberTemplate<r#type::Signature, r#type::Body>>()
        ),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Instance {
        pub signature: Signature = ast::<Signature>(),
        pub body: Body<Member> = ast::<Body<Member>>(),
    }
}
