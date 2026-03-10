use pernixc_parser::{
    abstract_tree::{self, AbstractTree},
    expect,
    parser::{ParserExt, ast},
};

use crate::{
    AccessModifier, Identifier, InstanceValue, Keyword, Punctuation, TraitRef,
    item::{
        Body, TrailingWhereClause, constant, function,
        generic_parameters::GenericParameters, r#type,
    },
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

#[cfg(test)]
mod test;

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Signature {
        pub instance_keyword: Keyword = expect::Keyword::Instance,
        pub identifier: Identifier = expect::Identifier,
        pub generic_parameters: GenericParameters
            = ast::<GenericParameters>().optional(),
        pub colon: Punctuation = ':',
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
    pub struct AssociatedInstanceValue {
        pub equals: Punctuation = '=',
        pub instance_value: InstanceValue = ast::<InstanceValue>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct AssociatedInstanceBody {
        pub value: AssociatedInstanceValue
            = ast::<AssociatedInstanceValue>().optional(),
        pub trailing_where_clause: TrailingWhereClause
            = ast::<TrailingWhereClause>().optional(),
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
        Instance(
            MemberTemplate<Signature, AssociatedInstanceBody>
                = ast::<MemberTemplate<Signature, AssociatedInstanceBody>>()
        )
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Instance {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub extern_keyword: Keyword = expect::Keyword::Extern.optional(),
        pub signature: Signature = ast::<Signature>(),
        pub body: Body<Member> = ast::<Body<Member>>(),
    }
}
