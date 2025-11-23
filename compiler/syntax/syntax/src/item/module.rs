use enum_as_inner::EnumAsInner;
use pernixc_lexical::tree::DelimiterKind;
use pernixc_parser::{
    abstract_tree, expect,
    parser::{Parser as _, ast},
};
use pernixc_serialize::{Deserialize, Serialize};
use pernixc_stable_hash::{StableHash, Value};
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use super::{
    constant::Constant, r#enum::Enum, r#extern::Extern, function::Function,
    implements::Implements, marker::Marker, r#struct::Struct, r#trait::Trait,
    r#type::Type,
};
use crate::{
    AccessModifier, Identifier, Keyword, Passable, SimplePath,
    item::effect::Effect,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

abstract_tree::abstract_tree! {
    #[derive(
        Debug,
        Clone,
        PartialEq,
        Eq,
        PartialOrd,
        Ord,
        Hash,
        Serialize,
        Deserialize,
        StableHash
    )]
    pub struct Signature {
        pub module_keyword: Keyword = expect::Keyword::Module,
        pub identifier: Identifier = expect::Identifier
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct From {
        pub from: Keyword = expect::Keyword::From,
        pub simple_path: SimplePath = ast::<SimplePath>()
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct Alias {
        pub as_keyword: Keyword = expect::Keyword::As,
        pub identifier: Identifier = expect::Identifier
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ImportItem {
        pub simple_path: SimplePath = ast::<SimplePath>(),
        pub alias: Alias = ast::<Alias>().optional(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct ImportItems {
        pub items: #[multi] ImportItem
            = ast::<ImportItem>().repeat_with_separator_at_least_once(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Delimited(DelimiterKind::Parenthesis)}
    pub struct ParenthesizedImportItems {
        pub items: #[multi] ImportItem
            = ast::<ImportItem>().repeat_all_with_separator(','),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum ImportItemsKind  {
        Regular(ImportItems = ast::<ImportItems>()),
        Parenthesized(
            ParenthesizedImportItems = ast::<ParenthesizedImportItems>()
        ),
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
        Serialize,
        Deserialize,
        StableHash
    )]
    pub struct Import {
        pub from: From = ast::<From>().optional(),
        pub import_keyword: Keyword = expect::Keyword::Import,
        pub items: ImportItemsKind = ast::<ImportItemsKind>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    #{fragment = expect::Fragment::Indentation}
    pub struct InlineBody {
        pub content: Content = ast::<Content>(),
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, StableHash, Serialize, Deserialize)]
    pub struct Module {
        pub access_modifier: AccessModifier = ast::<AccessModifier>(),
        pub signature: Signature = ast::<Signature>(),
        pub inline_body: InlineBody = ast::<InlineBody>().optional(),
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
        Serialize,
        Deserialize,
    )]
    pub struct Content {
        pub members: #[multi] Passable<Member>
            = ast::<Passable<Member>>().line().repeat_all(),
    }
}

impl StableHash for Content {
    fn stable_hash<H: pernixc_stable_hash::StableHasher + ?Sized>(
        &self,
        state: &mut H,
    ) {
        let inner_tree = &self.0;
        inner_tree.ast_info.stable_hash(state);

        let (tree_hash, tree_count) = inner_tree
            .nodes
            .par_iter()
            .map(|x| {
                let sub_hash = state.sub_hash(&mut |h| {
                    x.stable_hash(h);
                });

                (sub_hash, 1)
            })
            .reduce(
                || (H::Hash::default(), 0),
                |(l_hash, l_count), (r_hash, r_count)| {
                    (l_hash.wrapping_add(r_hash), r_count + l_count)
                },
            );

        tree_hash.stable_hash(state);
        tree_count.stable_hash(state);
    }
}

abstract_tree::abstract_tree! {
    #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner)]
    pub enum Member {
        Module(Module = ast::<Module>()),
        Import(Import = ast::<Import>()),
        Trait(Trait = ast::<Trait>()),
        Function(Function = ast::<Function>()),
        Type(Type = ast::<Type>()),
        Struct(Struct = ast::<Struct>()),
        Implements(Implements = ast::<Implements>()),
        Enum(Enum = ast::<Enum>()),
        Constant(Constant = ast::<Constant>()),
        Extern(Extern = ast::<Extern>()),
        Marker(Marker = ast::<Marker>()),
        Effect(Effect = ast::<Effect>()),
    }
}

#[cfg(test)]
mod test;
