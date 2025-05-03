//! Contains the definition of the [`syntax_tree!`] macro.

#[doc(hidden)]
pub use std as __std;

use crate::{expect, from_node::FromNode, parser::Parser};

#[macro_export]
#[doc(hidden)]
macro_rules! extract {
    (multi) => {
        $crate::output::Multiple
    };
    () => {
        $crate::output::One
    };
}

pub use extract as __extract;
use pernixc_lexical::{token, tree::RelativeSpan};

/// Macro used for generating both syntax tree definition and its corresponding
/// parser expressions using familiar rust's struct and enum syntax.
#[macro_export]
macro_rules! abstract_tree {
    {
        $( #[$struct_meta:meta] )*
        $struct_vis:vis
        struct
        $struct_name:ident
        $(<
            $($generic_param:ident $(: $bound:tt)? ),* $(,)?
        >)?
        {
            $(
                $field_vis:vis
                $field_name:ident
                $(: $(#[$field_attr:ident])? $field_type:ty )?
                = $parser_expr:expr
            ),* $(,)?
        }
    } => {
        $( #[$struct_meta] )*
        $struct_vis
        struct
        $struct_name
        $(< $($generic_param),* >)?
        (
            #[doc(hidden)]
            $crate::abstract_tree::__std::sync::Arc<$crate::concrete_tree::Tree>
            $( ,
                #[doc(hidden)]
                $crate::abstract_tree::__std::marker::PhantomData<(
                $(
                    $generic_param
                ),* ,
            )> )?
        );

        // output verification
        const _: () = {
            struct __Verify
            $(<
                $($generic_param ),*
            >(
                $crate::abstract_tree::__std::marker::PhantomData<(
                    $(
                        $generic_param
                    ),* ,
                )>
            ))?;

            impl
            $(<
                $($generic_param $(: $bound)?),*
            >)?
            __Verify
            $(<
                $($generic_param),*
            >)?
            {
                $(
                    #[allow(dead_code)]
                    fn $field_name()
                        -> impl $crate::parser::Parser
                        $(+ $crate::output::Output<
                                Extract = $crate::abstract_tree::__extract!(
                                    $($field_attr)?
                                ),
                                Output = $field_type,
                        >)?

                    {
                        $parser_expr
                    }
                )*
            }
        };

        // field extraction
        impl
        $(<
            $($generic_param $(: $bound)?),*
        >)?
        $struct_name
        $(<
            $($generic_param),*
        >)?
        {
            $($(
            #[must_use]
            #[doc = concat!("extracts the `", stringify!($field_name), "` field")]
            $field_vis fn $field_name(&self)
               -> <
                $crate::abstract_tree::__extract!($($field_attr)?)
                as $crate::output::Extract
                >::Result<'_, $field_type> {
                    <$crate::abstract_tree::__extract!($($field_attr)?)
                        as $crate::output::Extract>::extract(&self.0.nodes)
                }
            )?)*
        }

        // parser
        const _: () = {
            struct __Parser
            $(<
                $($generic_param ),*
            >(
                $crate::abstract_tree::__std::marker::PhantomData<(
                    $(
                        $generic_param
                    ),* ,
                )>
            ))?;

            impl
            $(<
                $($generic_param $(: $bound)?),*
            >)?
            __Parser
            $(<
                $($generic_param),*
            >)?
            {
                fn parser(state: &mut $crate::state::State)
                    -> Result<(), $crate::parser::Unexpected> {

                    $(
                        let parser = $parser_expr;
                        $crate::parser::Parser::parse(&parser, state)?;
                    )*

                    Ok(())
                }
            }

            impl
            $(<
                $($generic_param $(: $bound)?),*
            >)?
            $crate::from_node::FromNode
            for
            $struct_name
            $(<
                $($generic_param),*
            >)?
            {
                fn from_node(
                    node: &$crate::concrete_tree::Node,
                ) -> Option<Self> {
                    node.as_branch().and_then(|branch| {
                        branch.ast_type_id.is_some_and(|x|
                            $crate::abstract_tree::__std::any::TypeId::of::<Self>() == x
                        )
                        .then_some(Self(
                            branch.clone(),
                            $(
                                $crate::abstract_tree::__std::marker::PhantomData::<(
                                    $(
                                        $generic_param
                                    ),* ,
                                )>
                            )?
                        ))
                    })
                }
            }

            impl
            $(<
                $($generic_param $(: $bound)?),*
            >)?
            $crate::abstract_tree::AbstractTree
            for
            $struct_name
            $(<
                $($generic_param),*
            >)?
            {
                fn parser() -> impl $crate::parser::Parser {
                    __Parser
                    $( ::<
                        $($generic_param),*
                    >)? ::parser
                }
            }

        };
    };

    {
        $( #[$enum_meta:meta] )*
        $enum_vis:vis
        enum
        $enum_name:ident
        $( < $generic_param:tt > )?
        {
            $(
                $variant_name:ident
                (
                    $variant_type:ty
                    =
                    $parser_expr:expr
                )

            ),* $(,)?
        }
    } => {
        $( #[$enum_meta] )*
        $enum_vis enum $enum_name {
            $(
                $variant_name($variant_type),
            )*
        }
    }
}

pub trait AbstractTree: Sized + FromNode {
    fn parser() -> impl Parser;
}

abstract_tree! {
    /// A test struct for the macro
    #[derive(Debug, Clone, PartialEq, Eq)]
    pub struct Test<T: 'static> {
        pub first: token::Kind<RelativeSpan> = expect::Identifier,
        pub second: token::Kind<RelativeSpan> = expect::Identifier,
        pub third: token::Kind<RelativeSpan> = expect::Identifier,
    }
}
