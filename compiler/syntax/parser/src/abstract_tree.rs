//! Contains the definition of the [`syntax_tree!`] macro.

#[doc(hidden)]
pub use std as __std;
use std::marker::PhantomData;

#[doc(hidden)]
pub use pernixc_lexical::tree::RelativeLocation;
#[doc(hidden)]
pub use pernixc_lexical::tree::RelativeSpan;
use pernixc_qbice::Interner;
#[doc(hidden)]
pub use pernixc_qbice::Interner as __Interner;
#[doc(hidden)]
pub use pernixc_source_file::SourceElement;
#[doc(hidden)]
pub use qbice::stable_type_id as __stable_type_id;
use qbice::{Decode, Encode};

use crate::{
    cache, error, expect,
    from_node::FromNode,
    parser::{self, Parser},
    state,
};

#[macro_export]
#[doc(hidden)]
macro_rules! extract {
    (multi) => {
        $crate::output::Multiple
    };
    () => {
        $crate::output::One
    };

    ( !multi -> $ty:ty where $($generic_param:ident),* ) => {
        impl $crate::abstract_tree::__std::iter::Iterator<Item = $ty>
            + use<'_ $(, $generic_param)*>
    };

    ( ! -> $ty:ty where $($generic_param:ident),* ) => {
        $crate::abstract_tree::__std::option::Option<$ty>
    };

    ( ~multi -> $parser:expr, $node:expr ) => {
        $crate::output::extract_multiple(
            $parser,
            $node,
        )
    };

    ( ~ -> $parser:expr, $node:expr ) => {
        $crate::output::extract_one(
            $parser,
            $node,
        )
    };
}

pub use extract as __extract;

#[macro_export]
#[doc(hidden)]
macro_rules! field_extract {
    (@call_tuple
        $((
            $field_vis:vis
            $fied_name:ident
            ($parser_expr:expr)
            ->
            $field_type:ty
            $(: $field_attr:ident)?
        ),)*
        where []
    ) => {
        $(
            $crate::abstract_tree::__field_extract!(
                @call
                (
                    $field_vis
                    $fied_name
                    ($parser_expr)
                    ->
                    $field_type
                    $(: $field_attr)?
                )
                where []
            );
        )*
    };

    (@call_tuple
        $((
            $field_vis:vis
            $fied_name:ident
            ($parser_expr:expr)
            ->
            $field_type:ty
            $(: $field_attr:ident)?
        ),)*
        where $tuple:tt
    ) => {
        $(
            $crate::abstract_tree::__field_extract!(
                @call
                (
                    $field_vis
                    $fied_name
                    ($parser_expr)
                    ->
                    $field_type
                    $(: $field_attr)?
                )
                where $tuple
            );
        )*
    };

    (@call
        (
            $field_vis:vis
            $field_name:ident
            ($parser_expr:expr)
            ->
            $field_type:ty
            $(: $field_attr:ident)?
        )
        where [$($generic_param:ident)*]
    ) => {
        /*
        IF YOU FOUND ERROR HERE: please make sure that the parser
        generates the correct output type also check the `#[multi]`
        attribute on the field since some parsers are able to
        generate multiple nodes of the same type.
        */
        #[inline]
        #[must_use]
        #[allow(dead_code)]
        #[doc = concat!("extracts the `", stringify!($field_name), "` field")]
        #[allow(clippy::double_must_use)]
        $field_vis fn $field_name(&self) ->
            $crate::abstract_tree::__extract!(
                !
                $($field_attr)?
                ->
                $field_type
                where
                $($generic_param),*
            )
        {
            $crate::abstract_tree::__extract!(
                ~$($field_attr)?
                ->
                $parser_expr,
                &self.0.nodes
            )
        }
    };
}

pub use field_extract as __field_extract;

/// A trait representing an abstract syntax tree (AST) node which can be
/// parsed using the given [`AbstractTree::parser`] function.
///
/// Rarely implemented directly, consider using the [`abstract_tree!`] macro
/// to generate a struct or enum that implements this trait.
pub trait AbstractTree:
    'static
    + Sized
    + FromNode
    + Identifiable
    + SourceElement<Location = RelativeLocation>
{
    /// Creates a parser for the syntax tree.
    ///
    /// # Note
    ///
    /// This function should be purely functional and should not have any side
    /// effects. This allows the parser to "memoize" the result and enables
    /// incremental parsing.
    #[must_use = "the parser is lazy and will not parse anything until it is \
                  used, consider using `parse()` to parse the tree immediately"]
    fn parser<I: Interner>() -> impl Parser<I>;

    /// Returns the fragment that this tree must step into before parsing.
    ///
    /// Default is `None`, which means that the parser does not need to step
    /// into any fragment, the parser shall parse the tree immediately in the
    /// current fragment level.
    #[must_use]
    fn step_into_fragment() -> Option<expect::Fragment> { None }

    /// Parses the given tree and returns the result.
    #[must_use]
    fn parse(
        tree: &pernixc_lexical::tree::Tree,
        interner: &impl Interner,
    ) -> (Option<Self>, Vec<error::Error>) {
        let mut cache = cache::Cache::default();
        let mut state = state::State::new(tree, &mut cache, interner);

        let parser = parser::ast::<Self>();
        let result = parser.parse(&mut state);

        state.finalize::<Self>(result.is_err())
    }
}

/// Macro used for generating both syntax tree definition and its corresponding
/// parser expressions using familiar rust's struct and enum syntax.
///
/// The macro offers two variants of declaring a syntax tree: struct and enum.
///
/// # Struct AST
///
/// The struct defines a syntax tree with a consecutive parser expression that
/// will be executed in order to parse the tree.
///
/// ``` ignore
/// ast! {
///     pub struct SimpleStruct {
///         pub visibility: AccessModifier =
///             parse_ast::<AccessModifier>().optional(),
///        pub keyword = expect::Keyword,
///        pub name: token::Kind<RelativeSpan> =
///             expect::Identifier,
///     }
/// }
///
/// // The above macro generate a struct that can be used like
/// pub fn use_simple_struct(ast: SimpleStruct) {
///     // is an instance of `Option<AccessModifier>`
///     let visibility = ast.visibility();
///
///     // is an instance of `Option<token::Kind<RelativeSpan>>`;
///     // it is an option since in case of an invalid input where the
///     // identifier is not present, the parser will try its best to parse the
///     // most tokens it can which could potentially result in a partially
///     // valid tree.
///     let name = ast.name();
/// }
/// ```
///
/// # Enum AST
///
/// The enum defines a choice of syntax trees that the parser will try to
/// parser. The order of declaration of the enum variants is important, as the
/// first matching variant will be used.
///
/// ``` ignore
/// ast! {
///     pub enum ModuleItem {
///         Function(FunctionItem = parser::ast::<FunctionItem>()),
///         Struct(StructItem = parser::ast::<StructItem>()),
///     }
/// }
///
/// // the above macro generates an enum that can be used like
/// pub fn use_module_item(ast: ModuleItem) {
///     // each enum variant is an AST that can be used normally
///     match ast {
///         ModuleItem::Function(func) => {}
///         ModuleItem::Struct(struct_item) => {}
///     }
/// }
/// ```
#[macro_export]
macro_rules! abstract_tree {
    {
        $( #[$struct_meta:meta] )*
        $( #{fragment = $fragment:expr} )?
        $struct_vis:vis
        struct
        $struct_name:ident
        $(<
            $($generic_param:ident $(: $first_bound:tt $( + $rest_bound:tt)*)? ),* $(,)?
        >)?
        {
            $(
                $( #[$field_meta:meta] )*
                $field_vis:vis
                $field_name:ident
                $(: $(#[$field_attr:ident])? $field_type:ty )?
                = $parser_expr:expr
            ),* $(,)?
        }
    } => {
        $( #[$struct_meta] )*
        #[derive($crate::abstract_tree::__stable_type_id::Identifiable)]
        #[stable_type_id_crate($crate::abstract_tree::__stable_type_id)]
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
            struct __Verify $(<
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
                $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
            >)?
            __Verify
            $(<
                $($generic_param),*
            >)?
            {
                $(
                    #[allow(dead_code)]
                    fn $field_name<I: $crate::abstract_tree::__Interner>()
                        -> impl $crate::parser::Parser<I>
                        $(+ for<'x> $crate::output::Output<
                                Extract = $crate::abstract_tree::__extract!(
                                    $($field_attr)?
                                ),
                                Output<'x> = $field_type,
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
            $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
        >)?
        $struct_name
        $(<
            $($generic_param),*
        >)?
        {
            $crate::abstract_tree::__field_extract!(
                @call_tuple
                $($((
                    $field_vis
                    $field_name
                    ($parser_expr)
                    ->
                    $field_type
                    $(: $field_attr)?
                ),)?)* where [ $($($generic_param)*)? ]
            );

            #[doc(hidden)]
            fn __parser<I: $crate::abstract_tree::__Interner>(
                state: &mut $crate::state::State<I>
            ) ->  $crate::abstract_tree::__std::result::Result<
                (),
                $crate::parser::Unexpected,
            > {
                $(
                    let parser = $parser_expr;
                    $crate::parser::Parser::parse(&parser, state)?;
                )*

                Ok(())
            }

            /// Retrieves the inner concrete tree that this abstract tree is
            /// layered on top of.
            #[allow(dead_code)]
            $struct_vis fn inner_tree(&self)
                -> &$crate::abstract_tree::__std::sync::Arc<
                    $crate::concrete_tree::Tree
                >
            {
                &self.0
            }

            /// Casts the given tree to this abstract tree type.
            #[allow(dead_code)]
            $struct_vis fn from_tree(
                tree:
                &$crate::abstract_tree::__std::sync::Arc<
                    $crate::concrete_tree::Tree
                >
            ) -> Option<Self> {
                tree.ast_info.clone().is_some_and(|x|
                    <Self as $crate::abstract_tree::__stable_type_id::Identifiable>::STABLE_TYPE_ID
                        == x.ast_type_id
                )
                .then_some(Self(
                    tree.clone(),
                    $(
                        $crate::abstract_tree::__std::marker::PhantomData::<(
                            $(
                                $generic_param
                            ),* ,
                        )>
                    )?
                ))
            }
        }

        // parser
        const _: () = {
            impl
            $(<
                $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
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
                        branch.ast_info.clone().is_some_and(|x|
                            <Self as $crate::abstract_tree::__stable_type_id::Identifiable>::STABLE_TYPE_ID
                                == x.ast_type_id
                        )
                        .then(|| Self(
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
                $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
            >)?
            $crate::abstract_tree::AbstractTree
            for
            $struct_name
            $(<
                $($generic_param),*
            >)?
            {
                fn parser<I: $crate::abstract_tree::__Interner>() -> impl $crate::parser::Parser<I> {
                    Self::__parser
                }

                $(
                    fn step_into_fragment() -> Option<$crate::expect::Fragment> {
                        Some($fragment)
                    }
                )?
            }


            impl
            $(<
                $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
            >)?
            $crate::abstract_tree::SourceElement
            for
            $struct_name
            $(<
                $($generic_param),*
            >)?
            {
                type Location = $crate::abstract_tree::RelativeLocation;

                fn span(&self) -> $crate::abstract_tree::RelativeSpan {
                    self.0.span()
                }
            }

        };
    };

    {
        $( #[$enum_meta:meta] )*
        $( #{fragment = $fragment:expr} )?
        $enum_vis:vis
        enum
        $enum_name:ident
        $(<
            $($generic_param:ident $(: $first_bound:tt $( + $rest_bound:tt)*)? ),* $(,)?
        >)?
        {
            $( #[$first_variant_meta:meta] )*
            $first_variant_name:ident
            (
                $first_variant_type:ty
                =
                $first_parser_expr:expr
            )

            $(
                ,
                $( #[$rest_variant_meta:meta] )*
                $rest_variant_name:ident
                (
                    $rest_variant_type:ty
                    =
                    $rest_parser_expr:expr
                )
            )*
            $(,)?
        }
    } => {
        $( #[$enum_meta] )*
        #[derive($crate::abstract_tree::__stable_type_id::Identifiable)]
        #[stable_type_id_crate($crate::abstract_tree::__stable_type_id)]
        $enum_vis
        enum
        $enum_name
        $(<
            $($generic_param),*
        >)?
        {
            $( #[$first_variant_meta] )*
            $first_variant_name($first_variant_type),
            $(
                $( #[$rest_variant_meta] )*
                $rest_variant_name($rest_variant_type)
            ),*
        }

        // parser
        const _: () = {
            impl
            $(<
                $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
            >)?
            $crate::from_node::FromNode
            for
            $enum_name
            $(<
                $($generic_param),*
            >)?
            {
                fn from_node(
                    node: &$crate::concrete_tree::Node,
                ) -> Option<Self> {
                    let branch = node.as_branch().and_then(|branch| {
                        branch.ast_info.clone().is_some_and(|x|
                            <Self as $crate::abstract_tree::__stable_type_id::Identifiable>::STABLE_TYPE_ID
                                == x.ast_type_id
                        )
                        .then(|| branch.clone())
                    })?;

                    if let Some(result) = $crate::output::extract_one(
                        $first_parser_expr,
                        &branch.nodes
                    ) {
                        return Some(Self::$first_variant_name(result));
                    }

                    $(

                    if let Some(result) = $crate::output::extract_one(
                        $rest_parser_expr,
                        &branch.nodes
                    ) {
                        return Some(Self::$rest_variant_name(result));
                    }
                    )*

                    None
                }
            }

            impl
            $(<
                $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
            >)?
            $crate::abstract_tree::AbstractTree
            for
            $enum_name
            $(<
                $($generic_param),*
            >)?
            {
                fn parser<I: $crate::abstract_tree::__Interner>()
                    -> impl $crate::parser::Parser<I> {
                    $crate::parser::Choice((
                        $first_parser_expr,
                        $($rest_parser_expr),*
                    ))
                }

                $(
                    fn step_into_fragment() -> Option<$crate::expect::Fragment> {
                        Some($fragment)
                    }
                )?
            }

            impl
            $(<
                $($generic_param $(: $first_bound $(+ $rest_bound)*)?),*
            >)?
            $crate::abstract_tree::SourceElement
            for
            $enum_name
            $(<
                $($generic_param),*
            >)?
            {
                type Location = $crate::abstract_tree::RelativeLocation;

                fn span(&self) -> $crate::abstract_tree::RelativeSpan {
                    match self {
                        Self::$first_variant_name(node) => node.span(),
                        $(Self::$rest_variant_name(node) => node.span()),*
                    }
                }
            }
        };
    }
}

pub use abstract_tree;
use qbice::stable_type_id::Identifiable;

/// A tagging type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Default,
    Encode,
    Decode,
    Identifiable,
)]
pub struct First;

/// A tagging type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Default,
    Encode,
    Decode,
    Identifiable,
)]
pub struct Second;

/// A tagging type
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    Default,
    Encode,
    Decode,
    Identifiable,
)]
pub struct Third;

/// A helper struct that allows you to tag multiple trees with the same type
/// in a single [`abstract_tree!`] macro.
///
/// By design of the node extraction, the parser returns the first matching
/// node in the tree which means that if you have multiple trees with the same
/// type in the same tree and you want to extract them, it always returns the
/// first one.
///
/// This struct allows you to differentiate between the trees by tagging them
/// with a unique simplen number.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Identifiable,
    Encode,
    Decode,
    derive_more::Deref,
    derive_more::DerefMut,
)]
pub struct Tag<T, U>(
    #[deref]
    #[deref_mut]
    pub T,
    pub PhantomData<U>,
);

impl<T: AbstractTree, U: Identifiable + 'static> AbstractTree for Tag<T, U> {
    fn parser<I: Interner>() -> impl Parser<I> { parser::ast::<T>() }

    fn step_into_fragment() -> Option<expect::Fragment> { None }
}

impl<T: AbstractTree, U: Identifiable + 'static> SourceElement for Tag<T, U> {
    type Location = RelativeLocation;

    fn span(&self) -> pernixc_source_file::Span<Self::Location> {
        self.0.span()
    }
}

impl<T: AbstractTree, U: Identifiable> FromNode for Tag<T, U> {
    fn from_node(node: &crate::concrete_tree::Node) -> Option<Self> {
        node.as_branch().and_then(|branch| {
            branch
                .ast_info
                .clone()
                .is_some_and(|x| x.ast_type_id == Self::STABLE_TYPE_ID)
                .then(|| {
                    branch.nodes.first().and_then(|node| {
                        T::from_node(node).map(|x| Self(x, PhantomData))
                    })
                })?
        })
    }
}

impl<T, U> Tag<T, U> {
    /// Gets the inner tree of this tag.
    pub fn into_innter(self) -> T { self.0 }
}
