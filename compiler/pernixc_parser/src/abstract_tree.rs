//! Contains the definition of the [`syntax_tree!`] macro.

#[doc(hidden)]
pub use std as __std;

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

    ( !multi -> $ty:ty ) => {
        impl $crate::abstract_tree::__std::iter::Iterator<Item = $ty>
    };

    ( ! -> $ty:ty) => {
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

/// A trait representing an abstract syntax tree (AST) node which can be
/// parsed using the given [`AbstractTree::parser`] function.
///
/// Rarely implemented directly, consider using the [`abstract_tree!`] macro
/// to generate a struct or enum that implements this trait.
pub trait AbstractTree: 'static + Sized + FromNode {
    /// Creates a parser for the syntax tree.
    ///
    /// # Note
    ///
    /// This function should be purely functional and should not have any side
    /// effects. This allows the parser to "memoize" the result and enables
    /// incremental parsing.
    #[must_use = "the parser is lazy and will not parse anything until it is \
                  used, consider using `parse()` to parse the tree immediately"]
    fn parser() -> impl Parser;

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
    ) -> (Option<Self>, Vec<error::Error>) {
        let mut cache = cache::Cache::default();
        let mut state = state::State::new(tree, &mut cache);

        let parser = parser::ast::<Self>();
        let _ = parser.parse(&mut state);

        state.finalize::<Self>()
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
///         Function(FunctionItem),
///         Struct(StructItem),
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
            $($generic_param:ident $(: $bound:tt)? ),* $(,)?
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
                    /*
                    IF YOU FOUND ERROR HERE: please make sure that the parser
                    generates the correct output type also check the `#[multi]`
                    attribute on the field since some parsers are able to
                    generate multiple nodes of the same type.
                     */
                    #[allow(dead_code)]
                    fn $field_name()
                        -> impl $crate::parser::Parser
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
            $($generic_param $(: $bound)?),*
        >)?
        $struct_name
        $(<
            $($generic_param),*
        >)?
        {
            $($(
            #[inline]
            #[must_use]
            #[doc = concat!("extracts the `", stringify!($field_name), "` field")]
            $field_vis fn $field_name(&self) ->
                $crate::abstract_tree::__extract!( ! $($field_attr)? -> $field_type )
            {
                $crate::abstract_tree::__extract!(
                    ~$($field_attr)?
                    ->
                    $parser_expr,
                    &self.0.nodes
                )
            }
            )?)*

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
                tree.ast_info.is_some_and(|x|
                    $crate::abstract_tree::__std::any::TypeId::of::<Self>()
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
                        branch.ast_info.is_some_and(|x|
                            $crate::abstract_tree::__std::any::TypeId::of::<Self>()
                                == x.ast_type_id
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

                $(
                    fn step_into_fragment() -> Option<$crate::expect::Fragment> {
                        Some($fragment)
                    }
                )?
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

pub use abstract_tree;
