#![allow(missing_docs)]

use std::fmt::{Display, Write};

#[doc(hidden)]
pub use derive_more as __derive_more;
use enum_as_inner::EnumAsInner;
use pernixc_lexical::kind;
use pernixc_parser::expect;
#[doc(hidden)]
pub use pernixc_test_input as __test_input;
use pernixc_test_input::Input;
#[doc(hidden)]
pub use proptest as __proptest;
use proptest::{
    prelude::{Arbitrary, BoxedStrategy, Just, Strategy, TestCaseError},
    prop_oneof,
    strategy::LazyJust,
    test_runner::TestCaseResult,
};

#[macro_export]
#[doc(hidden)]
macro_rules! map_expr {
    (struct, $field:ident, $map_out:expr) => {
        $field.as_ref().map(|$field| $map_out)
    };

    (enum, $field:ident, $map_out:expr) => {
        $map_out
    };

    (~struct, $input:expr) => {
        Some($input)
    };

    (~struct #[option], $input:expr) => {
        $input.as_ref()
    };

    (~struct #[multi], $input:expr) => {
        $input.as_slice()
    };

    (~enum, $input:expr) => {
        $input
    };
}

#[doc(hidden)]
pub use map_expr;

#[macro_export]
#[doc(hidden)]
macro_rules! map_variant_ty {
    ($varian_ty:ty) => {
        $varian_ty
    };

    () => {
        ()
    };
}

#[doc(hidden)]
pub use map_variant_ty;

#[macro_export]
#[doc(hidden)]
macro_rules! verify {
    (
        $kind:ident,
        $value_in:expr,
        $output_in:expr,
        $field:ident,
        Option<$type:ty>,
        map_input_assert($map_in:expr, $map_out:expr)
    ) => {{
        #[allow(non_snake_case)]
        let $field = $value_in;
        let input = $map_in;

        #[allow(non_snake_case)]
        let $field = $output_in;
        let output = $crate::arbitrary::map_expr!($kind, $field, $map_out);

        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~$kind #[option], input),
            output,
            ()
        )?
    }};

    (
        $kind:ident,
        $value_in:expr,
        $output_in:expr,
        $field:ident,
        Option<$type:ty>,
        map_input_assert($map_in:expr)
    ) => {{
        #[allow(non_snake_case)]
        let $field = $value_in;
        let input = $map_in;

        #[allow(non_snake_case)]
        let $field = $output_in;
        let output = $crate::arbitrary::map_expr!($kind, $field, $field);

        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~$kind, input),
            output,
            ()
        )?
    }};

    (
        $kind:ident,
        $value_in:expr,
        $output_in:expr,
        $field:ident,
        $type:ty,
        map_input_assert($map_in:expr, $map_out:expr)
    ) => {{
        #[allow(non_snake_case)]
        let $field = $value_in;
        let input = $map_in;

        #[allow(non_snake_case)]
        let $field = $output_in;
        let output = $crate::arbitrary::map_expr!($kind, $field, $map_out);

        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~$kind, input),
            output,
            ()
        )?
    }};

    (
        $kind:ident,
        $value_in:expr,
        $output_in:expr,
        $field:ident,
        $type:ty,
        map_input_assert($map_in:expr)
    ) => {{
        #[allow(non_snake_case)]
        let $field = $value_in;
        let input = $map_in;

        #[allow(non_snake_case)]
        let $field = $output_in;
        let output = $crate::arbitrary::map_expr!($kind, $field, $field);

        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~$kind #[option], input),
            output,
            ()
        )?
    }};

    (
        $kind:ident,
        $value_in:expr,
        $output_in:expr,
        $field:ident,
        $type:ty,
        prop_assert_eq($map_in:expr, $map_out:expr)
    ) => {{
        #[allow(non_snake_case)]
        let $field = &value_in;
        let input = $map_in;

        #[allow(non_snake_case)]
        let $field = $output_in;
        let output = $crate::arbitrary::map_expr!($kind, $field, $map_out);

        $crate::arbitrary::__proptest::prop_assert_eq!($field, output,);
    }};

    (
        $kind:ident,
        $value_in:expr,
        $output_in:expr,
        $field:ident,
        $type:ty,
        prop_assert(|$output_temp:ident| $map_out:expr)
    ) => {{
        #[allow(non_snake_case)]
        let $field = $value_in;

        #[allow(non_snake_case)]
        let $output_temp = $output_in;
        let output = $crate::arbitrary::map_expr!($kind, $output_temp, $map_out);

        $crate::arbitrary::__proptest::prop_assert!(output);
    }};

    ( struct,  $value_in:expr, $output_in:expr, $field:ident, Vec<$ty:ty> ) => {
        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~struct #[multi], $value_in),
            $output_in.collect::<Vec<_>>().as_slice(),
            (),
        )?
    };

    ( struct,  $value_in:expr, $output_in:expr, $field:ident, Option<$ty:ty> ) => {
        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~struct #[option], $value_in),
            $output_in.as_ref(),
            (),
        )?
    };

    ( struct,  $value_in:expr, $output_in:expr, $field:ident, Box<$ty:ty> ) => {
        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~struct, &**$value_in),
            $output_in.as_ref(),
            (),
        )?
    };

    ( struct,  $value_in:expr, $output_in:expr, $field:ident, bool ) => {
        $crate::arbitrary::__proptest::prop_assert_eq!(
            *$value_in,
            $output_in.is_some(),
            "expected {:?} got {:?}",
            $value_in,
            $output_in,
        )
    };

    ( struct, $value_in:expr, $output_in:expr, $field:ident, $type:ty ) => {
        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~struct, $value_in),
            $output_in.as_ref(),
            (),
        )?
    };

    ( enum, $value_in:expr, $output_in:expr, $field:ident, $type:ty ) => {
        $crate::arbitrary::__test_input::Input::assert(
            $crate::arbitrary::map_expr!(~enum, $value_in),
            $output_in,
            (),
        )?
    };
}

#[doc(hidden)]
pub use verify;

#[macro_export]
#[doc(hidden)]
macro_rules! map_variant_expr {
    ( $in_expr:expr, $variant_type:ty ) => {
        $in_expr
    };

    ( $in_expr:expr  ) => {
        ()
    };
}

#[doc(hidden)]
pub use map_variant_expr;

#[macro_export]
#[doc(hidden)]
macro_rules! variant_pattern {
    ( $enum_name:ident, $variant:ident, $variant_type:ty ) => {
        $enum_name::$variant($variant)
    };

    ( $enum_name:ident, $variant:ident ) => {
        $enum_name::$variant
    };
}

#[doc(hidden)]
pub use variant_pattern;

#[macro_export]
#[doc(hidden)]
macro_rules! reference {
    {
        $( #[$struct_meta:meta] )*
        $struct_vis:vis
        struct
        $struct_name:ident
        $(<
            $($generic_param:ident $(: $($bound:tt)*)? ),* $(,)?
        >)?
        for
        $output_type:ty
        {
            $(
                $( #[$field_meta:meta] )*
                $(#{$kind:ident(
                    $($verify:tt)*
                )})?
                $field_vis:vis
                $field_name:ident($($field_type:tt)*)
            ),* $(,)?
        }
    } => {
        $( #[$struct_meta] )*
        $struct_vis struct $struct_name $(<$($generic_param $(: $($bound)*)?),*>)? {
            $(
                $( #[$field_meta] )*
                $field_vis $field_name: $($field_type)*,
            )*
        }

        impl
        $(<
            $($generic_param $(: $($bound)*)? ),* $(,)?
        >)?
        $crate::arbitrary::__test_input::Input<
            &$output_type,
            ()
        > for &$struct_name $(<$($generic_param),*>)? {
            #[allow(unused_variables)]
            fn assert(
                self,
                output: &$output_type,
                (): (),
            ) -> $crate::arbitrary::__proptest::test_runner::TestCaseResult {
                $(
                    $crate::arbitrary::verify!(
                        struct,
                        &self.$field_name,
                        output.$field_name(),
                        $field_name,
                        $($field_type)*
                        $(, $kind( $($verify)* ))?
                    );
                )*
                Ok(())
            }
        }
    };

    {
        $( #[$enum_meta:meta] )*
        $enum_vis:vis
        enum
        $enum_name:ident
        $(<
            $($generic_param:ident $(: $($bound:tt)*)? ),* $(,)?
        >)?
        for
        $output_type:ty
        {
            $(
                $( #[$variant_meta:meta] )*
                $(#{$kind:ident(
                    $($verify:tt)*
                )})?
                $variant_ident:ident $( ($variant_type:ty) )?
            ),* $(,)?
        }
    } => {
        $( #[$enum_meta] )*
        $enum_vis enum $enum_name $(<$($generic_param $(: $($bound)*)?),*>)? {
            $(
                $( #[$variant_meta] )*
                $variant_ident $( ($variant_type) )?,
            )*
        }

        impl
        $(<
            $($generic_param $(: $($bound)*)? ),* $(,)?
        >)?
        $crate::arbitrary::__test_input::Input<
            &$output_type,
            ()
        > for &$enum_name $(<$($generic_param),*>)? {
            #[allow(unused_variables, non_snake_case, unreachable_patterns)]
            fn assert(
                self,
                output: &$output_type,
                (): (),
            ) -> $crate::arbitrary::__proptest::test_runner::TestCaseResult {
                type O = $output_type;
                match (self, output) {
                    $(
                        (
                            $crate::arbitrary::variant_pattern!(
                                $enum_name,
                                $variant_ident
                                $(,$variant_type)?
                            ),
                            O::$variant_ident(output),
                        ) => {
                            $crate::arbitrary::verify!(
                                enum,
                                $crate::arbitrary::map_variant_expr!(
                                    $variant_ident
                                    $(,$variant_type)?
                                ),
                                output,
                                $variant_ident,
                                $crate::arbitrary::map_variant_ty!(
                                    $($variant_type)?
                                )
                                $(, $kind( $($verify)* ))?
                            );

                            Ok(())
                        }
                    )*

                    (i, o) => {
                        Err(
                            $crate::arbitrary::__proptest::test_runner::TestCaseError::fail(
                                format!(
                                    "expected {i:?} got {o:?}",
                                )
                            )
                        )
                    }
                }
            }
        }
    }
}

pub use reference;

use crate::{expression::arbitrary::Expression, r#type::arbitrary::Type};

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    pub enum AccessModifier for super::AccessModifier {
        #[display("public")]
        #{prop_assert(|output| output.kind == expect::Keyword::Public)}
        Public,

        #[display("private")]
        #{prop_assert(|output| output.kind == expect::Keyword::Private)}
        Private,

        #[display("internal")]
        #{prop_assert(|output| output.kind == expect::Keyword::Internal)}
        Internal,
    }
}

impl Arbitrary for AccessModifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Public),
            Just(Self::Private),
            Just(Self::Internal),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("'{identifier}")]
    pub struct Label for super::Label {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (kind::Identifier),
    }
}

impl Arbitrary for Label {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        kind::Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    #[display("..")]
    pub struct Elided for super::Elided {}
}

impl Arbitrary for Elided {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        Just(Self {}).boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    pub enum LifetimeIdentifier for super::LifetimeIdentifier {
        #{map_input_assert(Identifier, &Identifier.kind)}
        Identifier(kind::Identifier),

        #[display("static")]
        #{prop_assert(|output| output.kind == expect::Keyword::Static)}
        Static,

        Elided(Elided),
    }
}

impl Arbitrary for LifetimeIdentifier {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            kind::Identifier::arbitrary().prop_map(Self::Identifier),
            Just(Self::Static),
            Elided::arbitrary().prop_map(Self::Elided),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("'{identifier}")]
    pub struct Lifetime for super::Lifetime {
        pub identifier (LifetimeIdentifier),
    }
}

impl Arbitrary for Lifetime {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        LifetimeIdentifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum GenericArgument for super::GenericArgument {
        Lifetime(Lifetime),
        Type(Type),
        Constant(ConstantArgument),
    }
}

impl IndentDisplay for GenericArgument {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Lifetime(i) => i.fmt(f),
            Self::Type(i) => i.indent_fmt(f, indent),
            Self::Constant(i) => i.indent_fmt(f, indent),
        }
    }
}

impl Arbitrary for GenericArgument {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((ty, expr): Self::Parameters) -> Self::Strategy {
        let ty =
            ty.unwrap_or_else(|| Type::arbitrary_with((expr.clone(), None)));

        prop_oneof![
            Lifetime::arbitrary().prop_map(Self::Lifetime),
            ty.prop_map(Self::Type),
            ConstantArgument::arbitrary_with(expr).prop_map(Self::Constant),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct GenericArguments for super::GenericArguments {
        pub arguments (Vec<GenericArgument>),
    }
}

impl Arbitrary for GenericArguments {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        proptest::collection::vec(GenericArgument::arbitrary_with(arg), 0..=6)
            .prop_map(|arguments| Self { arguments })
            .boxed()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Separated<'a, T, S> {
    pub value: &'a [T],
    pub separator: S,
}

impl<T: Display, S: Display> Display for Separated<'_, T, S> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut first = true;
        for item in self.value {
            if !first {
                write!(f, "{}", self.separator)?;
            }
            first = false;
            write!(f, "{item}")?;
        }
        Ok(())
    }
}

impl<T: IndentDisplay, S: Display> IndentDisplay for Separated<'_, T, S> {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        let mut first = true;
        for item in self.value {
            if !first {
                self.separator.fmt(f)?;
            }
            first = false;
            item.indent_fmt(f, indent)?;
        }
        Ok(())
    }
}

pub trait IntoSeparated<'a> {
    type Item;

    fn into_separated<S>(self, separator: S) -> Separated<'a, Self::Item, S>;
}

impl<'a, T> IntoSeparated<'a> for &'a [T] {
    type Item = T;

    fn into_separated<S>(self, separator: S) -> Separated<'a, Self::Item, S> {
        Separated { value: self, separator }
    }
}

impl IndentDisplay for GenericArguments {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('[')?;
        self.arguments.into_separated(", ").indent_fmt(f, indent)?;
        f.write_char(']')?;

        Ok(())
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct GenericIdentifier for super::GenericIdentifier {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (kind::Identifier),
        pub generic_arguments (Option<GenericArguments>)
    }
}

impl IndentDisplay for GenericIdentifier {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.identifier.fmt(f)?;
        if let Some(ref generic_arguments) = self.generic_arguments {
            generic_arguments.indent_fmt(f, indent)?;
        }
        Ok(())
    }
}

impl Arbitrary for GenericIdentifier {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        (
            kind::Identifier::arbitrary(),
            proptest::option::of(GenericArguments::arbitrary_with(arg)),
        )
            .prop_map(|(identifier, generic_arguments)| Self {
                identifier,
                generic_arguments,
            })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("'{identifier}")]
    pub struct LifetimeParameter for super::LifetimeParameter {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (kind::Identifier),
    }
}

impl Arbitrary for LifetimeParameter {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        kind::Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    pub enum SimplePathRoot for super::SimplePathRoot {
        #[display("target")]
        #{prop_assert(|output| output.kind == expect::Keyword::Target)}
        Target,

        #{map_input_assert(Identifier, &Identifier.kind)}
        Identifier(kind::Identifier),
    }
}

impl Arbitrary for SimplePathRoot {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Target),
            kind::Identifier::arbitrary().prop_map(Self::Identifier),
        ]
        .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display("::{identifier}")]
    pub struct SimplePathSubsequent for super::SimplePathSubsequent {
        #{map_input_assert(identifier, &identifier.kind)}
        pub identifier (kind::Identifier),
    }
}

impl Arbitrary for SimplePathSubsequent {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        kind::Identifier::arbitrary()
            .prop_map(|identifier| Self { identifier })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, derive_more::Display)]
    #[display(
        "{root}{}",
        subsequences.iter()
            .map(ToString::to_string)
            .collect::<String>()
    )]
    pub struct SimplePath for super::SimplePath {
        pub root (SimplePathRoot),
        pub subsequences (Vec<SimplePathSubsequent>),
    }
}

impl Arbitrary for SimplePath {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        (
            SimplePathRoot::arbitrary(),
            proptest::collection::vec(
                SimplePathSubsequent::arbitrary(),
                0..=10,
            ),
        )
            .prop_map(|(root, subsequences)| Self { root, subsequences })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum QualifiedIdentifierRoot for super::QualifiedIdentifierRoot {
        #{prop_assert(|output| output.kind == expect::Keyword::Target)}
        Target,

        #{prop_assert(|output| output.kind == expect::Keyword::This)}
        This,

        GenericIdentifier(GenericIdentifier),
    }
}

impl Arbitrary for QualifiedIdentifierRoot {
    type Parameters = Option<BoxedStrategy<GenericIdentifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            Just(Self::Target),
            Just(Self::This),
            arg.unwrap_or_else(GenericIdentifier::arbitrary)
                .prop_map(Self::GenericIdentifier),
        ]
        .boxed()
    }
}

impl IndentDisplay for QualifiedIdentifierRoot {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Target => f.write_str("target"),
            Self::This => f.write_str("this"),
            Self::GenericIdentifier(i) => i.indent_fmt(f, indent),
        }
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct QualifiedIdentifierSubsequent for super::QualifiedIdentifierSubsequent {
        pub generic_identifier (GenericIdentifier),
    }
}

impl IndentDisplay for QualifiedIdentifierSubsequent {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_str("::")?;
        self.generic_identifier.indent_fmt(f, indent)
    }
}

impl Arbitrary for QualifiedIdentifierSubsequent {
    type Parameters = Option<BoxedStrategy<GenericIdentifier>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        arg.unwrap_or_else(GenericIdentifier::arbitrary)
            .prop_map(|generic_identifier| Self { generic_identifier })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub struct QualifiedIdentifier for super::QualifiedIdentifier {
        pub root (QualifiedIdentifierRoot),
        pub subsequences (Vec<QualifiedIdentifierSubsequent>),
    }
}

impl IndentDisplay for QualifiedIdentifier {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        self.root.indent_fmt(f, indent)?;

        for subsequence in &self.subsequences {
            subsequence.indent_fmt(f, indent)?;
        }

        Ok(())
    }
}

impl Arbitrary for QualifiedIdentifier {
    type Parameters =
        (Option<BoxedStrategy<Type>>, Option<BoxedStrategy<Expression>>);

    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(arg: Self::Parameters) -> Self::Strategy {
        let generic_identifier_strategy =
            GenericIdentifier::arbitrary_with(arg);
        (
            QualifiedIdentifierRoot::arbitrary_with(Some(
                generic_identifier_strategy.clone(),
            )),
            proptest::collection::vec(
                QualifiedIdentifierSubsequent::arbitrary_with(Some(
                    generic_identifier_strategy,
                )),
                0..=6,
            ),
        )
            .prop_map(|(root, subsequences)| Self { root, subsequences })
            .boxed()
    }
}

reference! {
    #[derive(Debug, Clone, Copy, derive_more::Display)]
    #[display(
        "&{}",
        if *mut_keyword { "mut " } else { "" },
    )]
    pub struct ReferenceOf for super::ReferenceOf {
        pub mut_keyword (bool),
    }
}

impl Arbitrary for ReferenceOf {
    type Parameters = ();
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with((): Self::Parameters) -> Self::Strategy {
        bool::arbitrary().prop_map(|mut_keyword| Self { mut_keyword }).boxed()
    }
}

reference! {
    #[derive(Debug, Clone)]
    pub enum ConstantArgument for super::ConstantArgument {
        Expression(Expression),
        Elided(Elided),
    }
}

impl Arbitrary for ConstantArgument {
    type Parameters = Option<BoxedStrategy<Expression>>;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(expr: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            expr.unwrap_or_else(Expression::arbitrary)
                .prop_map(Self::Expression),
            Elided::arbitrary().prop_map(Self::Elided),
        ]
        .boxed()
    }
}

impl IndentDisplay for ConstantArgument {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        f.write_char('{')?;
        match self {
            Self::Expression(i) => i.indent_fmt(f, indent)?,
            Self::Elided(i) => i.fmt(f)?,
        }
        f.write_char('}')
    }
}

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum Passable<T> {
    Pass,
    Line(T),
}

impl<T> Passable<T> {
    pub const fn as_ref(&self) -> Passable<&T> {
        match self {
            Self::Pass => Passable::Pass,
            Self::Line(i) => Passable::Line(i),
        }
    }
}

impl<T: Arbitrary + 'static> Arbitrary for Passable<T> {
    type Parameters = T::Parameters;
    type Strategy = BoxedStrategy<Self>;

    fn arbitrary_with(args: Self::Parameters) -> Self::Strategy {
        prop_oneof![
            1 =>
            LazyJust::new(|| Self::Pass),
            10 =>
            T::arbitrary_with(args).prop_map(Self::Line),
        ]
        .boxed()
    }
}

impl<O: std::fmt::Debug, T: std::fmt::Debug> Input<&super::Passable<O>, ()>
    for &Passable<T>
where
    for<'x, 'y> &'x T: Input<&'y O, ()>,
{
    fn assert(self, output: &super::Passable<O>, (): ()) -> TestCaseResult {
        match (self, output) {
            (Passable::Pass, super::Passable::Pass(_)) => Ok(()),
            (Passable::Line(i), super::Passable::Line(o)) => i.assert(o, ()),
            (i, output) => Err(TestCaseError::fail(format!(
                "Expected {i:?}, got {output:?}",
            ))),
        }
    }
}

impl<O: std::fmt::Debug, T: std::fmt::Debug> Input<super::Passable<&O>, ()>
    for Passable<&T>
where
    for<'x, 'y> &'x T: Input<&'y O, ()>,
{
    fn assert(self, output: super::Passable<&O>, (): ()) -> TestCaseResult {
        match (self, output) {
            (Passable::Pass, super::Passable::Pass(_)) => Ok(()),
            (Passable::Line(i), super::Passable::Line(o)) => i.assert(o, ()),
            (i, output) => Err(TestCaseError::fail(format!(
                "Expected {i:?}, got {output:?}",
            ))),
        }
    }
}

impl<T: Display> Display for Passable<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pass => f.write_str("pass"),
            Self::Line(i) => i.fmt(f),
        }
    }
}

impl<T: IndentDisplay> IndentDisplay for Passable<T> {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        indent: usize,
    ) -> std::fmt::Result {
        match self {
            Self::Pass => f.write_str("pass"),
            Self::Line(i) => i.indent_fmt(f, indent),
        }
    }
}

/// A wrapper trait over the `std::fmt::Display` trait that allows for
/// displaying the item with indentation.
pub trait IndentDisplay {
    /// Display the item with indentation.
    #[allow(clippy::missing_errors_doc)]
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result;
}

impl<T: IndentDisplay> IndentDisplay for Box<T> {
    fn indent_fmt(
        &self,
        f: &mut std::fmt::Formatter,
        indent: usize,
    ) -> std::fmt::Result {
        (**self).indent_fmt(f, indent)
    }
}

/// Write a line with indentation to the formatter.
#[allow(clippy::missing_errors_doc)]
pub fn write_indent_line(
    f: &mut std::fmt::Formatter,
    o: &impl Display,
    indent: usize,
) -> std::fmt::Result {
    for _ in 0..indent {
        f.write_str("    ")?;
    }

    writeln!(f, "{o}")
}

#[allow(clippy::missing_errors_doc)]
pub fn write_indent_line_for_indent_display(
    f: &mut std::fmt::Formatter,
    o: &impl IndentDisplay,
    indent: usize,
) -> std::fmt::Result {
    for _ in 0..indent {
        f.write_str("    ")?;
    }

    o.indent_fmt(f, indent)?;
    writeln!(f)
}

/// A wrapper over a `Display` item that includes the indentation level.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IndentDisplayItem<'a, T>(pub usize, pub &'a T);

impl<T: IndentDisplay> Display for IndentDisplayItem<'_, T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1.indent_fmt(f, self.0)
    }
}
