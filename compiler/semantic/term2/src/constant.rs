//! Data definitions for constant terms.

use std::fmt::Write as _;

use derive_more::Display;
use enum_as_inner::EnumAsInner;
use pernixc_qbice::TrackedEngine;
use pernixc_symbol::name::get_qualified_name;
use pernixc_target::Global;
use qbice::{
    Decode, Encode, Identifiable, StableHash, storage::intern::Interned,
};

use crate::{
    Never, TermRef,
    display::{Display as TermDisplay, Formatter, InferenceRendering},
    error::Error,
    folding::{
        Abort, Foldable, FoldableAsync, Folder, FolderAsync, finish_fold_async,
        fold_interned, fold_option_term, fold_option_term_async,
        fold_term_slice, fold_term_slice_async, fold_tuple_terms,
        fold_tuple_terms_async,
    },
    generic_parameters::{
        ConstantParameter, ConstantParameterID, GenericParameter,
        get_generic_parameters,
    },
    inference,
    matching::{Match, Matching, Substructural},
    sub_term::{self, IterSubTerms, SubTerm},
    tuple::SubTupleLocation,
};

#[cfg(any(test, feature = "arbitrary"))]
pub mod arbitrary;

/// Represents a primitive constant.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    EnumAsInner,
    Display,
)]
#[allow(missing_docs)]
pub enum Primitive {
    #[display("{_0}i8")]
    Int8(i8),
    #[display("{_0}i16")]
    Int16(i16),
    #[display("{_0}i32")]
    Int32(i32),
    #[display("{_0}i64")]
    Int64(i64),
    #[display("{_0}isize")]
    Isize(i64),
    #[display("{_0}u8")]
    Uint8(u8),
    #[display("{_0}u16")]
    Uint16(u16),
    #[display("{_0}u32")]
    Uint32(u32),
    #[display("{_0}u64")]
    Uint64(u64),
    #[display("{_0}usize")]
    Usize(u64),
    #[display("{_0}bool")]
    Bool(bool),
}

/// Represents a struct constant.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Struct {
    id: Global<pernixc_symbol::SymbolID>,
    fields: Vec<Interned<Constant>>,
}

/// Represents an enum constant.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Enum {
    variant_id: Global<pernixc_symbol::SymbolID>,
    associated_value: Option<Interned<Constant>>,
}

/// Represents an array constant.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
)]
pub struct Array {
    elements: Vec<Interned<Constant>>,
}

impl Struct {
    /// Creates a new struct constant payload.
    #[must_use]
    pub const fn new(
        id: Global<pernixc_symbol::SymbolID>,
        fields: Vec<Interned<Constant>>,
    ) -> Self {
        Self { id, fields }
    }

    /// Returns the struct symbol id.
    #[must_use]
    pub const fn id(&self) -> Global<pernixc_symbol::SymbolID> { self.id }

    /// Returns the field values.
    #[must_use]
    pub fn fields(&self) -> &[Interned<Constant>] { &self.fields }

    /// Returns the field values mutably.
    #[must_use]
    pub fn fields_mut(&mut self) -> &mut [Interned<Constant>] {
        &mut self.fields
    }
}

impl Enum {
    /// Creates a new enum constant payload.
    #[must_use]
    pub const fn new(
        variant_id: Global<pernixc_symbol::SymbolID>,
        associated_value: Option<Interned<Constant>>,
    ) -> Self {
        Self { variant_id, associated_value }
    }

    /// Returns the variant id.
    #[must_use]
    pub const fn variant_id(&self) -> Global<pernixc_symbol::SymbolID> {
        self.variant_id
    }

    /// Returns the associated value, if any.
    #[must_use]
    pub const fn associated_value(&self) -> Option<&Interned<Constant>> {
        self.associated_value.as_ref()
    }
}

impl Array {
    /// Creates a new array constant payload.
    #[must_use]
    pub const fn new(elements: Vec<Interned<Constant>>) -> Self {
        Self { elements }
    }

    /// Returns the element values.
    #[must_use]
    pub fn elements(&self) -> &[Interned<Constant>] { &self.elements }

    /// Returns the element values mutably.
    #[must_use]
    pub fn elements_mut(&mut self) -> &mut [Interned<Constant>] {
        &mut self.elements
    }
}

/// Represents a tuple constant.
pub type Tuple = crate::tuple::Tuple<Constant>;

/// Represents a constant term payload.
#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    StableHash,
    Encode,
    Decode,
    Identifiable,
    EnumAsInner,
    derive_more::From,
)]
#[allow(missing_docs)]
pub enum Constant {
    Primitive(Primitive),
    Inference(inference::Variable<Self>),
    Parameter(ConstantParameterID),
    Struct(Struct),
    Enum(Enum),
    Array(Array),
    Tuple(Tuple),
    Phantom,
    Error(Error),
}

impl Constant {
    /// Creates a constant parameter reference.
    #[must_use]
    pub fn new_parameter(
        parent_global_id: Global<pernixc_symbol::SymbolID>,
        constant_id: pernixc_arena::ID<ConstantParameter>,
    ) -> Self {
        Self::Parameter(ConstantParameterID::new(parent_global_id, constant_id))
    }
}

impl Default for Constant {
    fn default() -> Self { Self::Tuple(Tuple::unit()) }
}

impl TryFrom<Constant> for Tuple {
    type Error = Constant;

    fn try_from(value: Constant) -> Result<Self, Self::Error> {
        match value {
            Constant::Tuple(tuple) => Ok(tuple),
            _ => Err(value),
        }
    }
}

/// The location pointing to a sub-constant term in a constant.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    derive_more::From,
    EnumAsInner,
)]
pub enum SubConstantLocation {
    /// The index of the element in a tuple constant.
    #[from]
    Tuple(SubTupleLocation),

    /// The index of the field in a struct constant.
    Struct(usize),

    /// The associated value of an enum constant.
    Enum,

    /// The index of the element in an array constant.
    Array(usize),
}

impl From<SubConstantLocation> for sub_term::TermLocation {
    fn from(value: SubConstantLocation) -> Self {
        Self::Constant(sub_term::SubConstantLocation::FromConstant(value))
    }
}

impl sub_term::Location<Constant, Constant> for SubConstantLocation {
    fn try_get_sub_term(
        self,
        term: &Constant,
        tracked_engine: &TrackedEngine,
    ) -> Option<Interned<Constant>> {
        match (self, term) {
            (Self::Tuple(location), Constant::Tuple(tuple)) => {
                tuple.get_term(&location, tracked_engine)
            }

            (Self::Struct(location), Constant::Struct(constant)) => {
                constant.fields().get(location).cloned()
            }

            (Self::Enum, Constant::Enum(constant)) => {
                constant.associated_value().cloned()
            }

            (Self::Array(location), Constant::Array(constant)) => {
                constant.elements().get(location).cloned()
            }

            _ => None,
        }
    }
}

impl SubTerm for Constant {
    type SubTypeLocation = Never;
    type SubConstantLocation = SubConstantLocation;
    type SubLifetimeLocation = Never;
    type SubInstanceLocation = Never;
    type ThisSubTermLocation = SubConstantLocation;
}

impl Match for Constant {
    #[allow(clippy::too_many_lines)]
    fn substructural_match<'a>(
        &'a self,
        other: &'a Self,
    ) -> Option<
        impl Iterator<
            Item = Substructural<
                Self::SubLifetimeLocation,
                Self::SubTypeLocation,
                Self::SubConstantLocation,
                Self::SubInstanceLocation,
            >,
        > + 'a,
    > {
        enum MatchPlan<'a> {
            Struct(&'a Struct, &'a Struct),
            EnumWithValue(Interned<Constant>, Interned<Constant>),
            EnumWithoutValue,
            Array(&'a Array, &'a Array),
            Tuple(&'a Tuple, &'a Tuple),
        }

        let match_plan = match self {
            Self::Primitive(_)
            | Self::Inference(_)
            | Self::Parameter(_)
            | Self::Phantom
            | Self::Error(_) => None,

            Self::Struct(lhs) => match other {
                Self::Struct(rhs)
                    if lhs.id() == rhs.id()
                        && lhs.fields().len() == rhs.fields().len() =>
                {
                    Some(MatchPlan::Struct(lhs, rhs))
                }

                Self::Primitive(_)
                | Self::Inference(_)
                | Self::Parameter(_)
                | Self::Struct(_)
                | Self::Enum(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom
                | Self::Error(_) => None,
            },

            Self::Enum(lhs) => match other {
                Self::Enum(rhs) if lhs.variant_id() == rhs.variant_id() => {
                    match (lhs.associated_value(), rhs.associated_value()) {
                        (Some(lhs), Some(rhs)) => Some(
                            MatchPlan::EnumWithValue(lhs.clone(), rhs.clone()),
                        ),
                        (None, None) => Some(MatchPlan::EnumWithoutValue),
                        (Some(_), None) | (None, Some(_)) => None,
                    }
                }

                Self::Primitive(_)
                | Self::Inference(_)
                | Self::Parameter(_)
                | Self::Struct(_)
                | Self::Enum(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom
                | Self::Error(_) => None,
            },

            Self::Array(lhs) => match other {
                Self::Array(rhs)
                    if lhs.elements().len() == rhs.elements().len() =>
                {
                    Some(MatchPlan::Array(lhs, rhs))
                }

                Self::Primitive(_)
                | Self::Inference(_)
                | Self::Parameter(_)
                | Self::Struct(_)
                | Self::Enum(_)
                | Self::Array(_)
                | Self::Tuple(_)
                | Self::Phantom
                | Self::Error(_) => None,
            },

            Self::Tuple(lhs) => match other {
                Self::Tuple(rhs) => Some(MatchPlan::Tuple(lhs, rhs)),

                Self::Primitive(_)
                | Self::Inference(_)
                | Self::Parameter(_)
                | Self::Struct(_)
                | Self::Enum(_)
                | Self::Array(_)
                | Self::Phantom
                | Self::Error(_) => None,
            },
        }?;

        Some(pernixc_coroutine_iter::coroutine_iter!({
            match match_plan {
                MatchPlan::Struct(lhs, rhs) => {
                    for (idx, (lhs, rhs)) in lhs
                        .fields()
                        .iter()
                        .cloned()
                        .zip(rhs.fields().iter().cloned())
                        .enumerate()
                    {
                        yield Substructural::Constant(Matching::new(
                            lhs,
                            rhs,
                            SubConstantLocation::Struct(idx),
                            SubConstantLocation::Struct(idx),
                        ));
                    }
                }

                MatchPlan::EnumWithValue(lhs, rhs) => {
                    yield Substructural::Constant(Matching::new(
                        lhs,
                        rhs,
                        SubConstantLocation::Enum,
                        SubConstantLocation::Enum,
                    ));
                }

                MatchPlan::EnumWithoutValue => {}

                MatchPlan::Array(lhs, rhs) => {
                    for (idx, (lhs, rhs)) in lhs
                        .elements()
                        .iter()
                        .cloned()
                        .zip(rhs.elements().iter().cloned())
                        .enumerate()
                    {
                        yield Substructural::Constant(Matching::new(
                            lhs,
                            rhs,
                            SubConstantLocation::Array(idx),
                            SubConstantLocation::Array(idx),
                        ));
                    }
                }

                MatchPlan::Tuple(lhs, rhs) => {
                    for substructural in lhs.substructural_match(rhs).expect(
                        "validated tuple variants before building iterator",
                    ) {
                        yield substructural;
                    }
                }
            }
        }))
    }

    fn from_self_matching(
        matching: Matching<Interned<Self>, Self::ThisSubTermLocation>,
    ) -> Substructural<
        Self::SubLifetimeLocation,
        Self::SubTypeLocation,
        Self::SubConstantLocation,
        Self::SubInstanceLocation,
    > {
        Substructural::Constant(matching)
    }
}

impl IterSubTerms for Constant {
    type TermLocation = SubConstantLocation;

    fn iter_sub_terms(
        &self,
    ) -> impl Iterator<Item = (TermRef<'_>, Self::TermLocation)> + '_ {
        pernixc_coroutine_iter::coroutine_iter!({
            match self {
                Self::Primitive(_)
                | Self::Inference(_)
                | Self::Parameter(_)
                | Self::Phantom
                | Self::Error(_) => {}

                Self::Struct(struct_constant) => {
                    for (index, field) in
                        struct_constant.fields().iter().enumerate()
                    {
                        yield (
                            TermRef::Constant(field),
                            SubConstantLocation::Struct(index),
                        );
                    }
                }

                Self::Enum(enum_constant) => {
                    if let Some(associated_value) =
                        enum_constant.associated_value()
                    {
                        yield (
                            TermRef::Constant(associated_value),
                            SubConstantLocation::Enum,
                        );
                    }
                }

                Self::Array(array_constant) => {
                    for (index, element) in
                        array_constant.elements().iter().enumerate()
                    {
                        yield (
                            TermRef::Constant(element),
                            SubConstantLocation::Array(index),
                        );
                    }
                }

                Self::Tuple(tuple) => {
                    for (term, location) in tuple
                        .iter_terms_with_location(SubConstantLocation::Tuple)
                    {
                        yield (TermRef::Constant(term), location);
                    }
                }
            }
        })
    }
}

fn fold_constant_payload<F: Folder>(
    constant: &mut Constant,
    folder: &mut F,
    engine: &TrackedEngine,
) -> Result<(), Abort> {
    *constant = match constant.clone() {
        Constant::Primitive(primitive) => Constant::Primitive(primitive),
        Constant::Inference(variable) => Constant::Inference(variable),
        Constant::Parameter(parameter) => Constant::Parameter(parameter),

        Constant::Struct(struct_constant) => {
            let mut fields = struct_constant.fields().to_vec();
            fold_term_slice(&mut fields, folder, engine)?;

            Constant::Struct(Struct::new(struct_constant.id(), fields))
        }

        Constant::Enum(enum_constant) => {
            let mut associated_value =
                enum_constant.associated_value().cloned();
            fold_option_term(&mut associated_value, folder, engine)?;

            Constant::Enum(Enum::new(
                enum_constant.variant_id(),
                associated_value,
            ))
        }

        Constant::Array(array_constant) => {
            let mut elements = array_constant.elements().to_vec();
            fold_term_slice(&mut elements, folder, engine)?;

            Constant::Array(Array::new(elements))
        }

        Constant::Tuple(mut tuple) => {
            fold_tuple_terms(&mut tuple, folder, engine)?;
            Constant::Tuple(tuple)
        }

        Constant::Phantom => Constant::Phantom,
        Constant::Error(error) => Constant::Error(error),
    };

    Ok(())
}

impl Foldable for Interned<Constant> {
    fn fold_with<F: Folder>(
        &mut self,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort> {
        fold_interned(
            self,
            folder,
            engine,
            fold_constant_payload,
            Folder::fold_constant,
        )
    }
}

// We do manual async function because of cyclic `impl Trait` sutff.
#[allow(clippy::manual_async_fn)]
fn fold_constant_payload_async<'a, F: FolderAsync + 'a>(
    constant: &'a mut Constant,
    folder: &'a mut F,
    engine: &'a TrackedEngine,
) -> impl Future<Output = Result<(), Abort>> + Send + 'a {
    async move {
        *constant = match constant.clone() {
            Constant::Primitive(primitive) => Constant::Primitive(primitive),
            Constant::Inference(variable) => Constant::Inference(variable),
            Constant::Parameter(parameter) => Constant::Parameter(parameter),

            Constant::Struct(struct_constant) => {
                let mut fields = struct_constant.fields().to_vec();
                Box::pin(fold_term_slice_async(&mut fields, folder, engine))
                    .await?;

                Constant::Struct(Struct::new(struct_constant.id(), fields))
            }

            Constant::Enum(enum_constant) => {
                let mut associated_value =
                    enum_constant.associated_value().cloned();
                Box::pin(fold_option_term_async(
                    &mut associated_value,
                    folder,
                    engine,
                ))
                .await?;

                Constant::Enum(Enum::new(
                    enum_constant.variant_id(),
                    associated_value,
                ))
            }

            Constant::Array(array_constant) => {
                let mut elements = array_constant.elements().to_vec();

                Box::pin(fold_term_slice_async(&mut elements, folder, engine))
                    .await?;

                Constant::Array(Array::new(elements))
            }

            Constant::Tuple(mut tuple) => {
                Box::pin(fold_tuple_terms_async(&mut tuple, folder, engine))
                    .await?;

                Constant::Tuple(tuple)
            }

            Constant::Phantom => Constant::Phantom,
            Constant::Error(error) => Constant::Error(error),
        };

        Ok(())
    }
}

impl FoldableAsync for Interned<Constant> {
    async fn fold_with_async<F: FolderAsync>(
        &mut self,
        folder: &mut F,
        engine: &TrackedEngine,
    ) -> Result<(), Abort> {
        let mut rebuilt_value = self.as_ref().clone();
        fold_constant_payload_async(&mut rebuilt_value, folder, engine).await?;

        finish_fold_async!(self, rebuilt_value, folder, engine, fold_constant)
    }
}

impl TermDisplay for Constant {
    async fn fmt(
        &self,
        engine: &TrackedEngine,
        formatter: &mut Formatter<'_, '_>,
    ) -> std::fmt::Result {
        match self {
            Self::Primitive(primitive) => write!(formatter, "{primitive}"),

            Self::Inference(inference) => {
                let Some(rendering) = formatter
                    .configuration()
                    .constant_inferences()
                    .and_then(|m| m.get(inference))
                else {
                    return write!(formatter, "_");
                };

                match rendering {
                    InferenceRendering::Recurse(constant) => {
                        Box::pin(constant.fmt(engine, formatter)).await
                    }
                    InferenceRendering::Rendered(rendered) => {
                        write!(formatter, "{}", rendered.as_ref())
                    }
                }
            }

            Self::Parameter(member_id) => {
                let generic_parameters =
                    engine.get_generic_parameters(member_id.parent_id()).await;

                write!(
                    formatter,
                    "{}",
                    &**generic_parameters
                        .get_constant_parameter(member_id.id())
                        .name()
                )
            }

            Self::Struct(stu) => {
                let qualified_name = engine.get_qualified_name(stu.id()).await;
                write!(formatter, "{qualified_name} {{ ")?;

                for (index, field) in stu.fields().iter().enumerate() {
                    Box::pin(field.fmt(engine, formatter)).await?;

                    if index + 1 != stu.fields().len() {
                        write!(formatter, ", ")?;
                    }
                }

                write!(formatter, " }}")
            }

            Self::Enum(en) => {
                let qualified_name =
                    engine.get_qualified_name(en.variant_id()).await;
                write!(formatter, "{qualified_name}")?;

                if let Some(variant) = en.associated_value() {
                    write!(formatter, "(")?;
                    Box::pin(variant.fmt(engine, formatter)).await?;
                    write!(formatter, ")")?;
                }

                Ok(())
            }

            Self::Array(array) => {
                write!(formatter, "[")?;

                for (index, element) in array.elements().iter().enumerate() {
                    Box::pin(element.fmt(engine, formatter)).await?;

                    if index + 1 != array.elements().len() {
                        write!(formatter, ", ")?;
                    }
                }

                write!(formatter, "]")
            }

            Self::Tuple(tuple) => Box::pin(tuple.fmt(engine, formatter)).await,

            Self::Phantom => write!(formatter, "phantom"),

            Self::Error(_) => write!(formatter, "{{error}}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        TermRef,
        sub_term::{IterSubTerms, Location, RecursivelyIterSubTerms},
        test_support::create_test_engine,
        tuple::Element,
    };

    #[tokio::test]
    async fn sub_term_locations_return_interned_children() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let element = tracked.intern(Constant::Primitive(Primitive::Uint8(1)));
        let array =
            tracked.intern(Constant::Array(Array::new(vec![element.clone()])));

        assert_eq!(
            SubConstantLocation::Array(0)
                .get_sub_term(array.as_ref(), &tracked),
            element,
        );
    }

    #[tokio::test]
    async fn iter_sub_terms_tuple_uses_single_locations() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let first = tracked.intern(Constant::Primitive(Primitive::Bool(true)));
        let second = tracked.intern(Constant::Primitive(Primitive::Uint32(2)));
        let tuple = Constant::Tuple(Tuple::new(vec![
            Element::new_regular(first.clone()),
            Element::new_unpacked(second.clone()),
        ]));

        let sub_terms: Vec<_> = tuple.iter_sub_terms().collect();
        assert_eq!(sub_terms.len(), 2);

        assert!(matches!(
            sub_terms[0].0,
            TermRef::Constant(term) if term == &first
        ));
        assert_eq!(
            sub_terms[0].1,
            SubConstantLocation::Tuple(crate::tuple::SubTupleLocation::Single(
                0
            )),
        );

        assert!(matches!(
            sub_terms[1].0,
            TermRef::Constant(term) if term == &second
        ));
        assert_eq!(
            sub_terms[1].1,
            SubConstantLocation::Tuple(crate::tuple::SubTupleLocation::Single(
                1
            )),
        );
    }

    #[tokio::test]
    async fn recursive_iteration_includes_root_in_depth_first_order() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let inner = tracked.intern(Constant::Primitive(Primitive::Bool(true)));
        let tuple = tracked.intern(Constant::Tuple(Tuple::new(vec![
            Element::new_regular(inner.clone()),
        ])));
        let root =
            tracked.intern(Constant::Array(Array::new(vec![tuple.clone()])));

        let terms: Vec<_> = root.iter_sub_terms_recursive().collect();
        assert_eq!(terms.len(), 3);

        assert!(matches!(terms[0], TermRef::Constant(term) if term == &root));
        assert!(matches!(terms[1], TermRef::Constant(term) if term == &tuple));
        assert!(matches!(terms[2], TermRef::Constant(term) if term == &inner));
    }

    #[tokio::test]
    async fn writes_constant_array() {
        let engine = create_test_engine().await;
        let tracked = engine.tracked().await;

        let constant = tracked.intern(Constant::Array(Array::new(vec![
            tracked.intern(Constant::Primitive(Primitive::Uint8(1))),
            tracked.intern(Constant::Primitive(Primitive::Uint8(2))),
        ])));

        let rendered = constant.write_to_string(&tracked).await.unwrap();
        assert_eq!(rendered, "[1u8, 2u8]");
    }
}
