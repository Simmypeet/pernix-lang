//! Contains the model used in borrow checking and its structs/enums
//!
//! This module doesn't contain the logic for borrow checking, but only the
//! data structures used in the borrow checking. See the
//! [`crate::ir::representation::binding::finalize::borrow`] module for the
//! actual borrow checking logic.
//!
//! # Alias-Based Formulation Borrow Checking
//!
//! Huge thanks to Niko Matsakis for his blog post on the alias-based borrow
//! checking and various posts related to **Polonius** borrow checker.
//!
//! This documentation will recap and explain how the borrow checkng is adapted
//! to the language.
//!
//! See his blog post related to borrow checking: [An alias-based formulation of the borrow checker](https://smallcultfollowing.com/babysteps/blog/2018/04/27/an-alias-based-formulation-of-the-borrow-checker/)
//!
//! ## Introduction
//!
//! The core idea of the alias-based formulation is to represent `lifetime` as a
//! set of where `Borrow (&x)` could come from. These set of `Borrow`s
//! from now on will be called `Region`.
//!
//! ## Region Inference
//!
//! After the IR is created and it comes to the borrow checking phase, every
//! occurences of [`Lifetime`] in every types of the expressions, variables,
//! etc, will be replaced with region inference variables. Think of these
//! inferences variable as unknown variables that are used in algebraic
//! equations; the precise values are not yet known, but with deductions and
//! constraints, the values can be inferred.
//!
//! ### Control Flow Graph (CFG)
//!
//! The region inferences is done through the control flow graph (CFG). For
//! exmple, consider this program.
//!
//! ```pnx
//! let mutable ref;
//!
//! if (cond) {
//!     let a = 2;
//!     ref = &a;
//! } else {
//!     let b = 2;
//!     ref = &b;
//! }
//! ```
//!
//! When converting this program to the IR, the IR will look like this:
//!
//! ``` txt
//! block0:
//!     scope push
//!     let mutable ref: &'?x int32;
//!     %r0 = load cond;
//!
//!     if %r0 goto block1 else block2;
//!
//! block1:
//!     scope push
//!     let a: int32;
//!     store 2 to a;
//!     %r1 = &'?y a;
//!     store %r1 to ref;
//!     scope pop
//!
//!     goto block3;
//!
//! block2:
//!     scope push
//!     let b: int32;
//!     store 2 to b;
//!     %r2 = &'?z b;
//!     store %r2 to ref;
//!     scope pop
//!
//!     goto block3;
//!
//! block3:
//!     ...
//!     scope pop
//! ```
//!
//! **NOTATION:**
//! - `scope push/pop`: refers to the lexical scope that is introduced by `{..}`
//!   in the program, which determines how long the stack variables live.
//! - `let ..`: refers to declaration of variables on the stack at the current
//!   scope.
//! - `'?x`: refers to region inference variable.
//! - `%rx = ..`: refers to register assignment in the SSA from.
//!
//! Throughout the flow of program, the checker will build a **subset-relation**
//! graph, which can be eventually used to compute a set of `Borrow`.
//!
//! The outlives predicate `'a: 'b` can also be expressed as subset-relation as
//! `a` is subset of `b`.
//!
//! ### Traversing the CFG
//!
//! Let's continue with the previous example. The program has 3 region inference
//! variables `'?x, '?y, '?z`. In each block, we'll see how the subset-relation
//! graph look like for each block.
//!
//! Let's look at the `block0`. Unfortunately, this block doesn't do anything
//! much so the subset-relation graph doesn't have anything for not.
//!
//! subset-relation at the end of `block0`.
//! ``` txt
//! SubsetRelation = {}
//! ```
//!
//! Next let's look at the `block1`. `block1` is the block that flows from the
//! `block0` if the `cond` was `true`. When start building the subset-relation
//! graph for each block, the block will **inherit** the subset-relation from
//! the predecessor blocks. Therefore, `block1` inherit he subset-relation from
//! `block0`, which is an empty subset-relation `{}`.
//!
//! Let's continue looking at the first three instructions of `block2`
//!
//! ```txt
//! block1:
//!     scope push
//!     let a: int32;
//!     store 2 to a;
//!     ...
//! ```
//!
//! They are very straight forward; create a new scope, variable, aand store the
//! value to the variable.
//!
//! Next instruction:
//! ``` txt
//! block1:
//!     ...
//!     %r1 = &'?y a;
//!     ...
//! ```
//!
//! The register `%r1` is assigned with `&'?y a`, which is a `Borrow`
//! expression. Therefore, we can infer that `?y` has a `Borrow` register
//! of `%r1`, which makes the subset-relation look like this.
//!
//! ```txt
//! SubsetRelation = { {%r1} -> ?y }
//! ```
//!
//! `{%r1} -> ?y` means that the set `{%r1}` is the subset of region inference
//! `?y`.
//!
//! Next instruction:
//! ```txt
//! block1:
//!     ...
//!     store %r1 to ref
//!     ...
//! ```
//!
//! The value `%r1` is stored to the variable `ref`. The value `%r1` has a type
//! of `&'?y int32` while the variable `ref` has the type of `&'?x int32`.
//! Here's the interesting part, the type of `&'?y int32` doesn't exactly equal
//! to `&'?x int32`; however, with subtyping rules, this is copmatible, but it
//! incurs a new subset-relation constraint that is `?y -> ?x`, which will be
//! included in the subset-relation of `block2`
//!
//! Subset-relation of `block1` after `store %r1 to ref` instruction.
//!
//! ``` txt
//! SubsetRelation = { {%r1} -> ?y, ?y -> ?x }
//! ```
//!
//! **NOTE:** The subtyping-rules will incurs different subset-relation
//! constraints based on the variance of the type, which can be seen at
//! [`crate::type_system::compatible`] module.
//!
//! Same goes for `block2`, which is the `block0` flows into if `cond` was
//! false.
//!
//! For the sake for brevity, the `block2` will inherit the subset-relation of
//! `block0`, which is its predecessor, that is an empty set. And finally,
//! `block2` will have the subset-relation that looks alike to the one of
//! `block1`, which looks like this:
//!
//! ```txt
//! SubsetRelation = { {%r2} -> ?z, ?z -> ?x }
//! ```
//!
//! Finally, the `block3` will inherit the subset-relation for its predecessors,
//! which there are `block1` and `block2`. In situation where there are multiple
//! predecessors, it will merge the subset-relation from multiple predecessors,
//! which will finally look like this.
//!
//! Subset-relation of `block3` after mergning `block1` and `block2`:
//!
//! ```txt
//! SubsetRelation = { {%r1} -> ?y, {%2} -> ?z, ?y -> ?x, ?z -> ?x }
//! ```
//!
//! If we look the subset-relation as a graph, it will look something like this.
//!
//! ```txt
//! {%r1} --> ?y ---+--> ?x
//!                 |
//! {%r2} --> ?z ---+
//! ```
//!
//! Now it's more clear that `?x` is a set of `{%r1, %r2}`, which is the
//! possible `Borrow` expressions that it might come from.

use std::collections::HashSet;

use cache::{RegionVariances, RegisterInfos};
use enum_as_inner::EnumAsInner;
use pernixc_abort::Abort;
use pernixc_arena::{Key, ID};
use pernixc_handler::Handler;
use pernixc_ir::{address::Address, Representation, Values};
use pernixc_semantic::{diagnostic::Diagnostic, GlobalID, Table};
use pernixc_source_file::Span;
use pernixc_semantic::term::{
    constant::Constant,
    elided_lifetimes::ElidedLifetimeID,
    generic_parameter::{GenericParameters, LifetimeParameterID},
    lifetime::Lifetime,
    r#type::{Qualifier, Type},
    visitor::RecursiveIterator,
    ModelOf, Never,
};
use pernixc_type_system::{environment::Environment, normalizer::Normalizer};
use serde::{Deserialize, Serialize};

pub(crate) mod cache;
pub(crate) mod check;
pub(crate) mod invalidate;
pub(crate) mod liveness;
pub(crate) mod local_region_generator;
pub(crate) mod subset;
pub(crate) mod transform;

pub mod diagnostic;

/// A new type wrapper over a [`ID<LocalRegion>`] to overcome the constraint
/// over orphan-rule
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
    Serialize,
    Deserialize,
)]
pub struct LocalRegionID(ID<LocalRegion>);

impl From<Never> for LocalRegionID {
    fn from(value: Never) -> Self { match value {} }
}

/// An enumeration of either a named or elided lifetime parameter id.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum NonStaticUniversalRegion {
    Named(LifetimeParameterID),
    Elided(ElidedLifetimeID),
}

/// Represents a region that was created by explicitly specified by the user.
///
/// For exapmle, the in the `function fizz['a, 'b](buzz: &'a bool, foo: 'b bool,
/// bar: &'static bool)`, the `'a`, `'b`, and `'static` are the universal
/// regions.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum UniversalRegion {
    Static,
    NonStatic(NonStaticUniversalRegion),
}

impl<M: pernixc_term::Model> TryFrom<Lifetime<M>> for UniversalRegion {
    type Error = Lifetime<M>;

    fn try_from(value: Lifetime<M>) -> Result<Self, Self::Error> {
        match value {
            Lifetime::Static => Ok(Self::Static),
            Lifetime::Parameter(member_id) => {
                Ok(Self::NonStatic(NonStaticUniversalRegion::Named(member_id)))
            }
            Lifetime::Elided(member_id) => {
                Ok(Self::NonStatic(NonStaticUniversalRegion::Elided(member_id)))
            }
            lifetime => Err(lifetime),
        }
    }
}

impl<M: pernixc_term::Model> From<UniversalRegion> for Lifetime<M> {
    fn from(value: UniversalRegion) -> Self {
        match value {
            UniversalRegion::Static => Self::Static,
            UniversalRegion::NonStatic(param) => match param {
                NonStaticUniversalRegion::Named(member_id) => {
                    Self::Parameter(member_id)
                }
                NonStaticUniversalRegion::Elided(member_id) => {
                    Self::Elided(member_id)
                }
            },
        }
    }
}

impl pernixc_semantic::Display for UniversalRegion {
    fn fmt(
        &self,
        table: &Table,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        match self {
            Self::Static => write!(f, "'static"),
            Self::NonStatic(id) => {
                match id {
                    NonStaticUniversalRegion::Named(member_id) => {
                        let generic_parameters = table
                            .query::<GenericParameters>(member_id.parent)
                            .unwrap();

                        write!(
                            f,
                            "'{}",
                            generic_parameters.lifetimes()[member_id.id].name
                        )?;
                    }
                    NonStaticUniversalRegion::Elided(member_id) => {
                        write!(f, "'{}", member_id.id.into_index())?;
                    }
                };

                Ok(())
            }
        }
    }
}

/// Represents a region inference that was created in the IR. (declared within
/// the function body, automatically created by the compiler).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
#[allow(missing_docs)]
pub struct LocalRegion;

/// Represents a region in the IR. It can be either a universal region or a
/// local region.
///
/// Region represents a set of `Borrow` expression that flows through the
/// program.
///
/// The distinction between the universal and local region is that the
/// universal region is considered "live" throughout the program, which will
/// influence how th errors are generated.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
#[allow(missing_docs)]
pub enum Region {
    Universal(UniversalRegion),
    Local(LocalRegionID),
}

impl TryFrom<Lifetime<Model>> for Region {
    type Error = Lifetime<Model>;

    fn try_from(value: Lifetime<Model>) -> Result<Self, Self::Error> {
        match value {
            Lifetime::Static => Ok(Self::Universal(UniversalRegion::Static)),
            Lifetime::Parameter(member_id) => {
                Ok(Self::Universal(UniversalRegion::NonStatic(
                    NonStaticUniversalRegion::Named(member_id),
                )))
            }
            Lifetime::Elided(member_id) => {
                Ok(Self::Universal(UniversalRegion::NonStatic(
                    NonStaticUniversalRegion::Elided(member_id),
                )))
            }
            Lifetime::Inference(inference) => Ok(Self::Local(inference)),

            lifetime => Err(lifetime),
        }
    }
}

/// The model used in borrow checking phase.
#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    Default,
    Serialize,
    Deserialize,
)]
pub struct Model;

impl pernixc_term::Model for Model {
    type LifetimeInference = LocalRegionID;
    type TypeInference = Never;
    type ConstantInference = Never;

    fn from_default_type(ty: Type<pernixc_term::Default>) -> Type<Self> {
        Type::from_other_model(ty)
    }

    fn from_default_lifetime(
        lifetime: Lifetime<pernixc_term::Default>,
    ) -> Lifetime<Self> {
        Lifetime::from_other_model(lifetime)
    }

    fn from_default_constant(
        constant: Constant<pernixc_term::Default>,
    ) -> Constant<Self> {
        Constant::from_other_model(constant)
    }
}

/// Gets the regions that got dereferenced when using the given address
fn get_dereferenced_regions_in_address(
    values: &Values<Model>,
    mut address: &Address<Model>,
    span: &Span,
    current_site: GlobalID,
    environment: &Environment<Model, impl Normalizer<Model>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<HashSet<Region>, Abort> {
    let mut regions = HashSet::new();

    loop {
        match address {
            Address::Memory(_) => break Ok(regions),

            Address::Field(field) => {
                address = &field.struct_address;
            }
            Address::Tuple(tuple) => {
                address = &tuple.tuple_address;
            }
            Address::Index(index) => {
                address = &index.array_address;
            }
            Address::Variant(variant) => {
                address = &variant.enum_address;
            }

            Address::Reference(reference) => {
                let pointee_ty = values
                    .type_of_address(
                        &reference.reference_address,
                        current_site,
                        environment,
                    )
                    .map_err(|x| {
                        x.report_overflow(|x| {
                            x.report_as_type_calculating_overflow(
                                span.clone(),
                                handler,
                            )
                        })
                    })?
                    .result;

                let pointee_reference_ty = pointee_ty.into_reference().unwrap();

                regions.extend(
                    Region::try_from(pointee_reference_ty.lifetime).ok(),
                );

                address = &reference.reference_address;
            }
        }
    }
}

/// Gets the regions that appears when using the given address
fn get_regions_in_address(
    values: &Values<Model>,
    mut address: &Address<Model>,
    span: &Span,
    include_deref: bool,
    current_site: GlobalID,
    environment: &Environment<Model, impl Normalizer<Model>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<HashSet<Region>, Abort> {
    let address_ty = values
        .type_of_address(address, current_site, environment)
        .map_err(|x| {
            x.report_overflow(|x| {
                x.report_as_type_calculating_overflow(span.clone(), handler)
            })
        })?
        .result;

    let mut regions = RecursiveIterator::new(&address_ty)
        .filter_map(|x| x.0.into_lifetime().ok())
        .filter_map(|x| Region::try_from(*x).ok())
        .collect::<HashSet<_>>();

    if include_deref {
        loop {
            match address {
                Address::Memory(_) => break,

                Address::Field(field) => {
                    address = &field.struct_address;
                }
                Address::Tuple(tuple) => {
                    address = &tuple.tuple_address;
                }
                Address::Index(index) => {
                    address = &index.array_address;
                }
                Address::Variant(variant) => {
                    address = &variant.enum_address;
                }

                Address::Reference(reference) => {
                    let pointee_ty = values
                        .type_of_address(
                            &reference.reference_address,
                            current_site,
                            environment,
                        )
                        .map_err(|x| {
                            x.report_overflow(|x| {
                                x.report_as_type_calculating_overflow(
                                    span.clone(),
                                    handler,
                                )
                            })
                        })?
                        .result;

                    let pointee_reference_ty =
                        pointee_ty.into_reference().unwrap();

                    regions.extend(
                        Region::try_from(pointee_reference_ty.lifetime).ok(),
                    );

                    if pointee_reference_ty.qualifier == Qualifier::Immutable {
                        break;
                    }

                    address = &reference.reference_address;
                }
            }
        }
    }

    Ok(regions)
}

/// Performs the borrow check analysis.
#[allow(clippy::missing_errors_doc)]
pub fn borrow_check(
    ir: &Representation<pernixc_ir::model::Model>,
    current_site: GlobalID,
    environment: &Environment<Model, impl Normalizer<Model>>,
    handler: &dyn Handler<Box<dyn Diagnostic>>,
) -> Result<(), Abort> {
    // NOTE: we clone the whole ir here, is there a better way to do this?
    let (ir, _) =
        transform::transform_to_borrow_model(ir.clone(), environment.table());

    let register_infos =
        RegisterInfos::new(&ir, current_site, environment, handler)?;
    let region_variances =
        RegionVariances::new(&ir, current_site, environment)?;
    let reachability = ir.control_flow_graph.reachability();

    let subset = subset::analyze(
        &ir,
        &register_infos,
        &region_variances,
        current_site,
        environment,
        handler,
    )?;

    check::borrow_check_internal(
        &ir,
        &subset,
        &register_infos,
        &region_variances,
        &reachability,
        current_site,
        environment,
        handler,
    )?;

    Ok(())
}
