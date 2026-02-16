//! Represents a path to a sub-pattern/binding within a pattern.

use std::{collections::VecDeque, ops::Deref};

use pernixc_arena::ID;
use pernixc_ir::{
    address::{self, Address},
    pattern::Refutable,
};
use pernixc_semantic_element::{
    fields::{self, get_fields},
    variant::get_variant_associated_type,
};
use pernixc_term::{
    generic_arguments::Symbol, instantiation::get_instantiation, r#type::Type,
};

use crate::binder::{Binder, UnrecoverableError};

/// Represents a path to a field within a struct pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Field {
    /// The ID of the field being accessed.
    pub field_id: ID<fields::Field>,

    /// The path to the struct pattern containing the field.
    pub struct_path: Box<Path>,
}

/// Represents a path to an element within a tuple pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement {
    /// The index of the element being accessed.
    pub index: usize,

    /// The path to the tuple pattern containing the element.
    pub tuple_path: Box<Path>,
}

/// Represents a path to a variant within an enum pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    /// The ID of the variant being accessed.
    pub enum_path: Box<Path>,
}

/// Represents a path to a each sub-pattern/binding within a pattern.
///
/// For example, in the pattern `Some((x, y)): Option[(int32, int32)]`, the
/// path to `x` would be represented as:
///
/// ```txt
/// TupleElement {
///      index: 0,
///      tuple_path: Variant {
///         enum_path: Base,
///         // the enum variant doesn't have to be specified here since it's
///         // implied by the type of the pattern
///      }    
/// }
/// ```
///
/// This is similar to [`Address`] for memory locations, but is used to
/// represent paths within patterns.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub enum Path {
    Base,
    Field(Field),
    TupleElement(TupleElement),
    Variant(Variant),
}

impl Path {
    //// Retrieves the sub-pattern at the end of this path within the given
    /// refutable pattern.
    #[must_use]
    pub fn get_sub_refutable_from_path<'a>(
        &self,
        refutable: &'a Refutable,
    ) -> Option<&'a Refutable> {
        match self {
            Self::Base => Some(refutable),

            Self::Field(field_path) => {
                let structural_pat = field_path
                    .struct_path
                    .get_sub_refutable_from_path(refutable)?
                    .as_structural()?;

                structural_pat.patterns_by_field_id.get(&field_path.field_id)
            }

            Self::TupleElement(tuple_element_path) => {
                let tuple_pat = tuple_element_path
                    .tuple_path
                    .get_sub_refutable_from_path(refutable)?
                    .as_tuple()?;

                tuple_pat
                    .elements
                    .get(tuple_element_path.index)
                    .map(|x| &x.pattern)
            }

            Self::Variant(variant_path) => {
                let enum_pat = variant_path
                    .enum_path
                    .get_sub_refutable_from_path(refutable)?
                    .as_enum()?;

                enum_pat.pattern.as_deref()
            }
        }
    }

    pub(super) fn get_refutable_paths_internal(
        reftuable: &Refutable,
        prev: Self,
        paths: &mut VecDeque<Self>,
    ) {
        match reftuable {
            Refutable::Boolean(_) | Refutable::Integer(_) => {
                paths.push_back(prev);
            }

            Refutable::Wildcard(_) | Refutable::Named(_) => {}

            Refutable::Enum(en) => {
                paths.push_back(prev.clone());

                if let Some(inner_pat) = &en.pattern {
                    Self::get_refutable_paths_internal(
                        inner_pat,
                        Self::Variant(Variant { enum_path: Box::new(prev) }),
                        paths,
                    );
                }
            }

            Refutable::Tuple(tuple) => {
                for (index, pat) in tuple.elements.iter().enumerate() {
                    if pat.is_packed {
                        continue;
                    }

                    Self::get_refutable_paths_internal(
                        &pat.pattern,
                        Self::TupleElement(TupleElement {
                            index,
                            tuple_path: Box::new(prev.clone()),
                        }),
                        paths,
                    );
                }
            }

            Refutable::Structural(structural) => {
                let mut fields =
                    structural.patterns_by_field_id.iter().collect::<Vec<_>>();
                fields.sort_by_key(|(id, _)| *id);

                for (field_id, pat) in fields {
                    Self::get_refutable_paths_internal(
                        pat,
                        Self::Field(Field {
                            field_id: *field_id,
                            struct_path: Box::new(prev.clone()),
                        }),
                        paths,
                    );
                }
            }
        }
    }

    /// Returns the paths to all the reftutable patterns appeared. The order
    /// of [`Path`] appears in the returned vector matters.
    #[must_use]
    pub fn get_refutable_paths(reftuable: &Refutable) -> VecDeque<Self> {
        let mut paths = VecDeque::new();
        Self::get_refutable_paths_internal(reftuable, Self::Base, &mut paths);
        paths
    }
}

fn reduce_reference(mut ty: Type, mut address: Address) -> (Type, Address) {
    loop {
        match ty {
            Type::Reference(reference) => {
                ty = *reference.pointee;
                address = Address::Reference(address::Reference {
                    qualifier: reference.qualifier,
                    reference_address: Box::new(address),
                });
            }

            _ => break (ty, address),
        }
    }
}

/// The results of [`Binder::access_path_in_pattern`].
#[derive(Debug, Clone, PartialEq, Eq, derive_new::new)]
pub struct PathAccess<'r> {
    /// The refutable pattern that the path resolves to.
    pub pattern: &'r Refutable,

    /// The address of the pattern that the path resolves to.
    pub address: Address,

    /// The type of the [`Address`].
    pub r#type: Type,
}

impl Binder<'_> {
    /// Given the [`Path`] starting from the root [`Refutable`], navigate
    /// through the pattern according to the path and return the final
    /// [`PathAccess`] which contains the sub-pattern, its address, and its
    /// type.
    ///
    /// # Parameters
    ///
    /// - `refutable_pattern`: The root refutable pattern from which the path
    ///   starts.
    /// - `address`: The address that the `refutable_pattern` is bound to.
    /// - `ty`: The type of the `address`.
    /// - `path`: The path to navigate through the pattern.
    #[allow(clippy::too_many_lines)]
    pub async fn access_path_in_pattern<'r>(
        &mut self,
        refutable_pattern: &'r Refutable,
        address: Address,
        ty: Type,
        path: &Path,
    ) -> Result<PathAccess<'r>, UnrecoverableError> {
        match path {
            Path::Base => {
                let (ty, address) = reduce_reference(ty, address);
                Ok(PathAccess::new(refutable_pattern, address, ty))
            }

            Path::Variant(variant) => {
                let PathAccess { address, r#type, pattern } =
                    Box::pin(self.access_path_in_pattern(
                        refutable_pattern,
                        address,
                        ty,
                        &variant.enum_path,
                    ))
                    .await?;

                let Type::Symbol(Symbol { id: enum_id, generic_arguments }) =
                    r#type
                else {
                    panic!("unexpected type!");
                };

                let instantiation = self
                    .engine()
                    .get_instantiation(enum_id, generic_arguments)
                    .await
                    .expect("failed to get instantiation");

                let mut variant_ty = self
                    .engine()
                    .get_variant_associated_type(
                        refutable_pattern.as_enum().unwrap().variant_id,
                    )
                    .await
                    .expect("failed to get associated type")
                    .deref()
                    .clone();

                instantiation.instantiate(&mut variant_ty);

                let (ty, address) = reduce_reference(
                    variant_ty,
                    Address::Variant(address::Variant {
                        enum_address: Box::new(address),
                        id: pattern.as_enum().unwrap().variant_id,
                    }),
                );

                Ok(PathAccess::new(
                    pattern.as_enum().unwrap().pattern.as_ref().unwrap(),
                    address,
                    ty,
                ))
            }

            Path::TupleElement(tuple) => {
                let PathAccess { address, r#type, pattern } =
                    Box::pin(self.access_path_in_pattern(
                        refutable_pattern,
                        address,
                        ty,
                        &tuple.tuple_path,
                    ))
                    .await?;

                let element_pat = pattern
                    .as_tuple()
                    .unwrap()
                    .elements
                    .get(tuple.index)
                    .unwrap();

                let (ty, address) = reduce_reference(
                    r#type
                        .into_tuple()
                        .unwrap()
                        .elements
                        .remove(tuple.index)
                        .term,
                    Address::Tuple(address::Tuple {
                        tuple_address: Box::new(address),
                        offset: address::Offset::FromStart(tuple.index),
                    }),
                );

                Ok(PathAccess::new(&element_pat.pattern, address, ty))
            }

            Path::Field(path) => {
                let PathAccess { address, r#type, pattern } =
                    Box::pin(self.access_path_in_pattern(
                        refutable_pattern,
                        address,
                        ty,
                        &path.struct_path,
                    ))
                    .await?;

                // must be a struct type
                let Type::Symbol(Symbol { id: struct_id, generic_arguments }) =
                    r#type
                else {
                    panic!("unexpected type!");
                };

                let field_pat = pattern
                    .as_structural()
                    .unwrap()
                    .patterns_by_field_id
                    .get(&path.field_id)
                    .unwrap();

                let fields = self.engine().get_fields(struct_id).await;
                let instantitation = self
                    .engine()
                    .get_instantiation(struct_id, generic_arguments)
                    .await
                    .expect("failed to get instantiation");

                let mut field_ty = fields.fields[path.field_id].r#type.clone();

                instantitation.instantiate(&mut field_ty);

                let (ty, address) = reduce_reference(
                    field_ty,
                    Address::Field(address::Field {
                        struct_address: Box::new(address),
                        id: path.field_id,
                    }),
                );

                Ok(PathAccess::new(field_pat, address, ty))
            }
        }
    }
}
