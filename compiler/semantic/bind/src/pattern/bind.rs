//! Contains the definition of [`Bind`] trait for binding pattern.

use std::{num::NonZero, ops::Deref};

use pernixc_handler::Handler;
use pernixc_hash::HashMap;
use pernixc_ir::{
    instruction::SwitchValue,
    pattern::{
        Boolean, Enum, Integer, Irrefutable, Named, Refutable, Structural,
        Tuple, TupleElement, Wildcard,
    },
};
use pernixc_lexical::tree::RelativeLocation;
use pernixc_parser::abstract_tree::AbstractTree;
use pernixc_resolution::diagnostic::{
    Diagnostic as ResolutionDiagnostic, SymbolIsNotAccessible, SymbolNotFound,
};
use pernixc_semantic_element::{
    fields::get_fields, variant::get_variant_associated_type,
};
use pernixc_source_file::SourceElement;
use pernixc_symbol::{
    accessibility::{get_accessibility, is_accessible_from_globally},
    member::get_members,
};
use pernixc_term::{
    generic_arguments::Symbol,
    generic_parameters::get_generic_parameters,
    instantiation::Instantiation,
    r#type::{self, Qualifier, Type},
};

use crate::{
    binder::{Binder, UnrecoverableError, type_check::Expected},
    infer::constraint,
    pattern::bind::diagnostic::{
        AlreadyBoundFieldPattern, Diagnostic, ExpectedAssociatedPattern,
        ExpectedTuplePackPattern, FieldIsNotAccessible, FieldNotFound,
        MismatchedPatternBindingType, MismatchedTuplePatternLength,
        MoreThanOnePackedTuplePattern, PatternBindingType,
        TooLargeNumericLiteral, UnboundFields, UnexpectedAssociatedPattern,
    },
};

pub mod diagnostic;

/// A trait for transforming a syntax tree pattern into a semantic pattern.
pub trait Bind:
    From<Wildcard>
    + From<Named>
    + From<Tuple<Self>>
    + From<Structural<Self>>
    + SourceElement<Location = RelativeLocation>
{
    /// The syntax tree type that this pattern can be bound from.
    type SyntaxTree: AbstractTree;

    /// Binds the syntax tree pattern to a semantic pattern given the expected
    /// type.
    #[allow(async_fn_in_trait)]
    async fn bind(
        binder: &mut Binder<'_>,
        ty: &Type,
        syntax_tree: &Self::SyntaxTree,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Self>, UnrecoverableError>;
}

impl Bind for Refutable {
    type SyntaxTree = pernixc_syntax::pattern::Refutable;

    async fn bind(
        binder: &mut Binder<'_>,
        ty: &Type,
        syntax_tree: &Self::SyntaxTree,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Self>, UnrecoverableError> {
        match syntax_tree {
            pernixc_syntax::pattern::Refutable::Boolean(boolean) => Ok(binder
                .bind_boolean_pattern(boolean, ty, handler)
                .await?
                .map(Refutable::Boolean)),
            pernixc_syntax::pattern::Refutable::Integer(integer) => Ok(binder
                .bind_integer_pattern(ty, integer, handler)
                .await?
                .map(Refutable::Integer)),
            pernixc_syntax::pattern::Refutable::Struct(st) => {
                Ok(Box::pin(binder.bind_struct_pattern(st, ty, handler))
                    .await?
                    .map(Refutable::Structural))
            }
            pernixc_syntax::pattern::Refutable::Enum(enum_pattern) => {
                Ok(Box::pin(binder.bind_enum(enum_pattern, ty, handler))
                    .await?
                    .map(Refutable::Enum))
            }
            pernixc_syntax::pattern::Refutable::Named(named) => {
                Ok(bind_named_pattern(named).map(Refutable::Named))
            }
            pernixc_syntax::pattern::Refutable::Tuple(tuple) => {
                Ok(Box::pin(binder.bind_tuple(tuple, ty, handler))
                    .await?
                    .map(Refutable::Tuple))
            }
            pernixc_syntax::pattern::Refutable::Wildcard(wildcard) => {
                Ok(Some(Wildcard { span: wildcard.span() }.into()))
            }
        }
    }
}

impl Bind for Irrefutable {
    type SyntaxTree = pernixc_syntax::pattern::Irrefutable;

    async fn bind(
        binder: &mut Binder<'_>,
        ty: &Type,
        syntax_tree: &Self::SyntaxTree,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Self>, UnrecoverableError> {
        match syntax_tree {
            pernixc_syntax::pattern::Irrefutable::Named(named) => {
                Ok(bind_named_pattern(named).map(Irrefutable::Named))
            }
            pernixc_syntax::pattern::Irrefutable::Tuple(tuple) => {
                Ok(Box::pin(binder.bind_tuple(tuple, ty, handler))
                    .await?
                    .map(Irrefutable::Tuple))
            }
            pernixc_syntax::pattern::Irrefutable::Struct(structural) => Ok(
                Box::pin(binder.bind_struct_pattern(structural, ty, handler))
                    .await?
                    .map(Irrefutable::Structural),
            ),
            pernixc_syntax::pattern::Irrefutable::Wildcard(wildcard) => {
                Ok(Some(Wildcard { span: wildcard.span() }.into()))
            }
        }
    }
}

fn bind_named_pattern(
    syntax_tree: &pernixc_syntax::pattern::Named,
) -> Option<Named> {
    let identifier = syntax_tree.identifier()?;

    Some(Named {
        span: identifier.span,
        name: identifier.kind.0,
        is_mutable: syntax_tree.mutable_keyword().is_some(),
        reference_binding: syntax_tree.reference_of().as_ref().map(|x| {
            if x.mut_keyword().is_some() {
                Qualifier::Mutable
            } else {
                Qualifier::Immutable
            }
        }),
    })
}

impl Binder<'_> {
    async fn bind_boolean_pattern(
        &mut self,
        syntax_tree: &pernixc_syntax::pattern::Boolean,
        mut ty: &Type,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Boolean>, UnrecoverableError> {
        ty = ty.reduce_reference();

        if self
            .type_check(
                ty,
                Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
                syntax_tree.span(),
                handler,
            )
            .await?
        {
            Ok(Some(Boolean {
                value: syntax_tree.is_true(),
                span: syntax_tree.span(),
            }))
        } else {
            handler.receive(
                Diagnostic::MismatchedPatternBindingType(
                    MismatchedPatternBindingType {
                        expected: PatternBindingType::Boolean,
                        found: ty.clone(),
                        span: syntax_tree.span(),
                        type_inference_map: self.type_inference_rendering_map(),
                        constant_inference_map: self
                            .constant_inference_rendering_map(),
                    },
                )
                .into(),
            );

            Ok(None)
        }
    }

    async fn bind_integer_pattern(
        &mut self,
        mut ty: &Type,
        syntax_tree: &pernixc_syntax::pattern::Integer,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Integer>, UnrecoverableError> {
        ty = ty.reduce_reference();

        let Some(numeric_str) = syntax_tree.numeric() else {
            return Ok(None);
        };

        let value = match numeric_str.kind.0.as_ref().parse::<u64>() {
            Ok(value) => value,
            Err(err) => match err.kind() {
                std::num::IntErrorKind::NegOverflow
                | std::num::IntErrorKind::PosOverflow => {
                    handler.receive(
                        Diagnostic::TooLargeNumericLiteral(
                            TooLargeNumericLiteral { span: numeric_str.span },
                        )
                        .into(),
                    );
                    return Ok(None);
                }

                _ => unreachable!(),
            },
        };

        let is_negative = syntax_tree.minus().is_some() && value != 0;

        if self
            .type_check(
                ty,
                Expected::Constraint(if is_negative {
                    constraint::Type::SignedInteger
                } else {
                    constraint::Type::Integer
                }),
                syntax_tree.span(),
                handler,
            )
            .await?
        {
            Ok(Some(Integer {
                value: if is_negative {
                    SwitchValue::Negative(NonZero::new(value).unwrap())
                } else {
                    SwitchValue::Positive(value)
                },
                span: syntax_tree.span(),
            }))
        } else {
            Ok(None)
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn bind_tuple<T: Bind>(
        &mut self,
        syntax_tree: &pernixc_syntax::pattern::Tuple<T::SyntaxTree>,
        mut ty: &Type,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Tuple<T>>, UnrecoverableError> {
        ty = ty.reduce_reference();

        let Type::Tuple(tuple_ty) = ty else {
            handler.receive(
                Diagnostic::MismatchedPatternBindingType(
                    MismatchedPatternBindingType {
                        expected: PatternBindingType::Tuple,
                        found: ty.clone(),
                        span: syntax_tree.span(),
                        type_inference_map: self.type_inference_rendering_map(),
                        constant_inference_map: self
                            .constant_inference_rendering_map(),
                    },
                )
                .into(),
            );
            return Ok(None);
        };

        // find the position of the unpacked element in type
        let unpacked_position_in_type = {
            let unpacked_count =
                tuple_ty.elements.iter().filter(|x| x.is_unpacked).count();

            match unpacked_count {
                0 => None,
                1 => Some(
                    tuple_ty
                        .elements
                        .iter()
                        .position(|x| x.is_unpacked)
                        .unwrap(),
                ),

                _ => {
                    panic!(
                        "tuple has more than one unpacked element \
                         {tuple_ty:#?}"
                    )
                }
            }
        };

        // normal tuple pattern, the number of pattern must exactly
        // match
        let tuple_element_patterns = syntax_tree.types().collect::<Vec<_>>();

        // find the position of the packed element in pattern
        let packed_position_in_pattern = {
            let packed_count = tuple_element_patterns
                .iter()
                .filter(|x| x.ellipsis().is_some())
                .count();

            match packed_count {
                0 => None,
                1 => Some(
                    tuple_element_patterns
                        .iter()
                        .position(|x| x.ellipsis().is_some())
                        .unwrap(),
                ),

                _ => {
                    handler.receive(
                        Diagnostic::MoreThanOnePackedTuplePattern(
                            MoreThanOnePackedTuplePattern {
                                illegal_tuple_pattern_span: syntax_tree.span(),
                            },
                        )
                        .into(),
                    );

                    return Ok(None);
                }
            }
        };

        if let Some(packed_position_in_pattern) = packed_position_in_pattern {
            // check length
            if tuple_element_patterns.len() > tuple_ty.elements.len() + 1 {
                handler.receive(
                    Diagnostic::MismatchedTuplePatternLength(
                        MismatchedTuplePatternLength {
                            pattern_span: syntax_tree.span(),
                            pattern_element_count: tuple_element_patterns.len(),
                            type_element_count: tuple_ty.elements.len(),
                        },
                    )
                    .into(),
                );
                return Ok(None);
            }

            let start_range = 0..packed_position_in_pattern;
            let tuple_end_range =
                packed_position_in_pattern + 1..tuple_element_patterns.len();
            let type_end_range = (tuple_ty.elements.len()
                - tuple_end_range.len())
                ..tuple_ty.elements.len();
            let type_pack_range =
                packed_position_in_pattern..type_end_range.start;

            if let Some(unpacked_position_in_type) = unpacked_position_in_type {
                // need to be packed
                if start_range.contains(&unpacked_position_in_type) {
                    handler.receive(
                        Diagnostic::ExpectedTuplePackPattern(
                            ExpectedTuplePackPattern {
                                illegal_tuple_span: tuple_element_patterns
                                    .get(unpacked_position_in_type)
                                    .unwrap()
                                    .span(),
                            },
                        )
                        .into(),
                    );
                    return Ok(None);
                }

                if !type_pack_range.contains(&unpacked_position_in_type) {
                    let translated_end =
                        unpacked_position_in_type - type_pack_range.len() + 1;

                    handler.receive(
                        Diagnostic::ExpectedTuplePackPattern(
                            ExpectedTuplePackPattern {
                                illegal_tuple_span: tuple_element_patterns
                                    .get(translated_end)
                                    .unwrap()
                                    .span(),
                            },
                        )
                        .into(),
                    );
                    return Ok(None);
                }
            }

            // match start
            let mut elements = Vec::new();

            for (tuple_ty, tuple_pat) in tuple_ty.elements[start_range.clone()]
                .iter()
                .zip(&tuple_element_patterns[start_range])
            {
                assert!(!tuple_ty.is_unpacked);
                let pattern = if let Some(pattern) = tuple_pat.pattern() {
                    T::bind(self, &tuple_ty.term, &pattern, handler)
                        .await?
                        .unwrap_or_else(|| {
                            Wildcard { span: pattern.span() }.into()
                        })
                } else {
                    Wildcard { span: tuple_pat.span() }.into()
                };

                elements.push(TupleElement::new_non_packed(pattern));
            }

            let packed_type = Type::Tuple(pernixc_term::tuple::Tuple {
                elements: tuple_ty.elements[type_pack_range].to_vec(),
            });

            elements.push(TupleElement::new_packed(
                if let Some(pattern) =
                    tuple_element_patterns[packed_position_in_pattern].pattern()
                {
                    T::bind(self, &packed_type, &pattern, handler)
                        .await?
                        .unwrap_or_else(|| {
                            Wildcard { span: pattern.span() }.into()
                        })
                } else {
                    Wildcard {
                        span: tuple_element_patterns
                            [packed_position_in_pattern]
                            .span(),
                    }
                    .into()
                },
            ));

            for (ty_elem, pat_elem) in tuple_ty.elements[type_end_range]
                .iter()
                .zip(&tuple_element_patterns[tuple_end_range])
            {
                assert!(!ty_elem.is_unpacked);

                elements.push(TupleElement::new_non_packed(
                    if let Some(pattern) = pat_elem.pattern() {
                        T::bind(self, &ty_elem.term, &pattern, handler)
                            .await?
                            .unwrap_or_else(|| {
                                Wildcard { span: pattern.span() }.into()
                            })
                    } else {
                        Wildcard { span: pat_elem.span() }.into()
                    },
                ));
            }

            Ok(Some(Tuple { elements, span: syntax_tree.span() }))
        } else {
            // count must exactly match
            if tuple_element_patterns.len() != tuple_ty.elements.len() {
                handler.receive(
                    Diagnostic::MismatchedTuplePatternLength(
                        MismatchedTuplePatternLength {
                            pattern_span: syntax_tree.span(),
                            pattern_element_count: tuple_element_patterns.len(),
                            type_element_count: tuple_ty.elements.len(),
                        },
                    )
                    .into(),
                );
                return Ok(None);
            }

            // must not have unpacked element
            if let Some(unpacked_tuple_position) = unpacked_position_in_type {
                handler.receive(
                    Diagnostic::ExpectedTuplePackPattern(
                        ExpectedTuplePackPattern {
                            illegal_tuple_span: tuple_element_patterns
                                .get(unpacked_tuple_position)
                                .unwrap()
                                .span(),
                        },
                    )
                    .into(),
                );
            }

            let mut elements = Vec::new();

            for (tuple_ty, tuple_pat) in tuple_ty
                .elements
                .iter()
                .map(|x| &x.term)
                .zip(tuple_element_patterns.iter())
            {
                elements.push(TupleElement::new_non_packed(
                    if let Some(pattern) = tuple_pat.pattern() {
                        T::bind(self, tuple_ty, &pattern, handler)
                            .await?
                            .unwrap_or_else(|| {
                                Wildcard { span: pattern.span() }.into()
                            })
                    } else {
                        Wildcard { span: tuple_pat.span() }.into()
                    },
                ));
            }

            Ok(Some(Tuple { elements, span: syntax_tree.span() }))
        }
    }

    #[allow(clippy::too_many_lines)]
    async fn bind_struct_pattern<T: Bind>(
        &mut self,
        syntax_tree: &pernixc_syntax::pattern::Struct<T::SyntaxTree>,
        mut ty: &Type,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Structural<T>>, UnrecoverableError> {
        ty = ty.reduce_reference();

        // must be a struct type
        let Type::Symbol(Symbol { id: struct_id, generic_arguments }) = ty
        else {
            handler.receive(
                Diagnostic::MismatchedPatternBindingType(
                    MismatchedPatternBindingType {
                        expected: PatternBindingType::Struct,
                        found: ty.clone(),
                        span: syntax_tree.span(),
                        type_inference_map: self.type_inference_rendering_map(),
                        constant_inference_map: self
                            .constant_inference_rendering_map(),
                    },
                )
                .into(),
            );
            return Ok(None);
        };

        let struct_id = *struct_id;

        let struct_generic_parameters =
            self.engine().get_generic_parameters(struct_id).await;

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id,
            &struct_generic_parameters,
        )
        .unwrap();

        let fields = self.engine().get_fields(struct_id).await;

        let mut patterns_by_field_id = HashMap::<_, T>::default();

        let mut found_wildcard = false;
        // iterate to each field
        for field in syntax_tree.fields() {
            let Some(field_name) = (match &field {
                pernixc_syntax::pattern::Field::FieldAssociation(
                    association,
                ) => association.identifier(),
                pernixc_syntax::pattern::Field::Named(named) => {
                    named.identifier()
                }
                pernixc_syntax::pattern::Field::Wildcard(_) => {
                    found_wildcard = true;
                    continue;
                }
            }) else {
                continue;
            };

            // get the field id
            let Some((field_sym, field_id)) = fields
                .field_ids_by_name
                .get(field_name.kind.0.as_ref())
                .copied()
                .map(|x| (&fields.fields[x], x))
            else {
                // field not found error
                handler.receive(
                    Diagnostic::FieldNotFound(FieldNotFound {
                        struct_id,
                        identifier_span: field_name.span,
                        field_name: field_name.kind.0,
                    })
                    .into(),
                );

                continue;
            };

            let entry = match patterns_by_field_id.entry(field_id) {
                std::collections::hash_map::Entry::Occupied(entry) => {
                    handler.receive(
                        Diagnostic::AlreadyBoundFieldPattern(
                            AlreadyBoundFieldPattern {
                                rebound_span: field.span(),
                                first_bound_span: entry.get().span(),
                                struct_id,
                                field_id,
                            },
                        )
                        .into(),
                    );
                    continue;
                }
                std::collections::hash_map::Entry::Vacant(entry) => entry,
            };

            // instantiation the type
            let mut field_ty = field_sym.r#type.clone();

            instantiation.instantiate(&mut field_ty);

            field_ty = self
                .simplify_type(field_ty, field.span(), handler)
                .await?
                .result
                .clone();

            // the pattern for the field
            let pattern = match field {
                pernixc_syntax::pattern::Field::FieldAssociation(
                    field_association,
                ) => 'pat: {
                    let Some(pattern) = field_association.pattern() else {
                        break 'pat T::from(Wildcard {
                            span: field_association.span(),
                        });
                    };

                    T::bind(self, &field_ty, &pattern, handler)
                        .await?
                        .unwrap_or_else(|| {
                            Wildcard { span: pattern.span() }.into()
                        })
                }
                pernixc_syntax::pattern::Field::Named(named) => {
                    bind_named_pattern(&named).map_or_else(
                        || T::from(Wildcard { span: named.span() }),
                        std::convert::Into::into,
                    )
                }

                pernixc_syntax::pattern::Field::Wildcard(_) => {
                    unreachable!()
                }
            };

            if !self
                .engine()
                .is_accessible_from_globally(
                    self.current_site(),
                    field_sym.accessibility.into_global(struct_id.target_id),
                )
                .await
            {
                // soft error, no need to stop the process
                handler.receive(
                    Diagnostic::FieldIsNotAccessible(FieldIsNotAccessible {
                        field_id,
                        struct_id,
                        referring_site: self.current_site(),
                        referring_identifier_span: field_name.span,
                    })
                    .into(),
                );
            }

            entry.insert(pattern);
        }

        let unbound_fields = fields
            .fields
            .ids()
            .filter(|x| !patterns_by_field_id.contains_key(x))
            .collect::<Vec<_>>();

        patterns_by_field_id.extend(
            unbound_fields
                .iter()
                .copied()
                .map(|x| (x, Wildcard { span: syntax_tree.span() }.into())),
        );

        // report the unbound fields
        if !unbound_fields.is_empty() && !found_wildcard {
            handler.receive(
                Diagnostic::UnboundFields(UnboundFields {
                    field_ids: unbound_fields,
                    struct_id,
                    pattern_span: syntax_tree.span(),
                })
                .into(),
            );
        }

        Ok(Some(Structural {
            struct_id,
            patterns_by_field_id,
            span: syntax_tree.span(),
        }))
    }

    #[allow(clippy::too_many_lines)]
    async fn bind_enum(
        &mut self,
        syntax_tree: &pernixc_syntax::pattern::Enum,
        mut ty: &Type,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<Enum>, UnrecoverableError> {
        let Some(identifier) = syntax_tree.identifier() else {
            return Ok(None);
        };

        ty = ty.reduce_reference();

        // must be an enum type
        let Type::Symbol(Symbol { id: enum_id, generic_arguments }) = ty else {
            handler.receive(
                Diagnostic::MismatchedPatternBindingType(
                    MismatchedPatternBindingType {
                        expected: PatternBindingType::Enum,
                        found: ty.clone(),
                        span: syntax_tree.span(),
                        type_inference_map: self.type_inference_rendering_map(),
                        constant_inference_map: self
                            .constant_inference_rendering_map(),
                    },
                )
                .into(),
            );
            return Ok(None);
        };

        let enum_id = *enum_id;

        let member = self.engine().get_members(enum_id).await;
        let enum_generic_param =
            self.engine().get_generic_parameters(enum_id).await;

        // variant not found
        let Some(variant_id) = member
            .member_ids_by_name
            .get(identifier.kind.0.as_ref())
            .copied()
            .map(|x| enum_id.target_id.make_global(x))
        else {
            handler.receive(
                ResolutionDiagnostic::SymbolNotFound(SymbolNotFound {
                    name: identifier.kind.0,
                    searched_item_id: Some(enum_id),
                    resolution_span: identifier.span,
                })
                .into(),
            );

            return Ok(None);
        };

        // check if the variant is accessible
        if !self
            .engine()
            .is_accessible_from_globally(
                self.current_site(),
                self.engine()
                    .get_accessibility(variant_id)
                    .await
                    .into_global(enum_id.target_id),
            )
            .await
        {
            // soft error, report and continue
            handler.receive(
                ResolutionDiagnostic::SymbolIsNotAccessible(
                    SymbolIsNotAccessible {
                        referring_site: self.current_site(),
                        referred: variant_id,
                        referred_span: identifier.span,
                    },
                )
                .into(),
            );
        }

        let variant_sym =
            self.engine().get_variant_associated_type(variant_id).await;

        match (
            &variant_sym,
            syntax_tree.association().and_then(|x| x.pattern()),
        ) {
            (Some(ty), Some(pat)) => {
                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    enum_id,
                    &enum_generic_param,
                )
                .unwrap();

                let mut variant_ty = ty.deref().clone();

                instantiation.instantiate(&mut variant_ty);

                let simplification = self
                    .simplify_type(variant_ty, syntax_tree.span(), handler)
                    .await?;

                let pattern = Refutable::bind(
                    self,
                    &simplification.result,
                    &pat,
                    handler,
                )
                .await?
                .unwrap_or_else(|| {
                    Refutable::Wildcard(Wildcard { span: pat.span() })
                });

                Ok(Some(Enum {
                    variant_id,
                    pattern: Some(Box::new(pattern)),
                    span: syntax_tree.span(),
                }))
            }
            (None, None) => Ok(Some(Enum {
                variant_id,
                pattern: None,
                span: syntax_tree.span(),
            })),
            (Some(_), None) => {
                handler.receive(
                    Diagnostic::ExpectedAssociatedPattern(
                        ExpectedAssociatedPattern {
                            variant_id,
                            pattern_span: syntax_tree.span(),
                        },
                    )
                    .into(),
                );
                Ok(None)
            }
            (None, Some(_)) => {
                handler.receive(
                    Diagnostic::UnexpectedAssociatedPattern(
                        UnexpectedAssociatedPattern {
                            associated_pattern_span: syntax_tree.span(),
                            variant_id,
                        },
                    )
                    .into(),
                );
                Ok(None)
            }
        }
    }
}

impl Binder<'_> {
    /// Binds a syntax tree pattern into a semantic pattern given the expected
    /// type.
    pub async fn bind_pattern<T: Bind>(
        &mut self,
        syntax_tree: &T::SyntaxTree,
        ty: &Type,
        handler: &dyn Handler<crate::diagnostic::Diagnostic>,
    ) -> Result<Option<T>, UnrecoverableError> {
        T::bind(self, ty, syntax_tree, handler).await
    }
}
