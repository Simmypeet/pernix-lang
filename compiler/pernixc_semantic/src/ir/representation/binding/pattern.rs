//! Contains the code to bind a pattern syntax tree to the IR.

use std::collections::{HashMap, VecDeque};

use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::{infer, Binder};
use crate::{
    arena::ID,
    error::{
        AlreadyBoundFieldPattern, Error, ExpectAssociatedPattern,
        ExpectTuplePackPattern, FieldIsNotAccessible, FieldNotFound,
        FoundPackTuplePatternInReferenceBoundTupleType,
        MismatchedPatternBindingType, MismatchedQualifierForReferenceOf,
        MismatchedTuplePatternLength, MoreThanOnePackedTuplePattern,
        OverflowOperation, PatternBindingType, SymbolIsNotAccessible,
        SymbolNotFound, TooLargetNumericLiteral, TypeSystemOverflow,
        UnboundFields, UnexpectedAssociatedPattern,
    },
    ir::{
        self,
        address::{self, Address, Memory, Variant},
        instruction::{self, Instruction, Store, TuplePack},
        pattern::{
            Boolean, Enum, Integer, Irrefutable, NameBinding, NameBindingPoint,
            Named, Refutable, Structural, Tuple, TupleElement, Wildcard,
        },
        value::{
            register::{Assignment, Load, ReferenceOf},
            Value,
        },
        Erased,
    },
    symbol::{
        table::{self, representation::Index, resolution},
        AdtID, Field,
    },
    type_system::{
        self,
        instantiation::{self, Instantiation},
        model::Model,
        simplify,
        term::{
            self,
            lifetime::Lifetime,
            r#type::{self, Constraint, Qualifier, Reference, Type},
            Symbol,
        },
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum BindingKind {
    Value,
    Reference,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct Binding<'a> {
    kind: BindingKind,
    r#type: &'a Type<infer::Model>,
    address: Address<infer::Model>,
    qualifier: Qualifier,
}

pub(super) trait Pattern:
    From<Wildcard>
    + From<Named>
    + From<Tuple<Self>>
    + From<Structural<Self>>
    + SourceElement
{
    type SyntaxTree: SourceElement;

    fn bind<
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    >(
        binder: &mut Binder<S, RO, TO>,
        ty: &Type<infer::Model>,
        syntax_tree: &Self::SyntaxTree,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Self>, TypeSystemOverflow>;

    fn insert_named_binding_point<
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    >(
        binder: &mut Binder<S, RO, TO>,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        pattern: &Self,
        address_span: Option<Span>,
        binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<(), TypeSystemOverflow>;
}

impl Pattern for Refutable {
    type SyntaxTree = syntax_tree::pattern::Refutable;

    fn bind<
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    >(
        binder: &mut Binder<S, RO, TO>,
        ty: &Type<infer::Model>,
        syntax_tree: &Self::SyntaxTree,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Self>, TypeSystemOverflow> {
        match syntax_tree {
            syntax_tree::pattern::Refutable::Boolean(boolean) => {
                Ok(binder.bind_boolean(boolean, ty, handler)?.map(Into::into))
            }
            syntax_tree::pattern::Refutable::Integer(integer) => {
                Ok(binder.bind_integer(ty, integer, handler)?.map(Into::into))
            }
            syntax_tree::pattern::Refutable::Structural(structural) => {
                Ok(binder
                    .bind_structural(structural, ty, handler)?
                    .map(Into::into))
            }
            syntax_tree::pattern::Refutable::Enum(syn) => {
                Ok(binder.bind_enum(syn, ty, handler)?.map(Into::into))
            }
            syntax_tree::pattern::Refutable::Named(named) => {
                Ok(Some(bind_named_pattern(named).into()))
            }
            syntax_tree::pattern::Refutable::Tuple(tuple) => {
                Ok(binder.bind_tuple(tuple, ty, handler)?.map(Into::into))
            }
            syntax_tree::pattern::Refutable::Wildcard(wildcard) => {
                Ok(Some(Wildcard { span: wildcard.span() }.into()))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn insert_named_binding_point<
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    >(
        binder: &mut Binder<S, RO, TO>,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        pattern: &Self,
        address_span: Option<Span>,
        mut binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<(), TypeSystemOverflow> {
        match pattern {
            Self::Named(pat) => {
                match (pat.reference_binding, binding.kind) {
                    // obtains the reference of the value.
                    (
                        Some(qualifier),
                        BindingKind::Value | BindingKind::Reference,
                    ) => {
                        if qualifier > binding.qualifier {
                            handler.receive(Box::new(
                                MismatchedQualifierForReferenceOf {
                                    reference_of_span: pat.span.clone(),
                                    found_qualifier: binding.qualifier,
                                    expected_qualifier: qualifier,
                                    is_behind_reference: binding
                                        .address
                                        .is_behind_reference(),
                                },
                            ));
                        }

                        binder.create_reference_bound_named_pattern(
                            name_binding_point,
                            binding.r#type.clone(),
                            qualifier,
                            binding.address,
                            address_span,
                            pat.span.clone(),
                            pat.name.clone(),
                            pat.is_mutable,
                            handler,
                        );

                        Ok(())
                    }

                    // normal value binding
                    (None, BindingKind::Value) => {
                        let load_address = if must_copy {
                            let alloca_id = binder.create_alloca(
                                binding.r#type.clone(),
                                pat.span.clone(),
                            );

                            // copy/move the value from the given address
                            let load_value = Value::Register(
                                binder.create_register_assignmnet(
                                    Assignment::Load(Load {
                                        address: binding.address,
                                    }),
                                    address_span
                                        .unwrap_or_else(|| pat.span.clone()),
                                ),
                            );

                            // store the value to the alloca
                            let _ =
                                binder.current_block_mut().insert_instruction(
                                    Instruction::Store(Store {
                                        address: Address::Memory(
                                            Memory::Alloca(alloca_id),
                                        ),
                                        value: load_value,
                                    }),
                                );

                            Address::Memory(Memory::Alloca(alloca_id))
                        } else {
                            binding.address
                        };

                        let _ = name_binding_point.insert(
                            pat.name.clone(),
                            NameBinding {
                                mutable: pat.is_mutable,
                                load_address,
                                span: pat.span.clone(),
                            },
                            handler,
                        );

                        Ok(())
                    }

                    (None, BindingKind::Reference) => {
                        binder.create_reference_bound_named_pattern(
                            name_binding_point,
                            binding.r#type.clone(),
                            binding.qualifier,
                            binding.address,
                            address_span,
                            pat.span.clone(),
                            pat.name.clone(),
                            pat.is_mutable,
                            handler,
                        );

                        Ok(())
                    }
                }
            }

            Self::Tuple(pat) => binder.insert_named_binding_point_tuple(
                name_binding_point,
                pat,
                &address_span,
                binding,
                must_copy,
                handler,
            ),

            Self::Structural(pat) => binder
                .insert_named_binding_point_structural(
                    name_binding_point,
                    pat,
                    &address_span,
                    binding,
                    must_copy,
                    handler,
                ),

            Self::Wildcard(_) | Self::Boolean(_) | Self::Integer(_) => Ok(()),

            Self::Enum(variant) => {
                if let Some(inner) = &variant.pattern {
                    binding = reduce_reference(binding);

                    // must be an enum type
                    let Type::Symbol(Symbol {
                        id: r#type::SymbolID::Adt(AdtID::Enum(enum_id)),
                        generic_arguments,
                    }) = binding.r#type
                    else {
                        panic!("unexpected type!");
                    };

                    let enum_symbol = binder.table.get(*enum_id).unwrap();

                    let instantiation = Instantiation::from_generic_arguments(
                        generic_arguments.clone(),
                        (*enum_id).into(),
                        &enum_symbol.generic_declaration.parameters,
                    )
                    .unwrap();

                    let mut variant_ty = infer::Model::from_default_type(
                        binder
                            .table
                            .get(variant.variant_id)
                            .unwrap()
                            .associated_type
                            .clone()
                            .unwrap(),
                    );

                    instantiation::instantiate(&mut variant_ty, &instantiation);

                    Self::insert_named_binding_point(
                        binder,
                        name_binding_point,
                        &**inner,
                        address_span,
                        Binding {
                            kind: binding.kind,
                            r#type: &variant_ty,
                            address: Address::Variant(Variant {
                                enum_address: Box::new(binding.address),
                                id: variant.variant_id,
                            }),
                            qualifier: binding.qualifier,
                        },
                        must_copy,
                        handler,
                    )?;
                }

                Ok(())
            }
        }
    }
}

fn reduce_reference(mut binding: Binding) -> Binding {
    loop {
        match binding.r#type {
            Type::Reference(reference) => {
                // update the address, reference binding
                // info, and binding ty
                binding = Binding {
                    kind: BindingKind::Reference,
                    r#type: &reference.pointee,
                    qualifier: reference.qualifier.min(
                        if binding.address.is_behind_reference() {
                            binding.qualifier
                        } else {
                            Qualifier::Mutable
                        },
                    ),
                    address: Address::Reference(address::Reference {
                        qualifier: reference.qualifier,
                        reference_address: Box::new(binding.address),
                    }),
                };
            }

            _ => break binding,
        }
    }
}

impl Pattern for Irrefutable {
    type SyntaxTree = syntax_tree::pattern::Irrefutable;

    fn bind<
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    >(
        binder: &mut Binder<S, RO, TO>,
        ty: &Type<infer::Model>,
        syntax_tree: &Self::SyntaxTree,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Self>, TypeSystemOverflow> {
        match syntax_tree {
            syntax_tree::pattern::Irrefutable::Structural(structural) => {
                Ok(binder
                    .bind_structural(structural, ty, handler)?
                    .map(Into::into))
            }
            syntax_tree::pattern::Irrefutable::Named(named) => {
                Ok(Some(bind_named_pattern(named).into()))
            }
            syntax_tree::pattern::Irrefutable::Tuple(tuple) => {
                Ok(binder.bind_tuple(tuple, ty, handler)?.map(Into::into))
            }
            syntax_tree::pattern::Irrefutable::Wildcard(wildcard) => {
                Ok(Some(Wildcard { span: wildcard.span() }.into()))
            }
        }
    }

    #[allow(clippy::too_many_lines)]
    fn insert_named_binding_point<
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    >(
        binder: &mut Binder<S, RO, TO>,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        pattern: &Self,
        address_span: Option<Span>,
        binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<(), TypeSystemOverflow> {
        match pattern {
            Self::Named(pat) => {
                match (pat.reference_binding, binding.kind) {
                    // obtains the reference of the value.
                    (
                        Some(qualifier),
                        BindingKind::Value | BindingKind::Reference,
                    ) => {
                        if qualifier > binding.qualifier {
                            handler.receive(Box::new(
                                MismatchedQualifierForReferenceOf {
                                    reference_of_span: address_span
                                        .clone()
                                        .unwrap_or_else(|| pat.span.clone()),
                                    found_qualifier: binding.qualifier,
                                    expected_qualifier: qualifier,
                                    is_behind_reference: binding
                                        .address
                                        .is_behind_reference(),
                                },
                            ));
                        }

                        binder.create_reference_bound_named_pattern(
                            name_binding_point,
                            binding.r#type.clone(),
                            qualifier,
                            binding.address,
                            address_span,
                            pat.span.clone(),
                            pat.name.clone(),
                            pat.is_mutable,
                            handler,
                        );

                        Ok(())
                    }

                    // normal value binding
                    (None, BindingKind::Value) => {
                        let load_address = if must_copy {
                            let alloca_id = binder.create_alloca(
                                binding.r#type.clone(),
                                address_span
                                    .clone()
                                    .unwrap_or_else(|| pat.span.clone()),
                            );

                            // copy/move the value from the given address
                            let load_value = Value::Register(
                                binder.create_register_assignmnet(
                                    Assignment::Load(Load {
                                        address: binding.address,
                                    }),
                                    address_span
                                        .unwrap_or_else(|| pat.span.clone()),
                                ),
                            );

                            // store the value to the alloca
                            let _ =
                                binder.current_block_mut().insert_instruction(
                                    Instruction::Store(Store {
                                        address: Address::Memory(
                                            Memory::Alloca(alloca_id),
                                        ),
                                        value: load_value,
                                    }),
                                );

                            Address::Memory(Memory::Alloca(alloca_id))
                        } else {
                            binding.address
                        };

                        let _ = name_binding_point.insert(
                            pat.name.clone(),
                            NameBinding {
                                mutable: pat.is_mutable,
                                load_address,
                                span: pat.span.clone(),
                            },
                            handler,
                        );

                        Ok(())
                    }

                    (None, BindingKind::Reference) => {
                        binder.create_reference_bound_named_pattern(
                            name_binding_point,
                            binding.r#type.clone(),
                            binding.qualifier,
                            binding.address,
                            address_span,
                            pat.span.clone(),
                            pat.name.clone(),
                            pat.is_mutable,
                            handler,
                        );

                        Ok(())
                    }
                }
            }

            Self::Tuple(pat) => binder.insert_named_binding_point_tuple(
                name_binding_point,
                pat,
                &address_span,
                binding,
                must_copy,
                handler,
            ),

            Self::Structural(pat) => binder
                .insert_named_binding_point_structural(
                    name_binding_point,
                    pat,
                    &address_span,
                    binding,
                    must_copy,
                    handler,
                ),

            Self::Wildcard(_) => Ok(()),
        }
    }
}

fn bind_named_pattern(syntax_tree: &syntax_tree::pattern::Named) -> Named {
    Named {
        name: syntax_tree.identifier().span.str().to_owned(),
        span: syntax_tree.identifier().span.clone(),
        is_mutable: syntax_tree.mutable_keyword().is_some(),
        reference_binding: syntax_tree.reference_of().as_ref().map(|x| {
            if x.mutable_keyword().is_some() {
                Qualifier::Mutable
            } else {
                Qualifier::Immutable
            }
        }),
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Binder<'t, S, RO, TO>
{
    #[allow(clippy::too_many_lines)]
    fn bind_tuple<T: Pattern>(
        &mut self,
        syntax_tree: &syntax_tree::pattern::Tuple<T::SyntaxTree>,
        mut ty: &Type<infer::Model>,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Tuple<T>>, TypeSystemOverflow> {
        ty = ty.reduce_reference();

        let Type::Tuple(tuple_ty) = ty else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Tuple,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));
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
        let tuple_element_patterns = syntax_tree
            .connected_list()
            .iter()
            .flat_map(ConnectedList::elements)
            .collect::<Vec<_>>();

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
                    handler.receive(Box::new(MoreThanOnePackedTuplePattern {
                        illegal_tuple_pattern_span: syntax_tree.span(),
                    }));

                    return Ok(None);
                }
            }
        };

        if let Some(packed_position_in_pattern) = packed_position_in_pattern {
            // check length
            if tuple_element_patterns.len() > tuple_ty.elements.len() + 1 {
                handler.receive(Box::new(MismatchedTuplePatternLength {
                    pattern_span: syntax_tree.span(),
                    pattern_element_count: tuple_element_patterns.len(),
                    type_element_count: tuple_ty.elements.len(),
                }));
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
                    handler.receive(Box::new(ExpectTuplePackPattern {
                        illegal_tuple_span: tuple_element_patterns
                            .get(unpacked_position_in_type)
                            .unwrap()
                            .span(),
                    }));
                    return Ok(None);
                }

                if !type_pack_range.contains(&unpacked_position_in_type) {
                    let translated_end =
                        unpacked_position_in_type - type_pack_range.len() + 1;

                    handler.receive(Box::new(ExpectTuplePackPattern {
                        illegal_tuple_span: tuple_element_patterns
                            .get(translated_end)
                            .unwrap()
                            .span(),
                    }));
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

                elements.push(TupleElement::new_non_packed(
                    T::bind(
                        self,
                        &tuple_ty.term,
                        tuple_pat.pattern(),
                        handler,
                    )?
                    .unwrap_or_else(|| {
                        Wildcard { span: tuple_pat.pattern().span() }.into()
                    }),
                ));
            }

            let packed_type = Type::Tuple(term::Tuple {
                elements: tuple_ty.elements[type_pack_range].to_vec(),
            });
            elements.push(TupleElement::new_packed(
                T::bind(
                    self,
                    &packed_type,
                    tuple_element_patterns[packed_position_in_pattern]
                        .pattern(),
                    handler,
                )?
                .unwrap_or_else(|| {
                    Wildcard {
                        span: tuple_element_patterns
                            [packed_position_in_pattern]
                            .pattern()
                            .span(),
                    }
                    .into()
                }),
            ));

            for (ty_elem, pat_elem) in tuple_ty.elements[type_end_range]
                .iter()
                .zip(&tuple_element_patterns[tuple_end_range])
            {
                assert!(!ty_elem.is_unpacked);

                elements.push(TupleElement::new_non_packed(
                    T::bind(self, &ty_elem.term, pat_elem.pattern(), handler)?
                        .unwrap_or_else(|| {
                            Wildcard { span: pat_elem.pattern().span() }.into()
                        }),
                ));
            }

            Ok(Some(Tuple { elements, span: syntax_tree.span() }))
        } else {
            // count must exactly match
            if tuple_element_patterns.len() != tuple_ty.elements.len() {
                handler.receive(Box::new(MismatchedTuplePatternLength {
                    pattern_span: syntax_tree.span(),
                    pattern_element_count: tuple_element_patterns.len(),
                    type_element_count: tuple_ty.elements.len(),
                }));
                return Ok(None);
            }

            // must not have unpacked element
            if let Some(unpacked_tuple_position) = unpacked_position_in_type {
                handler.receive(Box::new(ExpectTuplePackPattern {
                    illegal_tuple_span: tuple_element_patterns
                        .get(unpacked_tuple_position)
                        .unwrap()
                        .span(),
                }));
            }

            let mut elements = Vec::new();

            for (tuple_ty, tuple_pat) in tuple_ty
                .elements
                .iter()
                .map(|x| &x.term)
                .zip(tuple_element_patterns.iter().copied())
            {
                elements.push(TupleElement::new_non_packed(
                    T::bind(self, tuple_ty, tuple_pat.pattern(), handler)?
                        .unwrap_or_else(|| {
                            Wildcard { span: tuple_pat.pattern().span() }.into()
                        }),
                ));
            }

            Ok(Some(Tuple { elements, span: syntax_tree.span() }))
        }
    }

    fn bind_boolean(
        &mut self,
        syntax_tree: &syntax_tree::expression::Boolean,
        mut ty: &Type<infer::Model>,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Boolean>, TypeSystemOverflow> {
        ty = ty.reduce_reference();

        if self.type_check(
            ty,
            r#type::Expected::Known(Type::Primitive(r#type::Primitive::Bool)),
            syntax_tree.span(),
            true,
            handler,
        )? {
            Ok(Some(Boolean {
                value: syntax_tree.is_true(),
                span: syntax_tree.span(),
            }))
        } else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Boolean,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));

            Ok(None)
        }
    }

    #[allow(clippy::too_many_lines)]
    fn bind_structural<T: Pattern>(
        &mut self,
        syntax_tree: &syntax_tree::pattern::Structural<T::SyntaxTree>,
        mut ty: &Type<infer::Model>,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Structural<T>>, TypeSystemOverflow> {
        ty = ty.reduce_reference();

        // must be a struct type
        let Type::Symbol(Symbol {
            id: r#type::SymbolID::Adt(AdtID::Struct(struct_id)),
            generic_arguments,
        }) = ty
        else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Struct,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));
            return Ok(None);
        };

        let struct_id = *struct_id;

        let struct_symbol = self.table.get(struct_id).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id.into(),
            &struct_symbol.generic_declaration.parameters,
        )
        .unwrap();

        let mut patterns_by_field_id = HashMap::new();

        // iterate to each field
        for field in
            syntax_tree.fields().iter().flat_map(ConnectedList::elements)
        {
            let field_name = match field {
                syntax_tree::pattern::Field::Association(association) => {
                    association.identifier().span.str()
                }
                syntax_tree::pattern::Field::Named(named) => {
                    named.identifier().span.str()
                }
            };

            // get the field id
            let Some((field_sym, field_id)) = struct_symbol
                .fields()
                .get_id(field_name)
                .map(|x| (struct_symbol.fields().get(x).unwrap(), x))
            else {
                // field not found error
                handler.receive(Box::new(FieldNotFound {
                    identifier_span: match field {
                        syntax_tree::pattern::Field::Association(pat) => {
                            pat.identifier().span.clone()
                        }
                        syntax_tree::pattern::Field::Named(pat) => {
                            pat.identifier().span.clone()
                        }
                    },
                    struct_id,
                }));

                continue;
            };

            let entry = match patterns_by_field_id.entry(field_id) {
                std::collections::hash_map::Entry::Occupied(_) => {
                    handler.receive(Box::new(AlreadyBoundFieldPattern {
                        pattern_span: field.span(),
                        struct_id,
                        field_id,
                    }));
                    continue;
                }
                std::collections::hash_map::Entry::Vacant(entry) => entry,
            };

            // instantiation the type
            let mut field_ty =
                infer::Model::from_default_type(field_sym.r#type.clone());

            instantiation::instantiate(&mut field_ty, &instantiation);
            let simplification =
                simplify::simplify(&field_ty, &self.create_environment())
                    .map_err(|overflow_error| TypeSystemOverflow {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: field.span(),
                        overflow_error,
                    })?;
            field_ty = simplification.result;

            // the pattern for the field
            let pattern = match field {
                syntax_tree::pattern::Field::Association(assoc) => {
                    T::bind(self, &field_ty, assoc.pattern(), handler)?
                        .unwrap_or_else(|| {
                            Wildcard { span: assoc.pattern().span() }.into()
                        })
                }
                syntax_tree::pattern::Field::Named(named) => {
                    bind_named_pattern(named).into()
                }
            };

            if !self
                .table
                .is_accessible_from(self.current_site, field_sym.accessibility)
                .unwrap()
            {
                // soft error, no need to stop the process
                handler.receive(Box::new(FieldIsNotAccessible {
                    field_id,
                    struct_id,
                    referring_site: self.current_site,
                    referring_identifier_span: match field {
                        syntax_tree::pattern::Field::Association(pat) => {
                            pat.identifier().span.clone()
                        }
                        syntax_tree::pattern::Field::Named(pat) => {
                            pat.identifier().span.clone()
                        }
                    },
                }));
            }

            entry.insert(pattern);
        }

        let unbound_fields = struct_symbol
            .fields()
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
        if !unbound_fields.is_empty() && syntax_tree.wildcard().is_none() {
            handler.receive(Box::new(UnboundFields {
                field_ids: unbound_fields,
                struct_id,
                pattern_span: syntax_tree.span(),
            }));
        }

        Ok(Some(Structural {
            struct_id,
            patterns_by_field_id,
            span: syntax_tree.span(),
        }))
    }

    #[allow(clippy::too_many_lines)]
    fn bind_enum(
        &mut self,
        syntax_tree: &syntax_tree::pattern::Enum,
        mut ty: &Type<infer::Model>,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Enum>, TypeSystemOverflow> {
        ty = ty.reduce_reference();

        // must be an enum type
        let Type::Symbol(Symbol {
            id: r#type::SymbolID::Adt(AdtID::Enum(enum_id)),
            generic_arguments,
        }) = ty
        else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Enum,
                found_type: self
                    .inference_context
                    .transform_type_into_constraint_model(ty.clone())
                    .map_err(|overflow_error| TypeSystemOverflow {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: syntax_tree.span(),
                        overflow_error,
                    })?,
                pattern_span: syntax_tree.span(),
            }));
            return Ok(None);
        };

        let enum_id = *enum_id;
        let enum_symbol = self.table.get(enum_id).unwrap();

        // variant not found
        let Some(variant_id) = enum_symbol
            .variant_ids_by_name()
            .get(syntax_tree.identifier().span.str())
            .copied()
        else {
            handler.receive(Box::new(SymbolNotFound {
                searched_global_id: Some(enum_id.into()),
                resolution_span: syntax_tree.identifier().span.clone(),
            }));

            return Ok(None);
        };

        // check if the variant is accessible
        if !self
            .table
            .is_accessible_from(
                self.current_site,
                self.table.get_accessibility(variant_id.into()).unwrap(),
            )
            .unwrap()
        {
            // soft error, report and continue
            handler.receive(Box::new(SymbolIsNotAccessible {
                referring_site: self.current_site,
                referred: variant_id.into(),
                referred_span: syntax_tree.identifier().span.clone(),
            }));
        }

        let variant_sym = self.table.get(variant_id).unwrap();

        match (&variant_sym.associated_type, syntax_tree.association()) {
            (Some(ty), Some(pat)) => {
                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    enum_id.into(),
                    &enum_symbol.generic_declaration.parameters,
                )
                .unwrap();

                let mut variant_ty =
                    infer::Model::from_default_type(ty.clone());

                instantiation::instantiate(&mut variant_ty, &instantiation);
                let simplification =
                    simplify::simplify(&variant_ty, &self.create_environment())
                        .map_err(|overflow_error| TypeSystemOverflow {
                            operation: OverflowOperation::TypeOf,
                            overflow_span: pat.span(),
                            overflow_error,
                        })?;

                let pattern = Refutable::bind(
                    self,
                    &simplification.result,
                    pat.tree(),
                    handler,
                )?
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
                handler.receive(Box::new(ExpectAssociatedPattern {
                    variant_id,
                    pattern_span: syntax_tree.span(),
                }));
                Ok(None)
            }
            (None, Some(_)) => {
                handler.receive(Box::new(UnexpectedAssociatedPattern {
                    associated_pattern_span: syntax_tree.span(),
                    variant_id,
                }));
                Ok(None)
            }
        }
    }

    fn bind_integer(
        &mut self,
        mut ty: &Type<infer::Model>,
        syntax_tree: &syntax_tree::pattern::Integer,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<Integer>, TypeSystemOverflow> {
        ty = ty.reduce_reference();
        let mut value = match syntax_tree.numeric().span.str().parse::<i128>() {
            Ok(value) => value,
            Err(err) => match err.kind() {
                std::num::IntErrorKind::NegOverflow
                | std::num::IntErrorKind::PosOverflow => {
                    handler.receive(Box::new(TooLargetNumericLiteral {
                        span: syntax_tree.numeric().span.clone(),
                    }));
                    return Ok(None);
                }

                _ => unreachable!(),
            },
        };

        if syntax_tree.minus().is_some() {
            value = -value;
        }

        if self.type_check(
            ty,
            r#type::Expected::Constraint(if syntax_tree.minus().is_some() {
                Constraint::SignedInteger
            } else {
                Constraint::Integer
            }),
            syntax_tree.span(),
            true,
            handler,
        )? {
            Ok(Some(Integer { value, span: syntax_tree.span() }))
        } else {
            Ok(None)
        }
    }

    pub(super) fn bind_pattern<T: Pattern>(
        &mut self,
        ty: &Type<infer::Model>,
        syntax_tree: &T::SyntaxTree,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<Option<T>, TypeSystemOverflow> {
        T::bind(self, ty, syntax_tree, handler)
    }

    /// Finds the [`Named`] pattern and adds it to the [`NameBindingPoint`].
    ///
    /// # Assumptions
    ///
    /// - The irrefutable pattern has been bound with the given
    ///   `simplified_type`.
    /// - `address` is where the value is stored.
    /// - All the pattern must have a span.
    ///
    /// Any violation of the assumptions will result in a panic.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn insert_irrefutable_named_binding_point(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        irreftuable: &Irrefutable,
        simplified_type: &Type<infer::Model>,
        address: Address<infer::Model>,
        address_span: Option<Span>,
        qualifier: Qualifier,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<(), TypeSystemOverflow> {
        Irrefutable::insert_named_binding_point(
            self,
            name_binding_point,
            irreftuable,
            address_span,
            Binding {
                kind: BindingKind::Value,
                r#type: simplified_type,
                qualifier,
                address,
            },
            must_copy,
            handler,
        )
    }

    /// Finds the [`Named`] pattern and adds it to the [`NameBindingPoint`].
    ///
    /// # Assumptions
    ///
    /// - The irrefutable pattern has been bound with the given
    ///   `simplified_type`.
    /// - `address` is where the value is stored.
    /// - All the pattern must have a span.
    ///
    /// Any violation of the assumptions will result in a panic.
    #[allow(clippy::too_many_arguments)]
    pub(super) fn insert_refutable_named_binding_point(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        refutable: &Refutable,
        simplified_type: &Type<infer::Model>,
        address: Address<infer::Model>,
        address_span: Option<Span>,
        qualifier: Qualifier,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<(), TypeSystemOverflow> {
        Refutable::insert_named_binding_point(
            self,
            name_binding_point,
            refutable,
            address_span,
            Binding {
                kind: BindingKind::Value,
                r#type: simplified_type,
                qualifier,
                address,
            },
            must_copy,
            handler,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn create_reference_bound_named_pattern(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        address_type: Type<infer::Model>,
        qualifier: Qualifier,
        address: Address<infer::Model>,
        address_span: Option<Span>,
        pattern_span: Span,
        name: String,
        mutable: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        // address_type <= alloca_type <= named_type
        let register_id = self.create_register_assignmnet(
            Assignment::ReferenceOf(ReferenceOf {
                address,
                qualifier,
                lifetime: Lifetime::Inference(Erased),
            }),
            address_span.unwrap_or_else(|| pattern_span.clone()),
        );

        let alloca_ty = Type::Reference(Reference {
            qualifier,
            lifetime: Lifetime::Inference(Erased),
            pointee: Box::new(address_type),
        });

        let alloca_id = self.create_alloca(alloca_ty, pattern_span.clone());

        let _ = self.current_block_mut().insert_instruction(
            instruction::Instruction::Store(Store {
                address: Address::Memory(Memory::Alloca(alloca_id)),
                value: Value::Register(register_id),
            }),
        );

        let _ = name_binding_point.insert(
            name,
            NameBinding {
                mutable,
                load_address: Address::Memory(Memory::Alloca(alloca_id)),
                span: pattern_span,
            },
            handler,
        );
    }

    const fn convert_from_start_index_to_end_index(
        start_index: usize,
        total_length: usize,
    ) -> usize {
        total_length - start_index - 1
    }

    #[allow(clippy::too_many_lines)]
    fn insert_named_binding_point_tuple<T: Pattern>(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        tuple_pat: &Tuple<T>,
        address_span: &Option<Span>,
        mut binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<(), TypeSystemOverflow> {
        binding = reduce_reference(binding);

        let Type::Tuple(tuple_ty) = binding.r#type else {
            panic!("unexpected type!");
        };

        assert!(tuple_pat.elements.iter().filter(|x| x.is_packed).count() <= 1);
        let packed_position =
            tuple_pat.elements.iter().position(|x| x.is_packed);

        if let Some(packed_position) = packed_position {
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

                    count => panic!("unexpected unpacked count: {count}"),
                }
            };

            // can't be reference bound
            if matches!(binding.kind, BindingKind::Reference) {
                handler.receive(Box::new(
                    FoundPackTuplePatternInReferenceBoundTupleType {
                        pattern_span: tuple_pat.span.clone(),
                    },
                ));
                return Ok(());
            }

            let start_range = 0..packed_position;
            let tuple_end_range = packed_position + 1..tuple_pat.elements.len();
            let type_end_range = (tuple_ty.elements.len()
                - tuple_end_range.len())
                ..tuple_ty.elements.len();
            let type_pack_range = packed_position..type_end_range.start;

            // match the start
            for (index, (tuple_ty, tuple_pat)) in tuple_ty.elements
                [start_range.clone()]
            .iter()
            .zip(&tuple_pat.elements[start_range])
            .enumerate()
            {
                assert!(!tuple_ty.is_unpacked);

                let element_address = Address::Tuple(address::Tuple {
                    tuple_address: Box::new(binding.address.clone()),
                    offset: address::Offset::FromStart(index),
                });

                T::insert_named_binding_point(
                    self,
                    name_binding_point,
                    &tuple_pat.pattern,
                    address_span.clone(),
                    Binding {
                        kind: binding.kind,
                        r#type: &tuple_ty.term,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    must_copy,
                    handler,
                )?;
            }

            // create a new alloca where all the elements will be stoered.
            let packed_type = Type::Tuple(term::Tuple {
                elements: tuple_ty.elements[type_pack_range.clone()].to_vec(),
            });
            let packed_alloca = self.create_alloca(
                packed_type.clone(),
                tuple_pat.elements.get(packed_position).unwrap().pattern.span(),
            );

            if let Some(unpacked_position_in_type) = unpacked_position_in_type {
                assert!(type_pack_range.contains(&unpacked_position_in_type));
                // pack from starting_pack to unpacked_position
                let before_unpacked_range =
                    type_pack_range.start..unpacked_position_in_type;

                for (offset, index) in before_unpacked_range.enumerate() {
                    let element_address =
                        Address::Tuple(crate::ir::address::Tuple {
                            tuple_address: Box::new(binding.address.clone()),
                            offset: address::Offset::FromStart(index),
                        });

                    let moved_reg = self.create_register_assignmnet(
                        Assignment::Load(Load { address: element_address }),
                        address_span.clone().unwrap_or_else(|| {
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                        }),
                    );

                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::Store(Store {
                            address: Address::Tuple(address::Tuple {
                                tuple_address: Box::new(Address::Memory(
                                    Memory::Alloca(packed_alloca),
                                )),
                                offset: address::Offset::FromStart(offset),
                            }),
                            value: Value::Register(moved_reg),
                        }),
                    );
                }

                // use dedicated instruction to pack the unpacked element
                let _ = self.current_block_mut().insert_instruction(
                    Instruction::TuplePack(TuplePack {
                        packed_tuple_span: Some(
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span(),
                        ),
                        store_address: Address::Memory(Memory::Alloca(
                            packed_alloca,
                        )),
                        tuple_address: binding.address.clone(),
                        before_packed_element_count: unpacked_position_in_type,
                        after_packed_element_count: tuple_ty.elements.len()
                            - unpacked_position_in_type
                            - 1,
                    }),
                );

                // pack from unpacked_position to end
                let after_unpacked_range =
                    unpacked_position_in_type + 1..type_pack_range.end;
                for (enumerate, index) in
                    after_unpacked_range.clone().enumerate()
                {
                    let element_address =
                        Address::Tuple(crate::ir::address::Tuple {
                            tuple_address: Box::new(binding.address.clone()),
                            offset: address::Offset::FromEnd(
                                Self::convert_from_start_index_to_end_index(
                                    index,
                                    tuple_ty.elements.len(),
                                ),
                            ),
                        });

                    let moved_reg = self.create_register_assignmnet(
                        Assignment::Load(Load { address: element_address }),
                        address_span.clone().unwrap_or_else(|| {
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                        }),
                    );

                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::Store(Store {
                            address: Address::Tuple(address::Tuple {
                                tuple_address: Box::new(Address::Memory(
                                    Memory::Alloca(packed_alloca),
                                )),
                                offset: address::Offset::FromEnd(
                                    after_unpacked_range.clone().count()
                                        - enumerate
                                        - 1,
                                ),
                            }),
                            value: Value::Register(moved_reg),
                        }),
                    );
                }
            } else {
                for (offset, index) in type_pack_range.enumerate() {
                    let element_address =
                        Address::Tuple(crate::ir::address::Tuple {
                            tuple_address: Box::new(binding.address.clone()),
                            offset: address::Offset::FromStart(index),
                        });

                    let moved_reg = self.create_register_assignmnet(
                        Assignment::Load(Load { address: element_address }),
                        address_span.clone().unwrap_or_else(|| {
                            tuple_pat
                                .elements
                                .get(packed_position)
                                .unwrap()
                                .pattern
                                .span()
                        }),
                    );

                    let _ = self.current_block_mut().insert_instruction(
                        Instruction::Store(Store {
                            address: Address::Tuple(address::Tuple {
                                tuple_address: Box::new(Address::Memory(
                                    Memory::Alloca(packed_alloca),
                                )),
                                offset: address::Offset::FromStart(offset),
                            }),
                            value: Value::Register(moved_reg),
                        }),
                    );
                }
            }

            T::insert_named_binding_point(
                self,
                name_binding_point,
                &tuple_pat.elements.get(packed_position).unwrap().pattern,
                address_span.clone(),
                Binding {
                    kind: binding.kind,
                    r#type: &packed_type,
                    address: Address::Memory(Memory::Alloca(packed_alloca)),
                    qualifier: Qualifier::Mutable,
                },
                false,
                handler,
            )?;

            // match the end
            for ((ty_elem, pat_elem), ty_index) in tuple_ty.elements
                [type_end_range.clone()]
            .iter()
            .zip(&tuple_pat.elements[tuple_end_range])
            .zip(type_end_range)
            {
                assert!(!ty_elem.is_unpacked);
                let element_address = Address::Tuple(address::Tuple {
                    tuple_address: Box::new(binding.address.clone()),
                    offset: if unpacked_position_in_type.is_some() {
                        address::Offset::FromEnd(
                            Self::convert_from_start_index_to_end_index(
                                ty_index,
                                tuple_ty.elements.len(),
                            ),
                        )
                    } else {
                        address::Offset::FromStart(ty_index)
                    },
                });

                T::insert_named_binding_point(
                    self,
                    name_binding_point,
                    &pat_elem.pattern,
                    address_span.clone(),
                    Binding {
                        kind: binding.kind,
                        r#type: &ty_elem.term,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    must_copy,
                    handler,
                )?;
            }
        } else {
            assert_eq!(
                tuple_ty.elements.iter().filter(|x| x.is_unpacked).count(),
                0
            );

            for (index, (tuple_ty, tuple_pat)) in tuple_ty
                .elements
                .iter()
                .map(|x| &x.term)
                .zip(tuple_pat.elements.iter())
                .enumerate()
            {
                let element_address = Address::Tuple(address::Tuple {
                    tuple_address: Box::new(binding.address.clone()),
                    offset: address::Offset::FromStart(index),
                });

                T::insert_named_binding_point(
                    self,
                    name_binding_point,
                    &tuple_pat.pattern,
                    address_span.clone(),
                    Binding {
                        kind: binding.kind,
                        r#type: tuple_ty,
                        address: element_address,
                        qualifier: binding.qualifier,
                    },
                    must_copy,
                    handler,
                )?;
            }
        }
        Ok(())
    }

    fn insert_named_binding_point_structural<T: Pattern>(
        &mut self,
        name_binding_point: &mut NameBindingPoint<infer::Model>,
        structural: &Structural<T>,
        address_span: &Option<Span>,
        mut binding: Binding,
        must_copy: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) -> Result<(), TypeSystemOverflow> {
        binding = reduce_reference(binding);

        // must be a struct type
        let Type::Symbol(Symbol {
            id: r#type::SymbolID::Adt(AdtID::Struct(struct_id)),
            generic_arguments,
        }) = binding.r#type
        else {
            panic!("unexpected type!");
        };

        let struct_id = *struct_id;
        let struct_symbol = self.table.get(struct_id).unwrap();

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id.into(),
            &struct_symbol.generic_declaration.parameters,
        )
        .unwrap();

        assert_eq!(
            struct_symbol.field_declaration_order().len(),
            structural.patterns_by_field_id.len()
        );

        for field_id in struct_symbol.field_declaration_order().iter().copied()
        {
            let mut binding_cloned = binding.clone();
            let mut field_ty = infer::Model::from_default_type(
                struct_symbol.fields().get(field_id).unwrap().r#type.clone(),
            );

            instantiation::instantiate(&mut field_ty, &instantiation);
            field_ty =
                simplify::simplify(&field_ty, &self.create_environment())
                    .map_err(|overflow_error| TypeSystemOverflow {
                        operation: OverflowOperation::TypeOf,
                        overflow_span: structural
                            .patterns_by_field_id
                            .get(&field_id)
                            .unwrap()
                            .span(),
                        overflow_error,
                    })?
                    .result;

            binding_cloned.r#type = &field_ty;
            binding_cloned.address = Address::Field(address::Field {
                struct_address: Box::new(binding_cloned.address),
                id: field_id,
            });

            T::insert_named_binding_point(
                self,
                name_binding_point,
                structural.patterns_by_field_id.get(&field_id).unwrap(),
                address_span.clone(),
                binding_cloned,
                must_copy,
                handler,
            )?;
        }

        Ok(())
    }
}

impl<
        't,
        S: table::State,
        RO: resolution::Observer<S, infer::Model>,
        TO: type_system::observer::Observer<infer::Model, S>
            + type_system::observer::Observer<ir::Model, S>,
    > Binder<'t, S, RO, TO>
{
    fn replace_refutable_in_tuple_pack_internal(
        reftuable: &mut Refutable,
        in_tuple_pack: bool,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        match reftuable {
            pattern @ (Refutable::Boolean(_)
            | Refutable::Integer(_)
            | Refutable::Enum(_)) => {
                if in_tuple_pack {
                    let span = pattern.span();
                    *pattern =
                        Refutable::Wildcard(crate::ir::pattern::Wildcard {
                            span: span.clone(),
                        });

                    handler.receive(Box::new(
                        FoundPackTuplePatternInReferenceBoundTupleType {
                            pattern_span: span,
                        },
                    ));
                }
            }

            Refutable::Wildcard(_) | Refutable::Named(_) => {}

            Refutable::Tuple(tuple) => {
                for pat in &mut tuple.elements {
                    Self::replace_refutable_in_tuple_pack_internal(
                        &mut pat.pattern,
                        pat.is_packed || in_tuple_pack,
                        handler,
                    );
                }
            }
            Refutable::Structural(structural) => {
                for field in structural.patterns_by_field_id.values_mut() {
                    Self::replace_refutable_in_tuple_pack_internal(
                        field,
                        in_tuple_pack,
                        handler,
                    );
                }
            }
        }
    }

    pub(super) fn replace_refutable_in_tuple_pack(
        reftuable: &mut Refutable,
        handler: &dyn Handler<Box<dyn Error>>,
    ) {
        Self::replace_refutable_in_tuple_pack_internal(
            reftuable, false, handler,
        );
    }

    pub(super) fn get_refutable_paths_internal(
        reftuable: &Refutable,
        prev: Path,
        paths: &mut VecDeque<Path>,
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
                        Path::Variant(VariantPath {
                            enum_path: Box::new(prev),
                        }),
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
                        Path::TupleElement(TupleElementPath {
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
                        Path::Field(FieldPath {
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
    pub(super) fn get_refutable_paths(reftuable: &Refutable) -> VecDeque<Path> {
        let mut paths = VecDeque::new();
        Self::get_refutable_paths_internal(reftuable, Path::Base, &mut paths);
        paths
    }

    fn reduce_reference_no_binding(
        mut ty: Type<infer::Model>,
        mut address: Address<infer::Model>,
    ) -> (Type<infer::Model>, Address<infer::Model>) {
        loop {
            match ty {
                Type::Reference(reference) => {
                    // update the address, reference binding
                    // info, and binding ty
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

    #[allow(clippy::too_many_lines)]
    pub(super) fn get_address_and_type_from_path<'r>(
        &mut self,
        refutable_pattern: &'r Refutable,
        path: &Path,
        address: Address<infer::Model>,
        ty: Type<infer::Model>,
    ) -> (Address<infer::Model>, Type<infer::Model>, &'r Refutable) {
        match path {
            Path::Base => {
                let (ty, address) =
                    Self::reduce_reference_no_binding(ty, address);
                (address, ty, refutable_pattern)
            }

            Path::Variant(variant) => {
                let (address, ty, reftuable_pattern) = self
                    .get_address_and_type_from_path(
                        refutable_pattern,
                        &variant.enum_path,
                        address,
                        ty,
                    );

                let Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(AdtID::Enum(enum_id)),
                    generic_arguments,
                }) = ty
                else {
                    panic!("unexpected type!");
                };

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    enum_id.into(),
                    &self
                        .table
                        .get(enum_id)
                        .unwrap()
                        .generic_declaration
                        .parameters,
                )
                .unwrap();

                let mut variant_ty = infer::Model::from_default_type(
                    self.table
                        .get(reftuable_pattern.as_enum().unwrap().variant_id)
                        .unwrap()
                        .associated_type
                        .clone()
                        .unwrap(),
                );

                instantiation::instantiate(&mut variant_ty, &instantiation);

                let (ty, address) = Self::reduce_reference_no_binding(
                    variant_ty,
                    Address::Variant(address::Variant {
                        enum_address: Box::new(address),
                        id: reftuable_pattern.as_enum().unwrap().variant_id,
                    }),
                );

                (
                    address,
                    ty,
                    reftuable_pattern
                        .as_enum()
                        .unwrap()
                        .pattern
                        .as_ref()
                        .unwrap(),
                )
            }

            Path::TupleElement(tuple) => {
                let (address, ty, reftuable_pattern) = self
                    .get_address_and_type_from_path(
                        refutable_pattern,
                        &tuple.tuple_path,
                        address,
                        ty,
                    );

                let element_pat = reftuable_pattern
                    .as_tuple()
                    .unwrap()
                    .elements
                    .get(tuple.index)
                    .unwrap();

                let (ty, address) = Self::reduce_reference_no_binding(
                    ty.into_tuple().unwrap().elements.remove(tuple.index).term,
                    Address::Tuple(address::Tuple {
                        tuple_address: Box::new(address),
                        offset: address::Offset::FromStart(tuple.index),
                    }),
                );

                (address, ty, &element_pat.pattern)
            }

            Path::Field(path) => {
                let (address, ty, reftuable_pattern) = self
                    .get_address_and_type_from_path(
                        refutable_pattern,
                        &path.struct_path,
                        address,
                        ty,
                    );

                // must be a struct type
                let Type::Symbol(Symbol {
                    id: r#type::SymbolID::Adt(AdtID::Struct(struct_id)),
                    generic_arguments,
                }) = ty
                else {
                    panic!("unexpected type!");
                };

                let field_pat = reftuable_pattern
                    .as_structural()
                    .unwrap()
                    .patterns_by_field_id
                    .get(&path.field_id)
                    .unwrap();

                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments,
                    struct_id.into(),
                    &self
                        .table
                        .get(struct_id)
                        .unwrap()
                        .generic_declaration
                        .parameters,
                )
                .unwrap();

                let mut field_ty = infer::Model::from_default_type(
                    self.table.get(struct_id).unwrap().fields()[path.field_id]
                        .r#type
                        .clone(),
                );

                instantiation::instantiate(&mut field_ty, &instantiation);

                let (ty, address) = Self::reduce_reference_no_binding(
                    field_ty,
                    Address::Field(address::Field {
                        struct_address: Box::new(address),
                        id: path.field_id,
                    }),
                );

                (address, ty, field_pat)
            }
        }
    }
}

impl Refutable {
    pub(super) fn get_from_path(&self, path: &Path) -> Option<&Self> {
        match path {
            Path::Base => Some(self),

            Path::Field(field_path) => {
                let structural_pat = self
                    .get_from_path(&field_path.struct_path)?
                    .as_structural()?;

                structural_pat.patterns_by_field_id.get(&field_path.field_id)
            }

            Path::TupleElement(tuple_element_path) => {
                let tuple_pat = self
                    .get_from_path(&tuple_element_path.tuple_path)?
                    .as_tuple()?;

                tuple_pat
                    .elements
                    .get(tuple_element_path.index)
                    .map(|x| &x.pattern)
            }

            Path::Variant(variant_path) => {
                let enum_pat =
                    self.get_from_path(&variant_path.enum_path)?.as_enum()?;

                enum_pat.pattern.as_deref()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct FieldPath {
    pub(super) field_id: ID<Field>,
    pub(super) struct_path: Box<Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct TupleElementPath {
    pub(super) index: usize,
    pub(super) tuple_path: Box<Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct VariantPath {
    pub(super) enum_path: Box<Path>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) enum Path {
    Base,
    Field(FieldPath),
    TupleElement(TupleElementPath),
    Variant(VariantPath),
}

#[cfg(test)]
mod test;
