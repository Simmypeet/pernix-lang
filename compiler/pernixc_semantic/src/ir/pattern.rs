//! Contains the definition of patterns

use std::collections::{BTreeSet, HashMap};

use enum_as_inner::EnumAsInner;
use pernixc_base::{
    handler::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{self, ConnectedList};

use super::address::Address;
use crate::{
    arena::ID,
    error::{
        self, AlreadyBoundFieldPattern, AlreadyBoundName,
        ExpectAssociatedPattern, ExpectTuplePackPattern, FieldIsNotAccessible,
        FieldNotFound, MismatchedPatternBindingType,
        MismatchedTuplePatternLength, MoreThanOnePackedTuplePattern,
        PatternBindingType, SymbolIsNotAccessible, SymbolNotFound,
        TooLargetNumericLiteral, UnboundFields, UnexpectedAssociatedPattern,
    },
    symbol::{
        self,
        table::{self, representation::Index},
        Field, GlobalID, Struct, Variant,
    },
    type_system::{
        environment::Environment,
        instantiation::{
            self, Instantiation, MismatchedGenericArgumentCountError,
        },
        model::Model,
        normalizer::Normalizer,
        observer::Observer,
        simplify,
        term::{
            self,
            r#type::{self, Qualifier, Reference, SymbolID, Type},
            Symbol,
        },
        LifetimeConstraint, Succeeded,
    },
};

/// An error that occurs when creating a pattern.
#[derive(
    Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum Error<M: Model> {
    #[error("found an invalid GlobalID in the type term.")]
    InvalidGlobalID(GlobalID),

    #[error(
        "an error occurred when trying to instantiate the type term. This is \
         mostly likely come from instantiating the struct or enum type."
    )]
    MismatchedGenericArgumentCount(
        #[from] MismatchedGenericArgumentCountError<M>,
    ),

    #[error("found a tuple type with more than one unpacked element.")]
    MoreThanOneUnpackedInTupleType(r#type::Tuple<M>),

    #[error("a fatal semantic error occurred which cannot be recovered.")]
    Semantic,
}

/// A trait that is implemented by [`Refutable`] and [`Irrefutable`].
pub trait Pattern:
    From<Named> + From<Tuple<Self>> + From<Wildcard> + From<Structural<Self>>
{
    /// The syntax tree equivalent of the pattern.
    type SyntaxTree: SourceElement;

    /// Binds the syntax tree to the pattern.
    ///
    /// # Parameters
    ///
    /// - `syntax_tree`: The syntax tree to bind.
    /// - `ty`: The type that is bound to the pattern. The type **must be
    ///   simplified** before being passed to this function.
    /// - `referring_site`: The site where the pattern is being bound.
    /// - `environment`: The environment to get the required information from.
    /// - `handler`: The handler to report the errors to.
    fn bind<M: Model, S: table::State>(
        syntax_tree: &Self::SyntaxTree,
        ty: &Type<M>,
        referring_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Succeeded<Self, M>, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
        Self: Sized;

    /// The span of the pattern.
    fn span(&self) -> Option<&Span>;
}

/// An integer literal pattern
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Integer {
    /// The value of the ingteger literal.
    pub value: i128,

    /// The span of the integer literal.
    pub span: Option<Span>,
}

impl Integer {
    fn bind<M: Model>(
        syntax_tree: &syntax_tree::pattern::Integer,
        mut ty: &Type<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
    {
        ty = reduce_reference(ty);

        if matches!(
            ty,
            Type::Primitive(
                r#type::Primitive::Int8
                    | r#type::Primitive::Int16
                    | r#type::Primitive::Int32
                    | r#type::Primitive::Int64
                    | r#type::Primitive::Uint8
                    | r#type::Primitive::Uint16
                    | r#type::Primitive::Uint32
                    | r#type::Primitive::Uint64
                    | r#type::Primitive::Usize
                    | r#type::Primitive::Isize
            )
        ) {
            let mut value =
                match syntax_tree.numeric().span.str().parse::<i128>() {
                    Ok(value) => value,
                    Err(err) => match err.kind() {
                        std::num::IntErrorKind::NegOverflow
                        | std::num::IntErrorKind::PosOverflow => {
                            handler.receive(Box::new(
                                TooLargetNumericLiteral {
                                    span: syntax_tree.numeric().span.clone(),
                                },
                            ));
                            return Err(Error::Semantic);
                        }

                        _ => unreachable!(),
                    },
                };

            if syntax_tree.minus().is_some() {
                value = -value;
            }

            Ok(Integer { value, span: Some(syntax_tree.span()) })
        } else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Integer,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));

            Err(Error::Semantic)
        }
    }
}

/// A boolean literal pattern
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Boolean {
    /// The value of the boolean literal.
    pub value: bool,

    /// The span of the boolean literal.
    pub span: Option<Span>,
}

impl Boolean {
    fn bind<M: Model>(
        syntax_tree: &syntax_tree::expression::Boolean,
        mut ty: &Type<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Self, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
    {
        ty = reduce_reference(ty);

        if matches!(ty, Type::Primitive(r#type::Primitive::Bool)) {
            Ok(Boolean {
                value: syntax_tree.is_true(),
                span: Some(syntax_tree.span()),
            })
        } else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Boolean,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));

            Err(Error::Semantic)
        }
    }
}

/// The kind of binding for the [`Named`] binding.
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, EnumAsInner,
)]
pub enum BindingKind {
    /// The value is bound by value.
    ///
    /// The boolean indicates whether the value is mutable or not.
    Value(bool),

    /// The value is bound by reference.
    Reference(Qualifier),
}

/// A pattern where the value is bound to a name
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Named {
    /// The name of the pattern.
    pub name: String,

    /// The binding kind of the name.
    pub kind: BindingKind,

    /// The span to the identifier of the name binding.
    pub span: Option<Span>,
}

impl Named {
    fn bind(syntax_tree: &syntax_tree::pattern::Named) -> Self {
        Named {
            name: syntax_tree.identifier().span.str().to_owned(),
            kind: match syntax_tree.binding() {
                syntax_tree::pattern::Binding::Ref(r) => {
                    BindingKind::Reference(match r.qualifier() {
                        Some(x) => match x {
                            syntax_tree::Qualifier::Mutable(_) => {
                                Qualifier::Mutable
                            }
                            syntax_tree::Qualifier::Unique(_) => {
                                Qualifier::Unique
                            }
                        },
                        None => Qualifier::Immutable,
                    })
                }
                syntax_tree::pattern::Binding::Value { mutable_keyword } => {
                    BindingKind::Value(mutable_keyword.is_some())
                }
            },
            span: Some(syntax_tree.identifier().span.clone()),
        }
    }
}

/// A refutable pattern specifying a variant of an enum.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<T: Pattern> {
    /// The ID of the variant that the pattern matches.
    pub variant_id: ID<Variant>,

    /// The pattern binding for the variant.
    pub pattern: Option<Box<T>>,
}

impl<T: Pattern> Enum<T> {
    fn bind<M: Model, S: table::State>(
        syntax_tree: &syntax_tree::pattern::Enum<T::SyntaxTree>,
        mut ty: &Type<M>,
        referring_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Succeeded<Self, M>, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
    {
        ty = reduce_reference(ty);

        // must be an enum type
        let Type::Symbol(Symbol {
            id: SymbolID::Enum(enum_id),
            generic_arguments,
        }) = ty
        else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Enum,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));
            return Err(Error::Semantic);
        };

        let enum_id = *enum_id;

        let enum_symbol = environment
            .table()
            .get(enum_id)
            .ok_or(Error::InvalidGlobalID(enum_id.into()))?;

        // variant not found
        let Some(variant_id): Option<ID<symbol::Variant>> = enum_symbol
            .variant_ids_by_name()
            .get(syntax_tree.identifier().span.str())
            .copied()
        else {
            handler.receive(Box::new(SymbolNotFound {
                searched_global_id: Some(enum_id.into()),
                resolution_span: syntax_tree.identifier().span.clone(),
            }));

            return Err(Error::Semantic);
        };

        // check if the variant is accessible
        if !environment
            .table()
            .is_accessible_from(
                referring_site,
                environment
                    .table()
                    .get_accessibility(variant_id.into())
                    .ok_or(Error::InvalidGlobalID(variant_id.into()))?,
            )
            .unwrap()
        {
            // soft error, report and continue
            handler.receive(Box::new(SymbolIsNotAccessible {
                referring_site,
                referred: variant_id.into(),
                referred_span: syntax_tree.identifier().span.clone(),
            }));
        }

        let variant_sym = environment
            .table()
            .get(variant_id)
            .ok_or(Error::InvalidGlobalID(variant_id.into()))?;

        match (&variant_sym.associated_type, syntax_tree.association()) {
            (Some(ty), Some(pat)) => {
                let instantiation = Instantiation::from_generic_arguments(
                    generic_arguments.clone(),
                    enum_id.into(),
                    &enum_symbol.generic_declaration.parameters,
                )?;

                let mut variant_ty = M::from_default_type(ty.clone());

                instantiation::instantiate(&mut variant_ty, &instantiation);
                let simplification =
                    simplify::simplify(&variant_ty, environment);

                let mut constraints = BTreeSet::new();

                let pattern = handle_binding_result(
                    T::bind(
                        pat.pattern(),
                        &simplification.result,
                        referring_site,
                        environment,
                        handler,
                    ),
                    &mut constraints,
                )?;

                Ok(Succeeded::with_constraints(
                    Enum { variant_id, pattern: Some(Box::new(pattern)) },
                    constraints,
                ))
            }
            (None, None) => {
                Ok(Succeeded::new(Enum { variant_id, pattern: None }))
            }
            (Some(_), None) => {
                handler.receive(Box::new(ExpectAssociatedPattern {
                    variant_id,
                    pattern_span: syntax_tree.span(),
                }));
                Err(Error::Semantic)
            }
            (None, Some(_)) => {
                handler.receive(Box::new(UnexpectedAssociatedPattern {
                    associated_pattern_span: syntax_tree.span(),
                    variant_id,
                }));
                Err(Error::Semantic)
            }
        }
    }
}

/// A pattern bound to an element in a tuple.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TupleElement<T: Pattern> {
    /// The pattern binding for the element.
    pub pattern: T,

    /// Whether the element is unpacked or not.
    pub is_packed: bool,
}

impl<T: Pattern> TupleElement<T> {
    /// Creates a new **packed** tuple element.
    #[must_use]
    pub fn new_packed(pattern: T) -> Self {
        TupleElement { pattern, is_packed: true }
    }

    /// Creates a new **non-packed** tuple element.
    #[must_use]
    pub fn new_non_packed(pattern: T) -> Self {
        TupleElement { pattern, is_packed: false }
    }
}

/// A pattern bound to a tuple type.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[allow(missing_docs)]
pub struct Tuple<T: Pattern> {
    /// The pattern binding for each element in the tuple.
    pub elements: Vec<TupleElement<T>>,

    /// The span of the whole tuple pattern.
    pub span: Option<Span>,
}

fn handle_binding_result<T: Pattern, M: Model>(
    result: Result<Succeeded<T, M>, Error<M>>,
    constraints: &mut BTreeSet<LifetimeConstraint<M>>,
) -> Result<T, Error<M>> {
    match result {
        Ok(v) => {
            constraints.extend(v.constraints);
            Ok(v.result)
        }
        Err(Error::Semantic) => Ok(Wildcard { span: None }.into()),
        Err(err) => return Err(err),
    }
}

impl<T: Pattern> Tuple<T> {
    fn bind<M: Model, S: table::State>(
        syntax_tree: &syntax_tree::pattern::Tuple<T::SyntaxTree>,
        mut ty: &Type<M>,
        referring_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Succeeded<Self, M>, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
    {
        ty = reduce_reference(ty);

        let Type::Tuple(tuple_ty) = ty else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Tuple,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));
            return Err(Error::Semantic);
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
                    return Err(Error::MoreThanOneUnpackedInTupleType(
                        tuple_ty.clone(),
                    ))
                }
            }
        };

        // normal tuple pattern, the number of pattern must exactly
        // match
        let tuple_element_patterns = syntax_tree
            .patterns()
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

                    return Err(Error::Semantic);
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
                return Err(Error::Semantic);
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
                    return Err(Error::Semantic);
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
                    return Err(Error::Semantic);
                }
            }

            // match start
            let mut constraints = BTreeSet::new();
            let mut elements = Vec::new();

            for (tuple_ty, tuple_pat) in tuple_ty.elements[start_range.clone()]
                .iter()
                .zip(&tuple_element_patterns[start_range.clone()])
            {
                assert!(!tuple_ty.is_unpacked);

                elements.push(TupleElement::new_non_packed(
                    handle_binding_result(
                        T::bind(
                            tuple_pat.pattern(),
                            &tuple_ty.term,
                            referring_site,
                            environment,
                            handler,
                        ),
                        &mut constraints,
                    )?,
                ));
            }

            let packed_type = Type::Tuple(term::Tuple {
                elements: tuple_ty.elements[type_pack_range.clone()]
                    .iter()
                    .cloned()
                    .collect(),
            });
            elements.push(TupleElement::new_packed(handle_binding_result(
                T::bind(
                    tuple_element_patterns[packed_position_in_pattern]
                        .pattern(),
                    &packed_type,
                    referring_site,
                    environment,
                    handler,
                ),
                &mut constraints,
            )?));

            for (ty_elem, pat_elem) in tuple_ty.elements[type_end_range.clone()]
                .iter()
                .zip(&tuple_element_patterns[tuple_end_range])
            {
                assert!(!ty_elem.is_unpacked);

                elements.push(TupleElement::new_non_packed(
                    handle_binding_result(
                        T::bind(
                            pat_elem.pattern(),
                            &ty_elem.term,
                            referring_site,
                            environment,
                            handler,
                        ),
                        &mut constraints,
                    )?,
                ));
            }

            Ok(Succeeded::with_constraints(
                Tuple { elements, span: Some(syntax_tree.span()) },
                constraints,
            ))
        } else {
            // count must exactly match
            if tuple_element_patterns.len() != tuple_ty.elements.len() {
                handler.receive(Box::new(MismatchedTuplePatternLength {
                    pattern_span: syntax_tree.span(),
                    pattern_element_count: tuple_element_patterns.len(),
                    type_element_count: tuple_ty.elements.len(),
                }));
                return Err(Error::Semantic);
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

            let mut constraints = BTreeSet::new();
            let mut elements = Vec::new();

            for (tuple_ty, tuple_pat) in tuple_ty
                .elements
                .iter()
                .map(|x| &x.term)
                .zip(tuple_element_patterns.iter().copied())
            {
                elements.push(TupleElement::new_non_packed(
                    handle_binding_result(
                        T::bind(
                            &tuple_pat.pattern(),
                            tuple_ty,
                            referring_site,
                            environment,
                            handler,
                        ),
                        &mut constraints,
                    )?,
                ));
            }

            Ok(Succeeded::with_constraints(
                Tuple { elements, span: Some(syntax_tree.span()) },
                constraints,
            ))
        }
    }
}

/// A pattern that matches on a struct with fields.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Structural<T: Pattern> {
    /// The ID of the struct that the pattern matches.
    pub struct_id: ID<Struct>,

    /// Mapping from each field to the pattern that the field must match.
    pub patterns_by_field_id: HashMap<ID<Field>, T>,

    /// The span of the whole structural pattern.
    pub span: Option<Span>,
}

fn reduce_reference<M: Model>(mut ty: &Type<M>) -> &Type<M> {
    while let Type::Reference(Reference { pointee, .. }) = ty {
        ty = pointee.as_ref();
    }

    ty
}

impl<T: Pattern> Structural<T> {
    fn bind<M: Model, S: table::State>(
        syntax_tree: &syntax_tree::pattern::Structural<T::SyntaxTree>,
        mut ty: &Type<M>,
        referring_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Succeeded<Self, M>, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
    {
        ty = reduce_reference(ty);
        let mut constraints = BTreeSet::new();

        // must be a struct type
        let Type::Symbol(Symbol {
            id: SymbolID::Struct(struct_id),
            generic_arguments,
        }) = ty
        else {
            handler.receive(Box::new(MismatchedPatternBindingType {
                expected_bindnig_type: PatternBindingType::Struct,
                found_type: ty.clone(),
                pattern_span: syntax_tree.span(),
            }));
            return Err(Error::Semantic);
        };

        let struct_id = *struct_id;

        let struct_symbol = environment
            .table()
            .get(struct_id)
            .ok_or(Error::InvalidGlobalID(struct_id.into()))?;

        let instantiation = Instantiation::from_generic_arguments(
            generic_arguments.clone(),
            struct_id.into(),
            &struct_symbol.generic_declaration.parameters,
        )?;

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
            let mut field_ty = M::from_default_type(field_sym.r#type.clone());

            instantiation::instantiate(&mut field_ty, &instantiation);
            let simplification = simplify::simplify(&field_ty, environment);
            field_ty = simplification.result;
            constraints.extend(simplification.constraints);

            // the pattern for the field
            let pattern = match field {
                syntax_tree::pattern::Field::Association(assoc) => {
                    handle_binding_result(
                        T::bind(
                            assoc.pattern(),
                            &field_ty,
                            referring_site,
                            environment,
                            handler,
                        ),
                        &mut constraints,
                    )?
                }
                syntax_tree::pattern::Field::Named(named) => {
                    Named::bind(named).into()
                }
            };

            if !environment
                .table()
                .is_accessible_from(referring_site, field_sym.accessibility)
                .unwrap()
            {
                // soft error, no need to stop the process
                handler.receive(Box::new(FieldIsNotAccessible {
                    field_id,
                    struct_id,
                    referring_site,
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
            unbound_fields.iter().copied().map(|x| {
                (x, Wildcard { span: Some(syntax_tree.span()) }.into())
            }),
        );

        // report the unbound fields
        if !unbound_fields.is_empty() && syntax_tree.wildcard().is_none() {
            handler.receive(Box::new(UnboundFields {
                field_ids: unbound_fields,
                struct_id,
                pattern_span: syntax_tree.span(),
            }));
        }

        Ok(Succeeded::with_constraints(
            Structural {
                struct_id,
                patterns_by_field_id,
                span: Some(syntax_tree.span()),
            },
            constraints,
        ))
    }
}

/// A pattern that discards the value
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Wildcard {
    /// The span of the wildcard.
    pub span: Option<Span>,
}

impl Wildcard {
    fn bind(syntax_tree: &syntax_tree::pattern::Wildcard) -> Self {
        Wildcard { span: Some(syntax_tree.span()) }
    }
}

/// A pattern that cannot be refuted (always matches)
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, derive_more::From)]
#[allow(missing_docs)]
pub enum Irrefutable {
    Named(Named),
    Tuple(Tuple<Self>),
    Structural(Structural<Self>),
    Wildcard(Wildcard),
}

impl Pattern for Irrefutable {
    type SyntaxTree = syntax_tree::pattern::Irrefutable;

    fn bind<M: Model, S: table::State>(
        syntax_tree: &Self::SyntaxTree,
        ty: &Type<M>,
        referring_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Succeeded<Self, M>, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
    {
        match syntax_tree {
            syntax_tree::pattern::Irrefutable::Structural(s) => {
                Structural::bind(s, ty, referring_site, environment, handler)
                    .map(|x| x.map(Irrefutable::Structural))
            }
            syntax_tree::pattern::Irrefutable::Named(s) => {
                Ok(Succeeded::new(Irrefutable::Named(Named::bind(s))))
            }
            syntax_tree::pattern::Irrefutable::Tuple(s) => {
                Tuple::bind(s, ty, referring_site, environment, handler)
                    .map(|x| x.map(Irrefutable::Tuple))
            }
            syntax_tree::pattern::Irrefutable::Wildcard(s) => {
                Ok(Succeeded::new(Irrefutable::Wildcard(Wildcard::bind(s))))
            }
        }
    }

    fn span(&self) -> Option<&Span> {
        match self {
            Irrefutable::Named(named) => named.span.as_ref(),
            Irrefutable::Tuple(tuple) => tuple.span.as_ref(),
            Irrefutable::Structural(structural) => structural.span.as_ref(),
            Irrefutable::Wildcard(wildcard) => wildcard.span.as_ref(),
        }
    }
}

/// A pattern that can be refuted (may not always match)
#[allow(missing_docs)]
#[derive(Debug, Clone, PartialEq, Eq, EnumAsInner, derive_more::From)]
pub enum Refutable {
    Boolean(Boolean),
    Integer(Integer),
    Named(Named),
    Enum(Enum<Self>),
    Tuple(Tuple<Self>),
    Structural(Structural<Self>),
    Wildcard(Wildcard),
}

impl Pattern for Refutable {
    type SyntaxTree = syntax_tree::pattern::Refutable;

    fn bind<M: Model, S: table::State>(
        syntax_tree: &Self::SyntaxTree,
        ty: &Type<M>,
        referring_site: GlobalID,
        environment: &Environment<
            M,
            S,
            impl Normalizer<M, S>,
            impl Observer<M, S>,
        >,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<Succeeded<Self, M>, Error<M>>
    where
        Type<M>: table::Display<table::Suboptimal>,
    {
        match syntax_tree {
            syntax_tree::pattern::Refutable::Boolean(b) => Ok(Succeeded::new(
                Refutable::Boolean(Boolean::bind(b, ty, handler)?),
            )),
            syntax_tree::pattern::Refutable::Integer(n) => Ok(Succeeded::new(
                Refutable::Integer(Integer::bind(n, ty, handler)?),
            )),
            syntax_tree::pattern::Refutable::Named(n) => {
                Ok(Succeeded::new(Refutable::Named(Named::bind(n))))
            }
            syntax_tree::pattern::Refutable::Enum(e) => {
                Enum::bind(e, ty, referring_site, environment, handler)
                    .map(|x| x.map(Refutable::Enum))
            }
            syntax_tree::pattern::Refutable::Tuple(t) => {
                Tuple::bind(t, ty, referring_site, environment, handler)
                    .map(|x| x.map(Refutable::Tuple))
            }
            syntax_tree::pattern::Refutable::Structural(s) => {
                Structural::bind(s, ty, referring_site, environment, handler)
                    .map(|x| x.map(Refutable::Structural))
            }
            syntax_tree::pattern::Refutable::Wildcard(w) => {
                Ok(Succeeded::new(Refutable::Wildcard(Wildcard::bind(w))))
            }
        }
    }

    fn span(&self) -> Option<&Span> {
        match self {
            Refutable::Boolean(boolean) => boolean.span.as_ref(),
            Refutable::Integer(numeric) => numeric.span.as_ref(),
            Refutable::Named(named) => named.span.as_ref(),
            Refutable::Enum(r#enum) => {
                r#enum.pattern.as_ref().and_then(|x| x.span())
            }
            Refutable::Tuple(tuple) => tuple.span.as_ref(),
            Refutable::Structural(structural) => structural.span.as_ref(),
            Refutable::Wildcard(wildcard) => wildcard.span.as_ref(),
        }
    }
}

/// Represents an lvalue binding in the pattern.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NameBinding<M: Model> {
    /// Whether the binding is mutable or not.
    pub mutable: bool,

    /// The address where the value is stored.
    pub load_address: Address<M>,

    /// The span of the identifier of the name binding.
    pub span: Option<Span>,
}

/// Contains all the named bindings in the patterns.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct NameBindingPoint<M: Model> {
    /// Mapping from the name of the binding to the named pattern.
    pub named_patterns_by_name: HashMap<String, NameBinding<M>>,
}

impl<M: Model> NameBindingPoint<M> {
    /// Inserts a name binding into the point.
    ///
    /// # Errors
    ///
    /// Returns [`Err`] with the passed name binding if the name is already
    /// bound.
    pub fn insert(
        &mut self,
        name: String,
        name_binding: NameBinding<M>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) -> Result<(), NameBinding<M>> {
        match self.named_patterns_by_name.entry(name) {
            std::collections::hash_map::Entry::Occupied(entry) => {
                if let (Some(existing_sapn), Some(new_span)) =
                    (entry.get().span.as_ref(), name_binding.span.as_ref())
                {
                    handler.receive(Box::new(AlreadyBoundName {
                        already_bound_identifier_span: existing_sapn.clone(),
                        new_binding_span: new_span.clone(),
                    }));
                }

                Err(name_binding)
            }
            std::collections::hash_map::Entry::Vacant(entry) => {
                entry.insert(name_binding);
                Ok(())
            }
        }
    }
}
