use std::collections::{hash_map::Entry, HashMap};

use paste::paste;
use pernixc_base::{
    diagnostic::Handler,
    source_file::{SourceElement, Span},
};
use pernixc_syntax::syntax_tree::{
    self,
    item::{LifetimeBoundOperand, TraitMemberBoundArgument},
};

use super::{
    resolution::{self, Resolution, WithGenerics, WithParent},
    state::{self, Config, Flag, Symbol},
    Index, IndexMut, Table,
};
use crate::{
    arena::ID,
    error::{
        self, GenericParameterDuplication, InvalidTypeInConstantTypePredicate,
        InvalidTypeInOutlivesPredicate, MisorderedGenericParameter,
        PrivateEntityLeakedToPublicInterface,
        TraitMemberBoundArgumentMismatched, TraitMemberExpected,
    },
    semantic::{
        model::{Entity, Forall, Model},
        predicate::{
            self, ConstantType, Equals, LifetimeOutlives, Predicate,
            TypeOutlives,
        },
        substitution::Substitution,
        term::{
            self, constant::Constant, lifetime::Lifetime, r#type::Type,
            GenericArguments,
        },
    },
    symbol::{
        self, semantic::Symbolic, ConstantParameter, ConstantParameterID, Enum,
        Function, Generic, GenericID, GenericKind, GlobalID,
        ImplementationFunction, ImplementationType, LifetimeParameter,
        LifetimeParameterID, Struct, Trait, TraitFunction, TraitType,
        TypeParameter, TypeParameterID,
    },
};

mod constant;
mod r#enum;
mod function;
mod implementation;
mod implementation_constant;
mod implementation_function;
mod implementation_type;
mod negative_implementation;
mod r#struct;
mod r#trait;
mod trait_constant;
mod trait_function;
mod trait_type;
mod r#type;
mod variant;

pub(super) trait Build: Symbol
where
    ID<Self>: Into<GlobalID>,
{
    fn build(
        config: Config<Self>,
        data: &mut Self::Data,
        build_flag: Self::Flag,
    );
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct ResolutionInfo<S: Model> {
    resolution: Resolution<S>,
    generic_identifier_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct ConstantResolutionInfo<S: Model> {
    constant: Constant<S>,
    expected_type: Type<S>,
    constant_expression_span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub(super) struct TypeResolutionInfo<S: Model> {
    r#type: Type<S>,
    type_span: Span,
}

/// Used to store all the symbol resolution occured during building the symbol.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(super) struct Storage<T: Symbol, S: Model>
where
    ID<T>: Into<GlobalID>,
{
    resolution: HashMap<T::Flag, Vec<ResolutionInfo<S>>>,
    constants: HashMap<T::Flag, Vec<ConstantResolutionInfo<S>>>,
    types: HashMap<T::Flag, Vec<TypeResolutionInfo<S>>>,
    current_flag: T::Flag,
}

impl<T: Symbol, S: Model> Storage<T, S>
where
    ID<T>: Into<GlobalID>,
{
    fn set_curret_flag(&mut self, flag: T::Flag) { self.current_flag = flag; }
}

impl<T: Symbol, S: Model> Default for Storage<T, S>
where
    ID<T>: Into<GlobalID>,
{
    fn default() -> Self {
        Self {
            resolution: HashMap::default(),
            constants: HashMap::default(),
            types: HashMap::default(),
            current_flag: <T::Flag as Flag>::drafting_state(),
        }
    }
}

pub(super) struct ResolutionConfigChain<
    'f,
    's,
    S: Model,
    First: resolution::Config<S> + ?Sized,
    Second: resolution::Config<S> + ?Sized,
> {
    first: &'f mut First,
    second: &'s mut Second,
    _phantom: std::marker::PhantomData<S>,
}

impl<
        'f,
        's,
        S: Model,
        First: resolution::Config<S> + ?Sized,
        Second: resolution::Config<S> + ?Sized,
    > ResolutionConfigChain<'f, 's, S, First, Second>
{
    pub(super) fn new(first: &'f mut First, second: &'s mut Second) -> Self {
        Self { first, second, _phantom: std::marker::PhantomData }
    }
}

impl<T: Symbol, S: Model> resolution::Config<S> for Storage<T, S>
where
    ID<T>: Into<GlobalID>,
{
    fn lifetime_arguments_placeholder(&mut self) -> Option<Lifetime<S>> { None }

    fn type_arguments_placeholder(&mut self) -> Option<Type<S>> { None }

    fn constant_arguments_placeholder(&mut self) -> Option<Constant<S>> { None }

    fn on_global_id_resolved(&mut self, _: GlobalID, _: &Span) {}

    fn on_generic_arguments_resolved(
        &mut self,
        _: GlobalID,
        _: Option<&GenericArguments<S>>,
        _: &Span,
    ) {
    }

    fn on_resolved(
        &mut self,
        resolution: &Resolution<S>,
        generic_identifier_span: &Span,
    ) {
        self.resolution.entry(self.current_flag).or_default().push(
            ResolutionInfo {
                resolution: resolution.clone(),
                generic_identifier_span: generic_identifier_span.clone(),
            },
        );
    }

    fn on_constant_arguments_resolved(
        &mut self,
        constant: &Constant<S>,
        expected_type: &Type<S>,
        constant_expression_span: &Span,
    ) {
        self.constants.entry(self.current_flag).or_default().push(
            ConstantResolutionInfo {
                constant: constant.clone(),
                expected_type: expected_type.clone(),
                constant_expression_span: constant_expression_span.clone(),
            },
        );
    }

    fn extra_lifetime_provider(&self, _: &str) -> Option<Lifetime<S>> { None }

    fn on_type_resolved(
        &mut self,
        ty: &term::r#type::Type<S>,
        type_span: &Span,
    ) {
        self.types.entry(self.current_flag).or_default().push(
            TypeResolutionInfo {
                r#type: ty.clone(),
                type_span: type_span.clone(),
            },
        );
    }
}

impl<
        'f,
        's,
        S: Model,
        First: resolution::Config<S> + ?Sized,
        Second: resolution::Config<S> + ?Sized,
    > resolution::Config<S>
    for ResolutionConfigChain<'f, 's, S, First, Second>
{
    fn lifetime_arguments_placeholder(&mut self) -> Option<Lifetime<S>> {
        self.first
            .lifetime_arguments_placeholder()
            .or_else(|| self.second.lifetime_arguments_placeholder())
    }

    fn type_arguments_placeholder(&mut self) -> Option<Type<S>> {
        self.first
            .type_arguments_placeholder()
            .or_else(|| self.second.type_arguments_placeholder())
    }

    fn constant_arguments_placeholder(&mut self) -> Option<Constant<S>> {
        self.first
            .constant_arguments_placeholder()
            .or_else(|| self.second.constant_arguments_placeholder())
    }

    fn on_global_id_resolved(
        &mut self,
        global_id: GlobalID,
        identifier_span: &Span,
    ) {
        self.first.on_global_id_resolved(global_id, identifier_span);
        self.second.on_global_id_resolved(global_id, identifier_span);
    }

    fn on_generic_arguments_resolved(
        &mut self,
        global_id: GlobalID,
        generic_arguments: Option<&GenericArguments<S>>,
        generic_identifier_span: &Span,
    ) {
        self.first.on_generic_arguments_resolved(
            global_id,
            generic_arguments,
            generic_identifier_span,
        );
        self.second.on_generic_arguments_resolved(
            global_id,
            generic_arguments,
            generic_identifier_span,
        );
    }

    fn on_resolved(
        &mut self,
        resolution: &Resolution<S>,
        generic_identifier_span: &Span,
    ) {
        self.first.on_resolved(resolution, generic_identifier_span);
        self.second.on_resolved(resolution, generic_identifier_span);
    }

    fn on_constant_arguments_resolved(
        &mut self,
        constant: &Constant<S>,
        expected_type: &Type<S>,
        constant_expression_span: &Span,
    ) {
        self.first.on_constant_arguments_resolved(
            constant,
            expected_type,
            constant_expression_span,
        );
        self.second.on_constant_arguments_resolved(
            constant,
            expected_type,
            constant_expression_span,
        );
    }

    fn extra_lifetime_provider(&self, name: &str) -> Option<Lifetime<S>> {
        self.first
            .extra_lifetime_provider(name)
            .or_else(|| self.second.extra_lifetime_provider(name))
    }

    fn on_type_resolved(
        &mut self,
        ty: &term::r#type::Type<S>,
        type_span: &Span,
    ) {
        self.first.on_type_resolved(ty, type_span);
        self.second.on_type_resolved(ty, type_span);
    }
}

struct HigherRankedLifetimeProvider<'a, S: Model> {
    higher_ranked_lifetimes: &'a HashMap<&'a str, Lifetime<S>>,
}

impl<S: Model> resolution::Config<S> for HigherRankedLifetimeProvider<'_, S> {
    fn lifetime_arguments_placeholder(&mut self) -> Option<Lifetime<S>> { None }

    fn type_arguments_placeholder(&mut self) -> Option<term::r#type::Type<S>> {
        None
    }

    fn constant_arguments_placeholder(
        &mut self,
    ) -> Option<term::constant::Constant<S>> {
        None
    }

    fn on_global_id_resolved(&mut self, _: GlobalID, _: &Span) {}

    fn on_generic_arguments_resolved(
        &mut self,
        _: GlobalID,
        _: Option<&GenericArguments<S>>,
        _: &Span,
    ) {
    }

    fn on_resolved(&mut self, _: &Resolution<S>, _: &Span) {}

    fn on_constant_arguments_resolved(
        &mut self,
        _: &term::constant::Constant<S>,
        _: &term::r#type::Type<S>,
        _: &Span,
    ) {
    }

    fn on_type_resolved(&mut self, _: &term::r#type::Type<S>, _: &Span) {}

    fn extra_lifetime_provider(&self, name: &str) -> Option<Lifetime<S>> {
        self.higher_ranked_lifetimes.get(name).cloned()
    }
}

impl Table {
    // create a generic parameter for the given generic symbol
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn create_generic_parameters<T: Symbol + Generic>(
        &self,
        generic_id: ID<T>,
        syntax_tree: Option<&syntax_tree::item::GenericParameters>,
        config: &mut dyn resolution::Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: Index<ID<T>>,
        ID<T>: Into<GlobalID> + Into<GenericID>,
        <Self as Index<ID<T>>>::Output: Generic,
    {
        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();

        let mut default_type_syns = Vec::new();
        let mut default_constant_syns = Vec::new();
        let mut errornous_default_parameters = false;

        // extract out the generic parameter syntax trees
        for parameter in
            syntax_tree.iter().flat_map(|x| x.parameter_list().elements())
        {
            match parameter {
                syntax_tree::item::GenericParameter::Constant(constant) => {
                    // extract out the default constant syntax tree
                    constant_parameter_syns
                        .push((constant.identifier(), constant.ty()));

                    if let Some(default) = constant.default() {
                        default_constant_syns.push(default);
                    } else if !default_type_syns.is_empty()
                        || !default_constant_syns.is_empty()
                    {
                        errornous_default_parameters = true;
                    }
                }
                syntax_tree::item::GenericParameter::Type(type_syn)
                    if constant_parameter_syns.is_empty() =>
                {
                    // extract out the default type syntax tree

                    type_parameter_syns.push(type_syn.identifier());

                    if let Some(default) = type_syn.default() {
                        default_type_syns.push(default);
                    } else if !default_type_syns.is_empty()
                        || !default_constant_syns.is_empty()
                    {
                        errornous_default_parameters = true;
                    }
                }
                syntax_tree::item::GenericParameter::Lifetime(lifetiem)
                    if constant_parameter_syns.is_empty()
                        && type_parameter_syns.is_empty() =>
                {
                    lifetime_parameter_syns.push(lifetiem);
                }
                arg => {
                    handler.receive(error::Error::MisorderedGenericParameter(
                        MisorderedGenericParameter {
                            generic_kind: match arg {
                                syntax_tree::item::GenericParameter::Lifetime(_) => {
                                    GenericKind::Lifetime
                                }
                                syntax_tree::item::GenericParameter::Type(_) => GenericKind::Type,
                                syntax_tree::item::GenericParameter::Constant(_) => {
                                    GenericKind::Constant
                                }
                            },
                            generic_parameter_span: arg.span(),
                        },
                    ));
                    return;
                }
            }
        }

        // check for errornous default parameters
        if errornous_default_parameters {
            handler.receive(
                error::Error::DefaultGenericParameterMustBeTrailing(
                    error::DefaultGenericParameterMustBeTrailing {
                        invalid_generic_default_parameter_spans:
                            default_type_syns
                                .iter()
                                .map(|x| x.span())
                                .chain(
                                    default_constant_syns
                                        .iter()
                                        .map(|x| x.span()),
                                )
                                .collect(),
                    },
                ),
            );
        }

        let mut generic_symbol =
            T::get_arena(self).get(generic_id).unwrap().upgradable_read();

        macro_rules! insert_generic_parameter {
            ($syns:ident, $kind:ident, $parameter:ident, $ident_expr:expr, $span_expr:expr, $parameter_expr:expr) => {
                paste! {
                    for $parameter in $syns {
                        let parameter_expr = $parameter_expr;

                        generic_symbol.with_upgraded(|generic_symbol| {
                            let generic_declaration = generic_symbol.generic_declaration_mut();

                            match generic_declaration
                                .parameters
                                .[<$kind:lower _parameter_ids_by_name>]
                                .entry($ident_expr) {
                                Entry::Vacant(entry) => {
                                    let id = generic_declaration
                                        .parameters
                                        .[<$kind:lower s>]
                                        .insert(parameter_expr);

                                    entry.insert(id);
                                }
                                Entry::Occupied(entry) => {
                                    handler.receive(error::Error::[<$kind ParameterDuplication>](
                                        GenericParameterDuplication {
                                            existing_generic_parameter_id: [<$kind ParameterID>] {
                                                parent: generic_id.into(),
                                                id: *entry.get(),
                                            },
                                            duplicating_generic_parameter_span: $span_expr,
                                        },
                                    ));
                                }
                            }
                        });
                    }
                }
            };
        }

        let this_accessibility = self
            .get_accessibility(generic_id.into())
            .expect("should be a valid id");

        insert_generic_parameter!(
            lifetime_parameter_syns,
            Lifetime,
            parameter,
            parameter.identifier().span.str().to_owned(),
            parameter.span(),
            LifetimeParameter {
                name: Some(parameter.identifier().span.str().to_owned()),
                parent_generic_id: generic_id.into(),
                span: Some(parameter.span()),
            }
        );

        insert_generic_parameter!(
            type_parameter_syns,
            Type,
            parameter,
            parameter.span.str().to_owned(),
            parameter.span(),
            TypeParameter {
                name: parameter.span.str().to_owned(),
                parent_generic_id: generic_id.into(),
                span: Some(parameter.span()),
            }
        );

        insert_generic_parameter!(
            constant_parameter_syns,
            Constant,
            parameter,
            parameter.0.span.str().to_owned(),
            parameter.0.span(),
            {
                let constant_type = self
                    .resolve_type(
                        parameter.1,
                        generic_id.into(),
                        config,
                        handler,
                    )
                    .unwrap_or_default();

                let ty_accessibility = self
                    .get_type_overall_accessibility(&constant_type)
                    .expect("should be valid");

                // no private type in public interface
                if this_accessibility > ty_accessibility {
                    handler.receive(
                        error::Error::PrivateTypeLeakedToPublicInterface(
                            PrivateEntityLeakedToPublicInterface {
                                entity: constant_type.clone(),
                                leaked_span: parameter.1.span(),
                                public_interface_id: generic_id.into(),
                            },
                        ),
                    );
                }

                ConstantParameter {
                    name: parameter.0.span.str().to_owned(),
                    parent_generic_id: generic_id.into(),
                    r#type: constant_type,
                    span: Some(parameter.0.span()),
                }
            }
        );

        if errornous_default_parameters {
            return;
        }

        // create default parameters
        for default_ty in default_type_syns {
            let ty = self
                .resolve_type(
                    default_ty.value(),
                    generic_id.into(),
                    config,
                    handler,
                )
                .unwrap_or_default();

            // no private type in public interface
            if this_accessibility
                > self
                    .get_type_overall_accessibility(&ty)
                    .expect("should be valid")
            {
                handler.receive(
                    error::Error::PrivateTypeLeakedToPublicInterface(
                        PrivateEntityLeakedToPublicInterface {
                            entity: ty.clone(),
                            leaked_span: default_ty.span(),
                            public_interface_id: generic_id.into(),
                        },
                    ),
                );
            }

            generic_symbol.with_upgraded(|generic_parameter| {
                generic_parameter
                    .generic_declaration_mut()
                    .parameters
                    .default_type_parameters
                    .push(ty);
            });
        }

        for default_constant in default_constant_syns {
            let default_constant_value = self
                .evaluate_functional(
                    default_constant.value(),
                    generic_id.into(),
                    handler,
                )
                .unwrap_or_default();

            // no private constant in public interface
            if this_accessibility
                > self
                    .get_constant_overall_accessibility(&default_constant_value)
                    .expect("should be valid")
            {
                handler.receive(
                    error::Error::PrivateConstantLeakedToPublicInterface(
                        PrivateEntityLeakedToPublicInterface {
                            entity: default_constant_value.clone(),
                            leaked_span: default_constant.value().span(),
                            public_interface_id: generic_id.into(),
                        },
                    ),
                );
            }

            generic_symbol.with_upgraded(|generic_parameter| {
                generic_parameter
                    .generic_declaration_mut()
                    .parameters
                    .default_constant_parameters
                    .push(default_constant_value);
            });
        }
    }

    fn build_with_parent_and_get<T: Symbol, Child: Build, Parent, S: Model>(
        config: &Config<T>,
        with_parent: &WithParent<WithGenerics<ID<Child>, S>, S>,
        build_flag: Child::Flag,
        get_parent_id: impl FnOnce(&Child) -> ID<Parent>,
    ) -> (GenericID, Substitution<S>)
    where
        ID<T>: Into<GlobalID>,
        ID<Child>: Into<GlobalID> + Into<GenericID> + Copy,
        Self: Index<ID<Child>, Output = Child>,
        ID<Parent>: Into<GenericID>,
    {
        let _ = config.build_to::<Child>(with_parent.member.id, build_flag);
        let parent_id = {
            let child =
                config.table().get(with_parent.member.id).expect("invalid ID");
            get_parent_id(&child)
        };

        (
            with_parent.member.id.into(),
            Substitution::from_generic_arguments(
                with_parent.parent_generic_arguments.clone(),
                parent_id.into(),
            )
            .append_from_generic_arguments(
                with_parent.member.generic_arguments.clone(),
                with_parent.member.id.into(),
            )
            .expect("should have no overlapping"),
        )
    }

    fn build_and_get<T: Symbol, U: Build, S: Model>(
        config: &Config<T>,
        with_generics: &WithGenerics<ID<U>, S>,
        build_flag: U::Flag,
    ) -> (GenericID, Substitution<S>)
    where
        ID<T>: Into<GlobalID>,
        ID<U>: Into<GenericID> + Into<GlobalID> + Copy,
    {
        let _ = config.build_to::<U>(with_generics.id, build_flag);

        (
            with_generics.id.into(),
            Substitution::from_generic_arguments(
                with_generics.generic_arguments.clone(),
                with_generics.id.into(),
            ),
        )
    }

    /// Gets all the required where clause predicates to make this resolution
    /// well-formed.
    #[allow(clippy::too_many_lines)]
    fn get_required_predicates<T: Symbol, S: Model>(
        config: &Config<T>,
        resolution: &Resolution<S>,
    ) -> Vec<Predicate<S>>
    where
        ID<T>: Into<GlobalID>,
    {
        let Some((generic_id, substitution)): Option<(
            GenericID,
            Substitution<S>,
        )> = (match resolution {
            Resolution::Module(_)
            | Resolution::TraitConstant(_)
            | Resolution::ImplementationConstant(_)
            | Resolution::Variant(_)
            | Resolution::Constant(_) => None,

            Resolution::Enum(id) => Some(Self::build_and_get::<T, Enum, S>(
                config,
                id,
                state::r#enum::Flag::WhereClause,
            )),
            Resolution::Struct(id) => {
                Some(Self::build_and_get::<T, Struct, S>(
                    config,
                    id,
                    state::r#struct::Flag::WhereClause,
                ))
            }
            Resolution::Function(id) => {
                Some(Self::build_and_get::<T, Function, S>(
                    config,
                    id,
                    state::function::Flag::WhereClause,
                ))
            }
            Resolution::Trait(id) => Some(Self::build_and_get::<T, Trait, S>(
                config,
                id,
                state::r#trait::Flag::WhereClause,
            )),
            Resolution::TraitFunction(id) => {
                Some(Self::build_with_parent_and_get::<
                    T,
                    TraitFunction,
                    Trait,
                    S,
                >(
                    config,
                    id,
                    state::trait_function::Flag::WhereClause,
                    |symbol| symbol.parent_trait_id,
                ))
            }
            Resolution::TraitType(id) => {
                Some(Self::build_with_parent_and_get::<T, TraitType, Trait, S>(
                    config,
                    id,
                    state::trait_type::Flag::WhereClause,
                    |symbol| symbol.parent_trait_id,
                ))
            }
            Resolution::Type(id) => {
                Some(Self::build_and_get::<T, symbol::Type, S>(
                    config,
                    id,
                    state::r#type::Flag::WhereClauseAndCheck,
                ))
            }
            Resolution::ImplementationFunction(id) => {
                Some(Self::build_and_get::<T, ImplementationFunction, S>(
                    config,
                    id,
                    state::implementation_function::Flag::WhereClause,
                ))
            }
            Resolution::ImplementationType(id) => {
                Some(Self::build_and_get::<T, ImplementationType, S>(
                    config,
                    id,
                    state::implementation_type::Flag::WhereClause,
                ))
            }
        }) else {
            return Vec::new();
        };

        let mut predicates = config
            .table()
            .get_generic(generic_id)
            .expect("invalid ID")
            .generic_declaration()
            .predicates
            .iter()
            .map(|x| {
                let mut predicate = x.predicate.clone().into_other_model();
                predicate.apply(&substitution); // apply substitution to every predicate
                predicate
            })
            .collect::<Vec<_>>();

        if let Resolution::Trait(resolution) = resolution {
            predicates.push(Predicate::Trait(
                crate::semantic::predicate::Trait {
                    trait_id: resolution.id,
                    const_trait: false,
                    generic_arguments: resolution.generic_arguments.clone(),
                },
            ));
        }

        predicates
    }

    // create trait member predicates for the given generic symbol
    fn create_trait_member_predicate<T: Symbol + Generic>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::TraitMemberBound,
        config: &mut dyn resolution::Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: Index<ID<T>> + IndexMut<ID<T>>,
        ID<T>: Into<GlobalID> + Into<GenericID>,
        <Self as Index<ID<T>>>::Output: Generic,
    {
        let Ok(resolution) = self.resolve(
            syntax_tree.qualified_identifier(),
            generic_id.into(),
            config,
            handler,
        ) else {
            return;
        };

        match (resolution, syntax_tree.argument()) {
            // trait type
            (
                Resolution::TraitType(trait_ty),
                TraitMemberBoundArgument::Type(ty),
            ) => {
                let Ok(resolve_ty) =
                    self.resolve_type(ty, generic_id.into(), config, handler)
                else {
                    return;
                };

                let mut generic_symbol = self.get_mut(generic_id).unwrap();
                generic_symbol.generic_declaration_mut().predicates.push(
                    symbol::Predicate {
                        predicate: predicate::Predicate::TypeEquals(Equals {
                            lhs: term::r#type::Type::TraitMember(
                                term::r#type::TraitMember {
                                    trait_type_id: trait_ty.member.id,
                                    trait_generic_arguments: trait_ty
                                        .parent_generic_arguments,
                                    member_generic_arguments: trait_ty
                                        .member
                                        .generic_arguments,
                                },
                            ),
                            rhs: resolve_ty,
                        }),
                        span: Some(syntax_tree.span()),
                        explicit: true,
                    },
                );
            }

            // trait constant
            (
                Resolution::TraitConstant(trait_constant),
                TraitMemberBoundArgument::Constant(constant),
            ) => {
                let Ok(evaluated_constant) = self.evaluate(
                    constant.expression(),
                    generic_id.into(),
                    handler,
                ) else {
                    return;
                };

                let mut generic_symbol = self.get_mut(generic_id).unwrap();
                generic_symbol.generic_declaration_mut().predicates.push(
                    symbol::Predicate {
                        predicate: predicate::Predicate::ConstantEquals(
                            Equals {
                                lhs: term::constant::Constant::TraitMember(
                                    term::constant::TraitMember {
                                        trait_constant_id: trait_constant
                                            .member,
                                        trait_arguments: trait_constant
                                            .parent_generic_arguments,
                                    },
                                ),
                                rhs: evaluated_constant,
                            },
                        ),
                        span: Some(syntax_tree.span()),
                        explicit: true,
                    },
                );
            }

            // mismatched type
            (
                Resolution::TraitType(..) | Resolution::TraitConstant(..),
                mismatched,
            ) => handler.receive(
                error::Error::TraitMemberBoundArgumentMismatched(
                    TraitMemberBoundArgumentMismatched {
                        trait_member_bound_argument_span: mismatched.span(),
                    },
                ),
            ),

            (resolved, _) => {
                handler.receive(error::Error::TraitMemberExpected(
                    TraitMemberExpected {
                        non_trait_member_span: syntax_tree.argument().span(),
                    },
                ));
            }
        }
    }

    fn create_trait_bound_predicate<T: Symbol + Generic>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::TraitBound,
        config: &mut dyn resolution::Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: Index<ID<T>> + IndexMut<ID<T>>,
        ID<T>: Into<GlobalID> + Into<GenericID>,
        <Self as Index<ID<T>>>::Output: Generic,
    {
        if self
            .resolve_trait_path(
                syntax_tree.qualified_identifier(),
                generic_id.into(),
                handler,
            )
            .is_err()
        {
            return;
        }

        // creates higher-ranked lifetime maps
        let higher_ranked_lifetimes = {
            let mut higher_ranked_lifetimes = HashMap::new();

            if let Some(higher_ranked_lifetimes_syns) =
                syntax_tree.higher_ranked_lifetime_parameters().as_ref()
            {
                for higher_ranked_lifetime_syn in higher_ranked_lifetimes_syns
                    .lifetime_parameter_list()
                    .elements()
                {
                    match higher_ranked_lifetimes.entry(
                        higher_ranked_lifetime_syn.identifier().span.str(),
                    ) {
                        Entry::Occupied(_) => {
                            handler.receive(
                                error::Error::HigherRankedLifetimeRedeclaration(
                                    error::HigherRankedLifetimeRedeclaration {
                                        redeclaration_span:
                                            higher_ranked_lifetime_syn.span(),
                                    },
                                ),
                            );
                        }
                        Entry::Vacant(entry) => {
                            entry.insert(Lifetime::Forall(Forall::generate()));
                        }
                    }
                }
            };

            higher_ranked_lifetimes
        };

        let mut higher_ranked_lifetime_provider =
            HigherRankedLifetimeProvider {
                higher_ranked_lifetimes: &higher_ranked_lifetimes,
            };

        let Ok(resolution) = self
            .resolve(
                syntax_tree.qualified_identifier(),
                generic_id.into(),
                &mut ResolutionConfigChain::new(
                    &mut higher_ranked_lifetime_provider,
                    config,
                ),
                handler,
            )
            .map(|x| {
                x.into_trait()
                    .expect("should be a trait as we've resolveed earlier")
            })
        else {
            return;
        };

        self.get_mut(generic_id)
            .unwrap()
            .generic_declaration_mut()
            .predicates
            .push(symbol::Predicate {
                predicate: Predicate::Trait(predicate::Trait {
                    trait_id: resolution.id,
                    const_trait: syntax_tree.const_keyword().is_some(),
                    generic_arguments: resolution
                        .generic_arguments
                        .into_other_model(),
                }),
                span: Some(syntax_tree.span()),
                explicit: true,
            });
    }

    fn create_lifetime_outlives_predicate<T: Symbol + Generic>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::LifetimeBound,
        config: &mut dyn resolution::Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: Index<ID<T>> + IndexMut<ID<T>>,
        ID<T>: Into<GlobalID> + Into<GenericID>,
        <Self as Index<ID<T>>>::Output: Generic,
    {
        let mut arguments = Vec::new();

        for argument_syn in syntax_tree.arguments().elements() {
            let Ok(argument) = self.resolve_lifetime(
                argument_syn,
                generic_id.into(),
                config,
                handler,
            ) else {
                continue;
            };

            arguments.push(argument);
        }

        match syntax_tree.operand() {
            LifetimeBoundOperand::LifetimeParameter(lt_parameter) => {
                let Ok(lifetime_parameter_id) = self
                    .resolve_lifetime_parameter(
                        lt_parameter.identifier(),
                        generic_id.into(),
                        handler,
                    )
                else {
                    return;
                };

                let mut generic_symbol =
                    self.get_mut(generic_id).expect("should've been valid");

                for argument in arguments {
                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::LifetimeOutlives(
                                LifetimeOutlives {
                                    operand: Lifetime::Parameter(
                                        lifetime_parameter_id,
                                    ),
                                    argument,
                                },
                            ),
                            span: Some(syntax_tree.span()),
                            explicit: true,
                        },
                    );
                }
            }
            LifetimeBoundOperand::QualifiedIdentifier(qualified_identifier) => {
                let Ok(ty) = self.resolve_qualified_identifier_type(
                    qualified_identifier,
                    generic_id.into(),
                    config,
                    handler,
                ) else {
                    return;
                };

                let mut generic_symbol = self.get_mut(generic_id).unwrap();

                let ty @ (term::r#type::Type::Parameter(..)
                | term::r#type::Type::TraitMember(..)) = ty
                else {
                    handler.receive(
                        error::Error::InvalidTypeInOutlivesPredicate(
                            InvalidTypeInOutlivesPredicate {
                                outlive_predicate_span: syntax_tree.span(),
                                invalid_type: ty,
                            },
                        ),
                    );
                    return;
                };

                for argument in arguments {
                    generic_symbol.generic_declaration_mut().predicates.push(
                        symbol::Predicate {
                            predicate: predicate::Predicate::TypeOutlives(
                                TypeOutlives { operand: ty.clone(), argument },
                            ),
                            span: Some(syntax_tree.span()),
                            explicit: true,
                        },
                    );
                }
            }
        }
    }

    fn create_constant_type_predicate<T: Symbol + Generic>(
        &self,
        generic_id: ID<T>,
        syntax_tree: &syntax_tree::item::ConstantTypeBound,
        config: &mut dyn resolution::Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: Index<ID<T>> + IndexMut<ID<T>>,
        ID<T>: Into<GlobalID> + Into<GenericID>,
        <Self as Index<ID<T>>>::Output: Generic,
    {
        let Ok(resolved_ty) = self.resolve_qualified_identifier_type(
            syntax_tree.qualified_identifier(),
            generic_id.into(),
            config,
            handler,
        ) else {
            return;
        };

        let ty @ (term::r#type::Type::Parameter(..)
        | term::r#type::Type::TraitMember(..)) = resolved_ty
        else {
            handler.receive(error::Error::InvalidTypeInConstantTypePredicate(
                InvalidTypeInConstantTypePredicate {
                    constant_type_predicate_span: syntax_tree.span(),
                    invalid_type: resolved_ty,
                },
            ));
            return;
        };

        let mut generic_symbol = self.get_mut(generic_id).unwrap();

        generic_symbol.generic_declaration_mut().predicates.push(
            symbol::Predicate {
                predicate: predicate::Predicate::ConstantType(ConstantType {
                    r#type: ty,
                }),
                span: Some(syntax_tree.span()),
                explicit: true,
            },
        );
    }

    // create where clause predicates for the given generic symbol
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn create_where_clause_predicates<T: Symbol + Generic>(
        &self,
        generic_id: ID<T>,
        syntax_tree: Option<&syntax_tree::item::WhereClause>,
        config: &mut dyn resolution::Config<Symbolic>,
        handler: &dyn Handler<error::Error>,
    ) where
        Self: Index<ID<T>> + IndexMut<ID<T>>,
        ID<T>: Into<GlobalID> + Into<GenericID>,
        <Self as Index<ID<T>>>::Output: Generic,
    {
        let Some(where_clause) = syntax_tree else {
            return;
        };

        for clause in where_clause.constraint_list().elements() {
            match clause {
                syntax_tree::item::Predicate::TraitMember(
                    trait_member_bound,
                ) => {
                    self.create_trait_member_predicate(
                        generic_id,
                        trait_member_bound,
                        config,
                        handler,
                    );
                }
                syntax_tree::item::Predicate::Trait(trait_bound) => {
                    self.create_trait_bound_predicate(
                        generic_id,
                        trait_bound,
                        config,
                        handler,
                    );
                }
                syntax_tree::item::Predicate::Lifetime(lifetime_bound) => self
                    .create_lifetime_outlives_predicate(
                        generic_id,
                        lifetime_bound,
                        config,
                        handler,
                    ),
                syntax_tree::item::Predicate::ConstantType(constant_type) => {
                    self.create_constant_type_predicate(
                        generic_id,
                        constant_type,
                        config,
                        handler,
                    );
                }
            }
        }
    }
}
