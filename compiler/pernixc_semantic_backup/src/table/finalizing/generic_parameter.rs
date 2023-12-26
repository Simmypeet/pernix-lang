use std::collections::hash_map::Entry;

use paste::paste;
use pernixc_base::source_file::SourceElement;
use pernixc_syntax::syntax_tree;

use crate::{
    arena::ID,
    entity::{
        lifetime::Lifetime,
        predicate::{ConstantType, Predicate},
    },
    error::{
        self, GenericParameterDuplication, MisorderedGenericParameter,
        PrivateEntityLeakedToPublicInterface,
    },
    symbol::{
        Constant, ConstantParameter, ConstantParameterID, Enum, Function, Generic, GenericID,
        GenericKind, GlobalID, Implementation, ImplementationConstant, ImplementationFunction,
        ImplementationType, LifetimeParameter, LifetimeParameterID, NegativeImplementation, Struct,
        Symbolic, Trait, TraitConstant, TraitFunction, TraitType, Type, TypeParameter,
        TypeParameterID, Variant,
    },
    table::{
        resolution::{self, Checking, Config},
        state::{self, StatefulSymbol},
        Index, IndexMut, IndexRaw, Table,
    },
};

pub(in crate::table) trait Finalize<Syn>
where
    Self: Sized,
{
    fn finalize(table: &Table, syntax_tree: &Syn, config: &mut state::Config<Self>);
}

impl Table {
    // create a generic parameter for the given generic symbol
    #[allow(clippy::too_many_lines, clippy::significant_drop_tightening)]
    fn create_generic_parameters<T>(
        &self,
        syntax_tree: Option<&syntax_tree::item::GenericParameters>,
        config: &mut state::Config<T>,
    ) where
        Self: IndexRaw<ID<T>> + Index<ID<T>, Output = T>,
        T: Generic + state::StatefulSymbol,
        ID<T>: Into<GenericID> + Into<GlobalID> + Copy,
    {
        let mut lifetime_parameter_syns = Vec::new();
        let mut type_parameter_syns = Vec::new();
        let mut constant_parameter_syns = Vec::new();

        let mut default_type_syns = Vec::new();
        let mut default_constant_syns = Vec::new();
        let mut errornous_default_parameters = false;

        // extract out the generic parameter syntax trees
        for parameter in syntax_tree
            .into_iter()
            .flat_map(|x| x.parameter_list().elements())
        {
            match parameter {
                syntax_tree::item::GenericParameter::Constant(constant) => {
                    // extract out the default constant syntax tree
                    constant_parameter_syns.push((constant.identifier(), constant.ty()));

                    if let Some(default) = constant.default() {
                        default_constant_syns.push(default);
                    } else if !default_type_syns.is_empty() || !default_constant_syns.is_empty() {
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
                    } else if !default_type_syns.is_empty() || !default_constant_syns.is_empty() {
                        errornous_default_parameters = true;
                    }
                }
                syntax_tree::item::GenericParameter::Lifetime(lifetiem)
                    if constant_parameter_syns.is_empty() && type_parameter_syns.is_empty() =>
                {
                    lifetime_parameter_syns.push(lifetiem);
                }
                arg => {
                    config
                        .handler()
                        .receive(error::Error::MisorderedGenericParameter(
                            MisorderedGenericParameter {
                                generic_kind: match arg {
                                    syntax_tree::item::GenericParameter::Lifetime(_) => {
                                        GenericKind::Lifetime
                                    }
                                    syntax_tree::item::GenericParameter::Type(_) => {
                                        GenericKind::Type
                                    }
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
            config
                .handler()
                .receive(error::Error::DefaultGenericParameterMustBeTrailing(
                    error::DefaultGenericParameterMustBeTrailing {
                        invalid_generic_default_parameter_spans: default_type_syns
                            .iter()
                            .map(|x| x.span())
                            .chain(default_constant_syns.iter().map(|x| x.span()))
                            .collect(),
                    },
                ));
        }

        let mut generic_symbol = self.get_raw(config.current_id()).unwrap().upgradable_read();

        macro_rules! insert_generic_parameter {
            ($syns:ident, $kind:ident, $parameter:ident, $ident_expr:expr, $span_expr:expr, $parameter_expr:expr) => {
                paste! {
                    for $parameter in $syns {
                        let parameter_expr = $parameter_expr;

                        generic_symbol.with_upgraded(|generic_symbol| {
                            let generic_declaration = generic_symbol
                                .generic_declaration_mut();

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
                                    config
                                        .handler()
                                        .receive(error::Error::[<$kind ParameterDuplication>](
                                            GenericParameterDuplication {
                                                existing_generic_parameter_id: [<$kind ParameterID>] {
                                                    parent: config.current_id().into(),
                                                    id: *entry.get(),
                                                },
                                                duplicating_generic_parameter_span: $span_expr,
                                            }));
                                }
                            };
                        });
                    }
                }
            };
        }

        let this_accessibility = self
            .get_accessibility(config.current_id().into())
            .expect("should be a valid id");

        insert_generic_parameter!(
            lifetime_parameter_syns,
            Lifetime,
            parameter,
            parameter.identifier().span.str().to_owned(),
            parameter.span(),
            LifetimeParameter {
                name: Some(parameter.identifier().span.str().to_owned()),
                parent_generic_id: config.current_id().into(),
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
                parent_generic_id: config.current_id().into(),
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
                let handler = config.handler();
                let constant_type = self
                    .resolve_type(parameter.1, config.current_id().into(), config, handler)
                    .unwrap_or_default();

                let ty_accessibility = self
                    .get_type_overall_accessibility(&constant_type)
                    .expect("should be valid");

                // no private type in public interface
                if this_accessibility > ty_accessibility {
                    config
                        .handler()
                        .receive(error::Error::PrivateTypeLeakedToPublicInterface(
                            PrivateEntityLeakedToPublicInterface {
                                entity: constant_type.clone(),
                                leaked_span: parameter.1.span(),
                                public_interface_id: config.current_id().into(),
                            },
                        ));
                }

                // type must be a type constant
                config.check(
                    Checking::Predicate(Predicate::ConstantType(ConstantType {
                        r#type: constant_type.clone(),
                    })),
                    parameter.1.span(),
                );

                ConstantParameter {
                    name: parameter.0.span.str().to_owned(),
                    parent_generic_id: config.current_id().into(),
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
            let handler = config.handler();
            let ty = self
                .resolve_type(
                    default_ty.value(),
                    config.current_id().into(),
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
                config
                    .handler()
                    .receive(error::Error::PrivateTypeLeakedToPublicInterface(
                        PrivateEntityLeakedToPublicInterface {
                            entity: ty.clone(),
                            leaked_span: default_ty.span(),
                            public_interface_id: config.current_id().into(),
                        },
                    ));
            }

            generic_symbol.with_upgraded(|generic_parameter| {
                generic_parameter
                    .generic_declaration_mut()
                    .parameters
                    .default_type_parameters
                    .push(ty);
            });
        }

        // create default constant parameters
        let starting_index = generic_symbol
            .generic_declaration()
            .parameters
            .constants
            .len()
            - default_constant_syns.len();

        for (idx, default_constant) in default_constant_syns.into_iter().enumerate() {
            let default_constant_value = self
                .evaluate_functional(
                    default_constant.value(),
                    config.current_id().into(),
                    config.handler(),
                )
                .unwrap_or_default();

            let expected_ty = generic_symbol
                .generic_declaration()
                .parameters
                .constants
                .get(ID::new(idx + starting_index))
                .expect("should be a valid index")
                .r#type
                .clone();

            // no private constant in public interface
            if this_accessibility
                > self
                    .get_constant_overall_accessibility(&default_constant_value)
                    .expect("should be valid")
            {
                config
                    .handler()
                    .receive(error::Error::PrivateConstantLeakedToPublicInterface(
                        PrivateEntityLeakedToPublicInterface {
                            entity: default_constant_value.clone(),
                            leaked_span: default_constant.value().span(),
                            public_interface_id: config.current_id().into(),
                        },
                    ));
            }

            // add type checking
            config.check(
                Checking::TypeCheck(default_constant_value.clone(), expected_ty),
                default_constant.value().span(),
            );

            generic_symbol.with_upgraded(|generic_parameter| {
                generic_parameter
                    .generic_declaration_mut()
                    .parameters
                    .default_constant_parameters
                    .push(default_constant_value);
            });
        }
    }
}

macro_rules! implements_finalize {
    (
        $kind:ident,
        $syntax:path,
        $table:ident,
        $syntax_tree:ident,
        $config:ident,
        $generic_parameter:expr
        $(, $pre:expr)?
    ) => {
        paste! {
            impl Finalize<$syntax> for $kind {
                fn finalize(
                    $table: &Table,
                    $syntax_tree: &$syntax,
                    $config: &mut state::Config<Self>,
                ) {
                    $($pre ;)?

                    $table.create_generic_parameters(
                        $generic_parameter,
                        $config,
                    );
                }
            }
        }
    };

    // for non-generic symbol
    ($kind:ident, $syntax_tree:path) => {
        paste! {
            impl Finalize<$syntax_tree> for $kind {
                fn finalize(
                    _: &Table,
                    _: &$syntax_tree,
                    _: &mut state::Config<Self>,
                ) {}
            }
        }
    }
}

pub struct AnnonymousLifetimeProvider<'a, 'b, 'c, T> {
    table: &'c Table,
    config: &'a mut state::Config<'b, T>,
}

impl<T> resolution::Config<Symbolic> for AnnonymousLifetimeProvider<'_, '_, '_, T>
where
    ID<T>: Into<GenericID> + Into<GlobalID> + Copy,
    Table: IndexMut<ID<T>, Output = T>,
    T: Generic + StatefulSymbol,
{
    fn lifetime_arguments_placeholder(
        &mut self,
    ) -> Option<crate::entity::lifetime::Lifetime<Symbolic>> {
        let new_lifetime_parameter = LifetimeParameter {
            name: None,
            parent_generic_id: self.config.current_id().into(),
            span: None,
        };

        let id = self
            .table
            .get_mut(self.config.current_id())
            .unwrap()
            .generic_declaration_mut()
            .parameters
            .lifetimes
            .insert(new_lifetime_parameter);

        Some(Lifetime::Parameter(LifetimeParameterID {
            parent: self.config.current_id().into(),
            id,
        }))
    }

    fn type_arguments_placeholder(&mut self) -> Option<crate::entity::r#type::Type<Symbolic>> {
        self.config.type_arguments_placeholder()
    }

    fn constant_arguments_placeholder(
        &mut self,
    ) -> Option<crate::entity::constant::Constant<Symbolic>> {
        self.config.constant_arguments_placeholder()
    }

    fn check(&mut self, checking: Checking<Symbolic>, span: pernixc_base::source_file::Span) {
        self.config.check(checking, span);
    }

    fn on_global_id_resolved(&mut self, global_id: GlobalID) {
        self.config.on_global_id_resolved(global_id);
    }

    fn on_generic_arguments_resolved(
        &mut self,
        generic_id: GenericID,
        generic_arguments: &crate::entity::GenericArguments<Symbolic>,
    ) {
        self.config
            .on_generic_arguments_resolved(generic_id, generic_arguments);
    }

    fn on_resolved(&mut self, resolved: &resolution::Resolution<Symbolic>) {
        self.config.on_resolved(resolved);
    }

    fn extra_lifetime_provider(&self, name: &str) -> Option<Lifetime<Symbolic>> {
        self.config.extra_lifetime_provider(name)
    }
}

implements_finalize!(
    Enum,
    syntax_tree::item::EnumSignature,
    table,
    syntax_tree,
    config,
    syntax_tree.generic_parameters().as_ref()
);
implements_finalize!(Variant, syntax_tree::item::Variant);
implements_finalize!(
    Struct,
    syntax_tree::item::Struct,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().generic_parameters().as_ref()
);
implements_finalize!(Constant, syntax_tree::item::Constant);
implements_finalize!(
    Type,
    syntax_tree::item::Type,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().generic_parameters().as_ref()
);
implements_finalize!(
    Function,
    syntax_tree::item::Function,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().generic_parameters().as_ref()
);
implements_finalize!(
    Trait,
    syntax_tree::item::TraitSignature,
    table,
    syntax_tree,
    config,
    syntax_tree.generic_parameters().as_ref()
);
implements_finalize!(
    TraitFunction,
    syntax_tree::item::TraitFunction,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().generic_parameters().as_ref(),
    {
        // parent generic parameter must be created first
        let parent_trait_id = table.get(config.current_id()).unwrap().parent_trait_id;
        let _ = config.finalize_to(
            parent_trait_id.into(),
            state::FinalizeFlag::GenericParameter,
        );
    }
);
implements_finalize!(
    TraitType,
    syntax_tree::item::TraitType,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().generic_parameters().as_ref(),
    {
        // parent generic parameter must be created first
        let parent_trait_id = table.get(config.current_id()).unwrap().parent_trait_id;
        let _ = config.finalize_to(
            parent_trait_id.into(),
            state::FinalizeFlag::GenericParameter,
        );
    }
);
implements_finalize!(TraitConstant, syntax_tree::item::TraitConstant);
implements_finalize!(
    ImplementationFunction,
    syntax_tree::item::ImplementationFunction,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().generic_parameters().as_ref(),
    {
        // parent generic parameter must be created first
        let parent_impl_id = table
            .get(config.current_id())
            .unwrap()
            .parent_implementation_id;
        let _ = config.finalize_to(parent_impl_id.into(), state::FinalizeFlag::GenericParameter);
    }
);
implements_finalize!(
    ImplementationType,
    syntax_tree::item::ImplementationType,
    table,
    syntax_tree,
    config,
    syntax_tree.signature().generic_parameters().as_ref(),
    {
        // parent generic parameter must be created first
        let parent_impl_id = table
            .get(config.current_id())
            .unwrap()
            .parent_implementation_id;
        let _ = config.finalize_to(parent_impl_id.into(), state::FinalizeFlag::GenericParameter);
    }
);
implements_finalize!(
    ImplementationConstant,
    syntax_tree::item::ImplementationConstant
);

macro_rules! implements_finalize_for_implementation {
    ($kind:ident) => {
        impl Finalize<syntax_tree::item::ImplementationSignature> for $kind {
            fn finalize(
                table: &Table,
                syntax_tree: &syntax_tree::item::ImplementationSignature,
                config: &mut state::Config<Self>,
            ) {
                // normal generic parameter creation
                table.create_generic_parameters(syntax_tree.generic_parameters().as_ref(), config);

                let _ = config.finalize_to(
                    table
                        .get(config.current_id())
                        .unwrap()
                        .signature
                        .trait_id
                        .into(),
                    state::FinalizeFlag::GenericParameter,
                );

                // resolved for the generic arguments of the parent trait
                let handler = config.handler();
                let implementation_id = config.current_id();
                let parent_trait_id = table.get(implementation_id).unwrap().signature.trait_id;
                let generic_identifier = syntax_tree
                    .qualified_identifier()
                    .rest()
                    .last()
                    .map_or(syntax_tree.qualified_identifier().first(), |(_, x)| x);
                let mut annonymous_lifetime_provider = AnnonymousLifetimeProvider { table, config };

                let generic_argumnent = match table.resolve_generic_arguments(
                    generic_identifier,
                    implementation_id.into(),
                    parent_trait_id.into(),
                    &mut annonymous_lifetime_provider,
                    handler,
                ) {
                    Ok(Some(generic_argumnet)) => generic_argumnet,
                    Ok(None) => {
                        unreachable!("the trait symbol is generic, should have generic arguments")
                    }
                    Err(_) => {
                        // error, this implementation is invalid, no need to go further
                        return;
                    }
                };

                table
                    .get_mut(implementation_id)
                    .unwrap()
                    .signature
                    .arguments = generic_argumnent;
            }
        }
    };
}

implements_finalize_for_implementation!(Implementation);
implements_finalize_for_implementation!(NegativeImplementation);
