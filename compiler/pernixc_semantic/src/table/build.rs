use std::collections::HashMap;

use pernixc_base::source_file::Span;

use super::{
    resolution::{self, Resolution},
    state::{Config, Symbol},
};
use crate::{
    arena::ID,
    semantic::{
        model::Model,
        term::{constant::Constant, lifetime::Lifetime, r#type::Type, GenericArguments},
    },
    symbol::GlobalID,
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
    fn build(config: Config<Self>, build_flag: Self::Flag);
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

/// Used to store all the symbol resolution occured during building the symbol.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub(super) struct Storage<T: Symbol, S: Model>
where
    ID<T>: Into<GlobalID>,
{
    resolution: HashMap<T::Flag, Vec<ResolutionInfo<S>>>,
    constants: HashMap<T::Flag, Vec<ConstantResolutionInfo<S>>>,
    current_flag: T::Flag,
}

pub(super) struct ResolutionConfigChain<
    'f,
    's,
    S: Model,
    First: resolution::Config<S>,
    Second: resolution::Config<S>,
> {
    first: &'f mut First,
    second: &'s mut Second,
    _phantom: std::marker::PhantomData<S>,
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

    fn on_resolved(&mut self, resolution: &Resolution<S>, generic_identifier_span: &Span) {
        self.resolution
            .entry(self.current_flag)
            .or_default()
            .push(ResolutionInfo {
                resolution: resolution.clone(),
                generic_identifier_span: generic_identifier_span.clone(),
            });
    }

    fn on_constant_resolved(
        &mut self,
        constant: &Constant<S>,
        expected_type: &Type<S>,
        constant_expression_span: &Span,
    ) {
        self.constants
            .entry(self.current_flag)
            .or_default()
            .push(ConstantResolutionInfo {
                constant: constant.clone(),
                expected_type: expected_type.clone(),
                constant_expression_span: constant_expression_span.clone(),
            });
    }
}

impl<'f, 's, S: Model, First: resolution::Config<S>, Second: resolution::Config<S>>
    resolution::Config<S> for ResolutionConfigChain<'f, 's, S, First, Second>
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

    fn on_global_id_resolved(&mut self, global_id: GlobalID, identifier_span: &Span) {
        self.first.on_global_id_resolved(global_id, identifier_span);
        self.second
            .on_global_id_resolved(global_id, identifier_span);
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

    fn on_resolved(&mut self, resolution: &Resolution<S>, generic_identifier_span: &Span) {
        self.first.on_resolved(resolution, generic_identifier_span);
        self.second.on_resolved(resolution, generic_identifier_span);
    }

    fn on_constant_resolved(
        &mut self,
        constant: &Constant<S>,
        expected_type: &Type<S>,
        constant_expression_span: &Span,
    ) {
        self.first
            .on_constant_resolved(constant, expected_type, constant_expression_span);
        self.second
            .on_constant_resolved(constant, expected_type, constant_expression_span);
    }

    fn extra_lifetime_provider(&self, name: &str) -> Option<Lifetime<S>> {
        self.first
            .extra_lifetime_provider(name)
            .or_else(|| self.second.extra_lifetime_provider(name))
    }
}
