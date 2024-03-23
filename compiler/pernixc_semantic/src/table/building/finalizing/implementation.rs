use std::collections::HashSet;

use parking_lot::RwLockReadGuard;
use pernixc_base::diagnostic::Handler;

use super::Finalizer;
use crate::{
    arena::ID,
    error::{self, UnusedGenericParameterInImplementation},
    semantic::{
        self, session,
        term::{constant, lifetime, r#type, GenericArguments},
    },
    symbol::{
        ConstantParameterID, GenericID, GenericParameters, GlobalID,
        ImplementationTemplate, LifetimeParameterID, TypeParameterID,
    },
    table::{Element, Index, Representation, State, Table},
};

/// Contains all the unused generic parameters in a generic arguments.
pub struct UnusedGenericParameters {
    pub lifetimes: HashSet<LifetimeParameterID>,
    pub types: HashSet<TypeParameterID>,
    pub constants: HashSet<ConstantParameterID>,
}

impl UnusedGenericParameters {
    /// Gets the list of unused generic parameters.
    ///
    /// # Parameters
    ///
    /// - `generic_id`: The [`GenericID`] where the `generic_parameters` are
    ///   from.
    /// - `generic_parameters`: The [`GenericParameters`] to compare the
    /// - `generic_arguments`: The generic arguments to search for unused
    ///   parameters.
    pub fn get_unused_generic_parameters(
        generic_id: GenericID,
        generic_parameters: &GenericParameters,
        generic_arguments: &GenericArguments,
    ) -> Self {
        let mut unused_generic_arguments = Self {
            lifetimes: generic_parameters
                .lifetime_order
                .iter()
                .map(|x| LifetimeParameterID { parent: generic_id, id: *x })
                .collect(),
            types: generic_parameters
                .type_order
                .iter()
                .map(|x| TypeParameterID { parent: generic_id, id: *x })
                .collect(),
            constants: generic_parameters
                .constant_order
                .iter()
                .map(|x| ConstantParameterID { parent: generic_id, id: *x })
                .collect(),
        };

        unused_generic_arguments.check_in_generic_arguments(generic_arguments);

        unused_generic_arguments
    }

    /// Reports the unused generic parameters in an implementation.
    pub fn report_as_unused_generic_parameters_in_implementation<
        ID: Into<GenericID> + Copy + std::fmt::Debug + Send + Sync + 'static,
    >(
        &self,
        implementation_kind_id: ID,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) {
        for unused_lt in &self.lifetimes {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_lt.id.into(),
                implementation_id: implementation_kind_id,
            }));
        }

        for unused_ty in &self.types {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_ty.id.into(),
                implementation_id: implementation_kind_id,
            }));
        }

        for unused_val in &self.constants {
            handler.receive(Box::new(UnusedGenericParameterInImplementation {
                generic_parameter_id: unused_val.id.into(),
                implementation_id: implementation_kind_id,
            }));
        }
    }

    fn check_in_generic_arguments(&mut self, args: &GenericArguments) {
        for lt in &args.lifetimes {
            self.check_in_lifetime(lt);
        }

        for ty in &args.types {
            self.check_in_type(ty);
        }

        for val in &args.constants {
            self.check_in_constant(val);
        }
    }

    fn check_in_type(&mut self, ty: &r#type::Type) {
        match ty {
            r#type::Type::Parameter(parameter) => {
                self.types.remove(parameter);
            }
            r#type::Type::Symbol(symbol) => {
                self.check_in_generic_arguments(&symbol.generic_arguments);
            }
            r#type::Type::Pointer(pointer) => {
                self.check_in_type(&pointer.pointee);
            }
            r#type::Type::Reference(reference) => {
                self.check_in_lifetime(&reference.lifetime);
                self.check_in_type(&reference.pointee);
            }
            r#type::Type::Array(array) => {
                self.check_in_type(&array.r#type);
                self.check_in_constant(&array.length);
            }
            r#type::Type::Tuple(tuple) => {
                for ty in &tuple.elements {
                    self.check_in_type(ty.as_term());
                }
            }
            r#type::Type::Local(local) => {
                self.check_in_type(&local.0);
            }
            r#type::Type::MemberSymbol(member_symbol) => {
                self.check_in_generic_arguments(
                    &member_symbol.member_generic_arguments,
                );
                self.check_in_generic_arguments(
                    &member_symbol.parent_generic_arguments,
                );
            }

            r#type::Type::TraitMember(_)
            | r#type::Type::Inference(_)
            | r#type::Type::Primitive(_) => {}
        }
    }
    fn check_in_lifetime(&mut self, lt: &lifetime::Lifetime) {
        match lt {
            lifetime::Lifetime::Parameter(parameter) => {
                self.lifetimes.remove(parameter);
            }

            lifetime::Lifetime::Inference(_)
            | lifetime::Lifetime::Static
            | lifetime::Lifetime::Local(_)
            | lifetime::Lifetime::Forall(_) => {}
        }
    }
    fn check_in_constant(&mut self, val: &constant::Constant) {
        match val {
            constant::Constant::Parameter(parameter) => {
                self.constants.remove(parameter);
            }

            constant::Constant::Primitive(_)
            | constant::Constant::TraitMember(_)
            | constant::Constant::Inference(_) => {}

            constant::Constant::Struct(val) => {
                for field in &val.fields {
                    self.check_in_constant(field);
                }
            }

            constant::Constant::Enum(val) => {
                if let Some(value) = &val.associated_value {
                    self.check_in_constant(value);
                }
            }

            constant::Constant::Array(array) => {
                for element in &array.elements {
                    self.check_in_constant(element);
                }
            }

            constant::Constant::Local(local) => {
                self.check_in_constant(&local.0);
            }

            constant::Constant::Tuple(tuple) => {
                for element in &tuple.elements {
                    self.check_in_constant(element.as_term());
                }
            }

            constant::Constant::Symbol(symbol) => {
                self.check_in_generic_arguments(&symbol.generic_arguments);
            }

            constant::Constant::MemberSymbol(member_symbol) => {
                self.check_in_generic_arguments(
                    &member_symbol.member_generic_arguments,
                );
                self.check_in_generic_arguments(
                    &member_symbol.parent_generic_arguments,
                );
            }
        }
    }
}

impl Table<Finalizer> {
    pub(super) fn implementation_signature_check<
        'a,
        T: Element,
        U: 'a + Copy + Into<GenericID>,
        V: 'static,
    >(
        &'a self,
        implementation_id: ID<T>,
        handler: &dyn Handler<Box<dyn error::Error>>,
    ) where
        Representation<<Finalizer as State>::Container>: Index<
            ID<T>,
            Output<'a> = RwLockReadGuard<'a, ImplementationTemplate<U, V>>,
        >,
        ID<T>: Into<GenericID> + Into<GlobalID>,
    {
        let implementation_sym = self.get(implementation_id).unwrap();

        // check if all the generic parameters are used in the
        // implementation arguments
        let unused_generic_parameters =
            UnusedGenericParameters::get_unused_generic_parameters(
                implementation_id.into(),
                &implementation_sym.signature.generic_declaration.parameters,
                &implementation_sym.signature.arguments,
            );
        unused_generic_parameters
            .report_as_unused_generic_parameters_in_implementation(
                implementation_id,
                handler,
            );

        // check if the signature matches the trait definition
        let mut semantic = semantic::Default;
        let mut session = session::Default::default();
        let premise =
            self.get_active_premise(implementation_id.into()).unwrap();

        self.check_instantiation_predicates_from_generic_arguments(
            &premise,
            implementation_sym.signature.implemented_id.into(),
            implementation_sym.signature.arguments.clone(),
            implementation_sym.span.as_ref().unwrap(),
            &mut semantic,
            &mut session,
            handler,
        );
    }
}
