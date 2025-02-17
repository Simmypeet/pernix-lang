//! Contains the logic mapping from the Pernix type to LLVM's type.

use std::{
    borrow::Cow,
    collections::{BTreeSet, HashMap},
};

use inkwell::{
    types::{BasicType, BasicTypeEnum},
    AddressSpace,
};
use pernixc_component::fields::Fields;
use pernixc_ir::model::Erased;
use pernixc_table::component::SymbolKind;
use pernixc_term::{
    constant::Constant,
    generic_arguments::GenericArguments,
    generic_parameter::GenericParameters,
    instantiation::{self, Instantiation},
    lifetime::Lifetime,
    r#type::{Primitive, Type},
    sub_term::TermLocation,
    visitor::MutableRecursive,
    Model as _, Symbol,
};
use pernixc_type_system::{
    environment::{Environment, Premise},
    normalizer,
    term::Term,
};

use crate::{context::Context, Model};

/// Represents the mapping between the Pernix type and the LLVM type.
#[derive(Debug, Default)]
pub struct Map<'ctx> {
    /// The mapping between the ADT instantiation and the LLVM type.
    adt_instantiation: HashMap<Symbol<Model>, BasicTypeEnum<'ctx>>,
}

/// A mutable visitor that erases all the lifetimes in the term.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct EraseLifetime;

impl MutableRecursive<Lifetime<Model>> for EraseLifetime {
    fn visit(
        &mut self,
        term: &mut Lifetime<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        *term = Lifetime::Inference(Erased);

        true
    }
}

impl MutableRecursive<Type<Model>> for EraseLifetime {
    fn visit(
        &mut self,
        _: &mut Type<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl MutableRecursive<Constant<Model>> for EraseLifetime {
    fn visit(
        &mut self,
        _: &mut Constant<Model>,
        _: impl Iterator<Item = TermLocation>,
    ) -> bool {
        true
    }
}

impl<'ctx> Context<'_, 'ctx> {
    /// Performs [`Self::normalize_term`] on all the terms in the generic
    /// arguments.
    pub fn normalize_generic_arguments(
        &self,
        generic_arguments: &mut GenericArguments<Model>,
    ) {
        let mut erase_lifetime = EraseLifetime;

        for lifetime in &mut generic_arguments.lifetimes {
            pernixc_term::visitor::accept_recursive_mut(
                lifetime,
                &mut erase_lifetime,
            );
        }

        for ty in &mut generic_arguments.types {
            pernixc_term::visitor::accept_recursive_mut(
                ty,
                &mut erase_lifetime,
            );
            take_mut::take(ty, |ty| self.normalize_term(ty));
        }

        for constant in &mut generic_arguments.constants {
            pernixc_term::visitor::accept_recursive_mut(
                constant,
                &mut erase_lifetime,
            );
            take_mut::take(constant, |constant| self.normalize_term(constant));
        }
    }

    /// Normalizes the term by removing all the trait member types and packed
    /// tuple elements.
    pub fn normalize_term<T: Term<Model = Model>>(&self, mut term: T) -> T {
        let env = Environment::new(
            Cow::Owned(Premise::<Model> {
                predicates: BTreeSet::default(),
                // NOTE: query site should not influence the simplification
                // process
                query_site: None,
            }),
            self.table(),
            normalizer::NO_OP,
        );

        let mut erase_lifetime = EraseLifetime;
        pernixc_term::visitor::accept_recursive_mut(
            &mut term,
            &mut erase_lifetime,
        );

        env.simplify(term).unwrap().result.clone()
    }

    /// Mono-morphizes the term by instantiating the term, normalizing it, and
    /// erase all the lifetimes.
    pub fn monomorphize_term<T: Term<Model = Model>>(
        &self,
        mut term: T,
        instantiation: &Instantiation<Model>,
    ) -> T {
        instantiation::instantiate(&mut term, instantiation);

        self.normalize_term(term)
    }

    /// Retrieves the LLVM type from the Pernix type. The Pernix type must be
    /// fully monomorphized, meaning that there shouldn't be generic parameters,
    /// trait members, or non-erased lifetimes.
    pub fn get_type(&mut self, ty: Type<Model>) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Primitive(primitive) => match primitive {
                Primitive::Int8 | Primitive::Uint8 => {
                    self.context().i8_type().into()
                }
                Primitive::Int16 | Primitive::Uint16 => {
                    self.context().i16_type().into()
                }
                Primitive::Int32 | Primitive::Uint32 => {
                    self.context().i32_type().into()
                }
                Primitive::Int64 | Primitive::Uint64 => {
                    self.context().i64_type().into()
                }

                Primitive::Float32 => self.context().f32_type().into(),
                Primitive::Float64 => self.context().f64_type().into(),
                Primitive::Bool => self.context().bool_type().into(),

                Primitive::Isize | Primitive::Usize => self
                    .context()
                    .ptr_sized_int_type(self.target_data(), None)
                    .into(),
            },

            Type::Error(error) => {
                panic!("error type found {error:?}")
            }
            Type::Parameter(member_id) => {
                panic!("non-monomorphized type found {member_id:?}")
            }

            Type::Reference(_) | Type::Pointer(_) => {
                self.context().ptr_type(AddressSpace::default()).into()
            }

            Type::Inference(infer) => match infer {},

            Type::Array(array_ty) => {
                let element_ty = self.get_type(*array_ty.r#type);

                element_ty
                    .array_type(
                        (*array_ty
                            .length
                            .as_primitive()
                            .unwrap()
                            .as_usize()
                            .unwrap())
                        .try_into()
                        .unwrap(),
                    )
                    .into()
            }

            Type::Symbol(symbol) => {
                // already monomorphized
                if let Some(value) =
                    self.type_map_mut().adt_instantiation.get(&symbol).copied()
                {
                    return value;
                }

                let generic_params =
                    self.table().query::<GenericParameters>(symbol.id).unwrap();
                let symbol_kind = *self.table().get::<SymbolKind>(symbol.id);

                let instantiation = Instantiation::from_generic_arguments(
                    symbol.generic_arguments.clone(),
                    symbol.id,
                    &generic_params,
                )
                .unwrap();

                match symbol_kind {
                    SymbolKind::Struct => {
                        let fields =
                            self.table().query::<Fields>(symbol.id).unwrap();

                        let llvm_fields = fields
                            .field_declaration_order
                            .iter()
                            .copied()
                            .map(|field_id| {
                                self.get_type(self.monomorphize_term(
                                    Model::from_default_type(
                                        fields.fields[field_id].r#type.clone(),
                                    ),
                                    &instantiation,
                                ))
                            })
                            .collect::<Vec<_>>();

                        let struct_ty =
                            self.context().struct_type(&llvm_fields, false);

                        self.type_map_mut()
                            .adt_instantiation
                            .insert(symbol, struct_ty.into());

                        struct_ty.into()
                    }

                    SymbolKind::Enum => todo!(),

                    _ => panic!("unsupported symbol kind {symbol_kind:?}"),
                }
            }

            Type::Tuple(_)
            | Type::Phantom(_)
            | Type::MemberSymbol(_)
            | Type::TraitMember(_) => panic!("unsupported type {ty:?}"),

            Type::FunctionSignature(function_signature) => {
                panic!("should not be here {function_signature:?}")
            }
        }
    }
}
