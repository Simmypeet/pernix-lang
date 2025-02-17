//! Contains the logic mapping from the Pernix type to LLVM's type.

use std::{
    any::Any,
    borrow::Cow,
    collections::{BTreeSet, HashMap},
};

use enum_as_inner::EnumAsInner;
use getset::Getters;
use inkwell::{
    types::{BasicType, BasicTypeEnum, StructType},
    AddressSpace,
};
use pernixc_arena::ID;
use pernixc_component::fields::{Field, Fields};
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

/// The result of converting a Pernix type to an LLVM type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, EnumAsInner)]
pub enum LlvmType<'ctx> {
    /// The conversion is successful.
    Basic(BasicTypeEnum<'ctx>),

    /// The type is `Zero-Sized Type` (including uninhabitable types); this can
    /// be interpreted to many things in different scenarios such as `void`
    /// in the return type of a function, or no-op in instruction.
    Zst,
}

/// The signature of the struct in LLVM.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LlvmStructSignature<'ctx> {
    /// The LLVM struct type.
    pub llvm_struct_type: StructType<'ctx>,

    /// The LLVM field types in order of declaration.
    pub llvm_field_types: Vec<LlvmType<'ctx>>,

    /// The original struct ID in the Pernix table.
    pub llvm_field_indices_by_field_id: HashMap<ID<Field>, usize>,
}

/// Represents the mapping between the Pernix type and the LLVM type.
#[derive(Debug, Default, Getters)]
pub struct Map<'ctx> {
    /// The mapping between the ADT instantiation and the LLVM type.
    ///
    /// If the value is `None`, it means that the struct is `Zero-sized type`.
    #[get = "pub"]
    struct_sigantures:
        HashMap<Symbol<Model>, Option<LlvmStructSignature<'ctx>>>,
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
    pub fn normalize_instantiation(
        &self,
        instantiation: &mut Instantiation<Model>,
    ) {
        let mut erase_lifetime = EraseLifetime;

        for lifetime in instantiation.lifetimes.values_mut() {
            pernixc_term::visitor::accept_recursive_mut(
                lifetime,
                &mut erase_lifetime,
            );
        }

        for ty in &mut instantiation.types.values_mut() {
            pernixc_term::visitor::accept_recursive_mut(
                ty,
                &mut erase_lifetime,
            );
            take_mut::take(ty, |ty| self.normalize_term(ty));
        }

        for constant in &mut instantiation.constants.values_mut() {
            pernixc_term::visitor::accept_recursive_mut(
                constant,
                &mut erase_lifetime,
            );
            take_mut::take(constant, |constant| self.normalize_term(constant));
        }
    }

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
    #[allow(clippy::too_many_lines)]
    pub fn get_type(&mut self, ty: Type<Model>) -> LlvmType<'ctx> {
        match ty {
            Type::Primitive(primitive) => match primitive {
                Primitive::Int8 | Primitive::Uint8 => {
                    LlvmType::Basic(self.context().i8_type().into())
                }
                Primitive::Int16 | Primitive::Uint16 => {
                    LlvmType::Basic(self.context().i16_type().into())
                }
                Primitive::Int32 | Primitive::Uint32 => {
                    LlvmType::Basic(self.context().i32_type().into())
                }
                Primitive::Int64 | Primitive::Uint64 => {
                    LlvmType::Basic(self.context().i64_type().into())
                }

                Primitive::Float32 => {
                    LlvmType::Basic(self.context().f32_type().into())
                }
                Primitive::Float64 => {
                    LlvmType::Basic(self.context().f64_type().into())
                }
                Primitive::Bool => {
                    LlvmType::Basic(self.context().bool_type().into())
                }

                Primitive::Isize | Primitive::Usize => LlvmType::Basic(
                    self.context()
                        .ptr_sized_int_type(self.target_data(), None)
                        .into(),
                ),
            },

            Type::Error(error) => {
                panic!("error type found {error:?}")
            }
            Type::Parameter(member_id) => {
                panic!("non-monomorphized type found {member_id:?}")
            }

            Type::Reference(_) | Type::Pointer(_) => LlvmType::Basic(
                self.context().ptr_type(AddressSpace::default()).into(),
            ),

            Type::Inference(infer) => match infer {},

            Type::Array(array_ty) => {
                if *array_ty.length.as_primitive().unwrap().as_usize().unwrap()
                    == 0
                {
                    return LlvmType::Zst;
                }
                let LlvmType::Basic(element_ty) =
                    self.get_type(*array_ty.r#type)
                else {
                    return LlvmType::Zst;
                };

                LlvmType::Basic(
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
                        .into(),
                )
            }

            Type::Symbol(symbol) => {
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
                        // already monomorphized
                        if let Some(value) =
                            self.type_map_mut().struct_sigantures.get(&symbol)
                        {
                            match value {
                                Some(ty) => {
                                    return LlvmType::Basic(
                                        ty.llvm_struct_type.into(),
                                    )
                                }
                                None => return LlvmType::Zst,
                            }
                        }
                        let fields =
                            self.table().query::<Fields>(symbol.id).unwrap();

                        let mut llvm_field_types = Vec::new();
                        let mut llvm_field_indices_by_field_id = HashMap::new();
                        let mut current_order = 0;

                        for field_id in
                            fields.field_declaration_order.iter().copied()
                        {
                            let field = &fields.fields[field_id];
                            let llvm_field_ty =
                                self.get_type(self.monomorphize_term(
                                    Model::from_default_type(
                                        field.r#type.clone(),
                                    ),
                                    &instantiation,
                                ));

                            llvm_field_types.push(llvm_field_ty);

                            if llvm_field_ty.is_basic() {
                                llvm_field_indices_by_field_id
                                    .insert(field_id, current_order);
                                current_order += 1;
                            }
                        }

                        let ty = if llvm_field_types.is_empty() {
                            LlvmType::Zst
                        } else {
                            LlvmType::Basic(
                                self.context()
                                    .struct_type(
                                        &llvm_field_types
                                            .iter()
                                            .copied()
                                            .filter_map(|x| x.into_basic().ok())
                                            .collect::<Vec<_>>(),
                                        false,
                                    )
                                    .into(),
                            )
                        };

                        self.type_map_mut().struct_sigantures.insert(
                            symbol,
                            ty.into_basic().ok().map(|x| LlvmStructSignature {
                                llvm_struct_type: x.into_struct_type(),
                                llvm_field_types,
                                llvm_field_indices_by_field_id,
                            }),
                        );

                        ty
                    }

                    SymbolKind::Enum => todo!(),

                    _ => panic!("unsupported symbol kind {symbol_kind:?}"),
                }
            }

            Type::Tuple(tuple) => {
                let mut elements = Vec::new();

                for element in tuple.elements {
                    assert!(
                        element.is_unpacked,
                        "unpacked tuple element found {element:?}"
                    );

                    if let LlvmType::Basic(llvm_ty) =
                        self.get_type(element.term)
                    {
                        elements.push(llvm_ty);
                    }
                }

                if elements.is_empty() {
                    LlvmType::Zst
                } else {
                    LlvmType::Basic(
                        self.context().struct_type(&elements, false).into(),
                    )
                }
            }

            Type::Phantom(_) => LlvmType::Zst,

            Type::MemberSymbol(_) | Type::TraitMember(_) => {
                panic!("unsupported type {ty:?}")
            }

            Type::FunctionSignature(function_signature) => {
                panic!("should not be here {function_signature:?}")
            }
        }
    }
}
