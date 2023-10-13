use crate::{
    constant,
    generic::deduction::Error,
    symbol::{
        ConstantParameterRef, GenericItemRef, LifetimeParameterRef, LocalConstantParameterRef,
        LocalLifetimeParameterRef, LocalSubstitution, LocalTraitConstantRef, LocalTraitTypeRef,
        LocalTypeParameterRef, StructRef, TraitConstantRef, TraitRef, TraitTypeRef,
        TypeParameterRef,
    },
    ty::{self, Lifetime},
};

#[test]
fn basic_type_deduction() {
    // type parameter - one to one mapping
    {
        let implements_ty = ty::Type::Parameter(TypeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalTypeParameterRef(0),
        });

        let trait_ty = ty::Type::Primitive(ty::Primitive::Int32);

        let deduction = super::dedcue_in_type(&dynements_ty, &trait_ty).unwrap();

        assert_eq!(
            deduction
                .substitution
                .type_substitutions
                .get(implements_ty.as_parameter().unwrap())
                .unwrap(),
            &trait_ty
        );
    }

    // trait associated type - one to one mapping
    {
        let implements_ty = ty::Type::TraitAssociated(ty::TraitAssociated {
            trait_type_ref: TraitTypeRef {
                trait_ref: TraitRef(0),
                local_ref: LocalTraitTypeRef(0),
            },
            trait_substitution: LocalSubstitution::default(),
            associated_substitution: LocalSubstitution::default(),
        });

        let trait_ty = ty::Type::Primitive(ty::Primitive::Int32);

        let deduction = super::dedcue_in_type(&dynements_ty, &trait_ty).unwrap();

        assert_eq!(
            deduction
                .trait_associated_type_deductions
                .get(implements_ty.as_trait_associated().unwrap())
                .unwrap(),
            &trait_ty
        );
    }
}

#[test]
fn basic_constant_deduction() {
    // constant parameter - one to one mapping
    {
        let constant_parameter_ref = ConstantParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalConstantParameterRef(0),
        };

        let implements_constant = constant::Constant::Parameter(constant_parameter_ref);
        let trait_constant = constant::Constant::Primitive(constant::Primitive::Int32(42));

        let deduction = super::deduce_in_constant(&dynements_constant, &trait_constant).unwrap();

        assert_eq!(
            deduction
                .substitution
                .constant_substitutions
                .get(&constant_parameter_ref)
                .unwrap(),
            &trait_constant
        );
    }

    // trait associated constant - one to one mapping
    {
        let implements_constant = constant::Constant::TraitAssociated(constant::TraitAssociated {
            trait_constant_ref: TraitConstantRef {
                trait_ref: TraitRef(0),
                local_ref: LocalTraitConstantRef(0),
            },
            trait_substitution: LocalSubstitution::default(),
        });

        let trait_constant = constant::Constant::Primitive(constant::Primitive::Int32(42));

        let deduction = super::deduce_in_constant(&dynements_constant, &trait_constant).unwrap();

        assert_eq!(
            deduction
                .trait_associated_constant_deductions
                .get(implements_constant.as_trait_associated().unwrap())
                .unwrap(),
            &trait_constant
        );
    }
}

#[test]
fn basic_unifying_type_deduction() {
    // simple struct mapping
    {
        let type_parameter = TypeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalTypeParameterRef(0),
        };

        let lieftime_parameter = LifetimeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalLifetimeParameterRef(0),
        };

        let implements_ty = ty::Type::Struct(ty::Struct {
            struct_ref: StructRef(0),
            substitution: LocalSubstitution {
                lifetimes: vec![Lifetime::Parameter(lieftime_parameter)],
                types: vec![ty::Type::Parameter(type_parameter)],
                constants: Vec::new(),
            },
        });

        let trait_ty = ty::Type::Struct(ty::Struct {
            struct_ref: StructRef(0),
            substitution: LocalSubstitution {
                lifetimes: vec![Lifetime::Static],
                types: vec![ty::Type::Primitive(ty::Primitive::Int32)],
                constants: vec![],
            },
        });

        let deduction = super::dedcue_in_type(&dynements_ty, &trait_ty).unwrap();

        assert_eq!(
            deduction
                .substitution
                .type_substitutions
                .get(&type_parameter)
                .unwrap(),
            &ty::Type::Primitive(ty::Primitive::Int32)
        );
        assert_eq!(
            deduction
                .substitution
                .lifetime_substitutions
                .get(&lieftime_parameter)
                .unwrap(),
            &Lifetime::Static
        );
    }
}

#[test]
fn reject_unifying_type_deduction() {
    // mismatched struct mapping
    {
        let type_parameter = TypeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalTypeParameterRef(0),
        };

        let lieftime_parameter = LifetimeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalLifetimeParameterRef(0),
        };

        let implements_ty = ty::Type::Struct(ty::Struct {
            struct_ref: StructRef(0),
            substitution: LocalSubstitution {
                lifetimes: vec![Lifetime::Parameter(lieftime_parameter)],
                types: vec![ty::Type::Parameter(type_parameter)],
                constants: Vec::new(),
            },
        });

        let trait_ty = ty::Type::Struct(ty::Struct {
            struct_ref: StructRef(1), // different struct
            substitution: LocalSubstitution {
                lifetimes: vec![Lifetime::Static],
                types: vec![ty::Type::Primitive(ty::Primitive::Int32)],
                constants: vec![],
            },
        });

        assert_eq!(
            super::dedcue_in_type(&dynements_ty, &trait_ty,).unwrap_err(),
            Error::NonUnifiable
        );
    }
}

#[test]
fn basic_tuple_type_unification() {
    // simple tuple mapping
    {
        let first_type_parameter = TypeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalTypeParameterRef(0),
        };
        let second_type_parameter = TypeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalTypeParameterRef(1),
        };

        let implements_ty = ty::Type::Tuple(ty::Tuple {
            elements: vec![
                ty::TupleElement::Regular(ty::Type::Parameter(first_type_parameter)),
                ty::TupleElement::Regular(ty::Type::Parameter(second_type_parameter)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int32)),
            ],
        });

        let trait_ty = ty::Type::Tuple(ty::Tuple {
            elements: vec![
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int8)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int16)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int32)),
            ],
        });

        let deduction = super::dedcue_in_type(&dynements_ty, &trait_ty).unwrap();

        assert_eq!(
            deduction
                .substitution
                .type_substitutions
                .get(&first_type_parameter)
                .unwrap(),
            &ty::Type::Primitive(ty::Primitive::Int8)
        );
        assert_eq!(
            deduction
                .substitution
                .type_substitutions
                .get(&second_type_parameter)
                .unwrap(),
            &ty::Type::Primitive(ty::Primitive::Int16)
        );
    }
}

#[test]
fn unpacked_tuple_type_unification() {
    // non-empty packed tuple
    {
        let unpacked_type_parmeter = TypeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalTypeParameterRef(0),
        };

        let implements_ty = ty::Type::Tuple(ty::Tuple {
            elements: vec![
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int8)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int16)),
                ty::TupleElement::Unpacked(ty::Unpacked::Parameter(unpacked_type_parmeter)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int32)),
            ],
        });

        let trait_ty = ty::Type::Tuple(ty::Tuple {
            elements: vec![
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int8)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int16)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Float32)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Float64)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int32)),
            ],
        });

        let deduction = super::dedcue_in_type(&dynements_ty, &trait_ty).unwrap();

        let expected_ty = ty::Type::Tuple(ty::Tuple {
            elements: vec![
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Float32)),
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Float64)),
            ],
        });

        assert_eq!(
            deduction
                .substitution
                .type_substitutions
                .get(&unpacked_type_parmeter)
                .unwrap(),
            &expected_ty
        );
    }

    // empty packed tuple
    {
        let unpacked_type_parmeter = TypeParameterRef {
            generic_item_ref: GenericItemRef::Struct(StructRef(0)),
            local_ref: LocalTypeParameterRef(0),
        };

        let implements_ty = ty::Type::Tuple(ty::Tuple {
            elements: vec![
                ty::TupleElement::Regular(ty::Type::Primitive(ty::Primitive::Int8)),
                ty::TupleElement::Unpacked(ty::Unpacked::Parameter(unpacked_type_parmeter)),
            ],
        });

        let trait_ty = ty::Type::Tuple(ty::Tuple {
            elements: vec![ty::TupleElement::Regular(ty::Type::Primitive(
                ty::Primitive::Int8,
            ))],
        });

        let deduction = super::dedcue_in_type(&dynements_ty, &trait_ty).unwrap();

        let expected_ty = ty::Type::Tuple(ty::Tuple { elements: vec![] });

        assert_eq!(
            deduction
                .substitution
                .type_substitutions
                .get(&unpacked_type_parmeter)
                .unwrap(),
            &expected_ty
        );
    }
}
