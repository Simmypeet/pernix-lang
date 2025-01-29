//! Contains the extension trait for getting the overall accessibility of a
//! term.

use pernixc_table::{
    component::Accessibility, GlobalAccessibility, GlobalID,
    MergeAccessibilityError, Table,
};

use crate::{
    constant::Constant, generic_arguments::GenericArguments,
    lifetime::Lifetime, r#type::Type, Default,
};

#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, thiserror::Error,
)]
#[allow(missing_docs)]
pub enum GetTermAccessibilityError {
    #[error("the term contains an invalid ID")]
    InvalidID,

    #[error(
        "the term contains two or more accessibilities that are scoped to \
         different modules"
    )]
    Unrealated,
}

/// An extension trait on the table for retrieving the overall accessibility of
/// a term.
pub trait Ext {
    /// Gets overall accessibility of the given [`r#type::Type`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    fn get_type_accessibility(
        &self,
        ty: &Type<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError>;

    /// Gets overall accessibility of the given [`constant::Constant`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    fn get_constant_accessibility(
        &self,
        constant: &Constant<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError>;

    /// Gets overall accessibility of the given [`Lifetime`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    fn get_lifetime_accessibility(
        &self,
        lifetime: &Lifetime<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError>;

    /// Gets overall accessibility of the given [`GenericArguments`].
    ///
    /// # Errors
    ///
    /// See [`GetTermAccessibilityError`] for more information.
    fn get_generic_arguments_accessibility(
        &self,
        generic_arguments: &GenericArguments<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError>;
}

impl From<MergeAccessibilityError> for GetTermAccessibilityError {
    fn from(value: MergeAccessibilityError) -> Self {
        match value {
            MergeAccessibilityError::InvalidModuleID => Self::InvalidID,
            MergeAccessibilityError::Unrelated => Self::Unrealated,
        }
    }
}

impl Ext for Table {
    #[allow(clippy::too_many_lines)]
    fn get_type_accessibility(
        &self,
        ty: &Type<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError> {
        match ty {
            Type::Inference(never) => match *never {},

            Type::MemberSymbol(member_symbol) => {
                let symbol_accessibility = match self
                    .get_accessibility(member_symbol.id)
                {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(member_symbol.id.target_id, id),
                    ),
                };

                let member_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.member_generic_arguments,
                    )?;

                let parent_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.parent_generic_arguments,
                    )?;

                let generic_arguments_accessibility = self
                    .merge_accessibility_down(
                        member_generic_accessibility,
                        parent_generic_accessibility,
                    )?;

                Ok(self.merge_accessibility_down(
                    symbol_accessibility,
                    generic_arguments_accessibility,
                )?)
            }

            Type::Symbol(symbol) => {
                let symbol_accessibility = match self
                    .get_accessibility(symbol.id)
                {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(symbol.id.target_id, id),
                    ),
                };
                let generic_arguments_accessibility = self
                    .get_generic_arguments_accessibility(
                        &symbol.generic_arguments,
                    )?;

                Ok(self.merge_accessibility_down(
                    symbol_accessibility,
                    generic_arguments_accessibility,
                )?)
            }

            Type::Pointer(pointer) => {
                self.get_type_accessibility(&pointer.pointee)
            }

            Type::Reference(reference) => {
                let lt_accessibility =
                    self.get_lifetime_accessibility(&reference.lifetime)?;
                let ty_accessibility =
                    self.get_type_accessibility(&reference.pointee)?;

                Ok(self.merge_accessibility_down(
                    lt_accessibility,
                    ty_accessibility,
                )?)
            }

            Type::Array(array) => {
                let ty_accessibility =
                    self.get_type_accessibility(&array.r#type)?;
                let length_accessibility =
                    self.get_constant_accessibility(&array.length)?;

                Ok(self.merge_accessibility_down(
                    ty_accessibility,
                    length_accessibility,
                )?)
            }

            Type::Parameter(parameter) => match self
                .get_accessibility(parameter.parent)
            {
                Accessibility::Public => Ok(GlobalAccessibility::Public),
                Accessibility::Scoped(id) => Ok(GlobalAccessibility::Scoped(
                    GlobalID::new(parameter.parent.target_id, id),
                )),
            },

            Type::Error(_) | Type::Primitive(_) => {
                Ok(GlobalAccessibility::Public)
            }

            Type::Tuple(tuple) => {
                let mut current_min = GlobalAccessibility::Public;

                for element in &tuple.elements {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_type_accessibility(&element.term)?,
                    )?;
                }

                Ok(current_min)
            }

            Type::TraitMember(member_symbol) => {
                let symbol_accessibility = match self
                    .get_accessibility(member_symbol.id)
                {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(member_symbol.id.target_id, id),
                    ),
                };

                let member_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.member_generic_arguments,
                    )?;

                let parent_generic_accessibility = self
                    .get_generic_arguments_accessibility(
                        &member_symbol.parent_generic_arguments,
                    )?;

                let generic_arguments_accessibility = self
                    .merge_accessibility_down(
                        member_generic_accessibility,
                        parent_generic_accessibility,
                    )?;

                Ok(self.merge_accessibility_down(
                    symbol_accessibility,
                    generic_arguments_accessibility,
                )?)
            }

            Type::Phantom(phantom) => self.get_type_accessibility(&phantom.0),
        }
    }

    fn get_constant_accessibility(
        &self,
        constant: &Constant<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError> {
        match constant {
            Constant::Inference(never) => match *never {},

            Constant::Struct(constant) => {
                let mut current_min = match self.get_accessibility(constant.id)
                {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(constant.id.target_id, id),
                    ),
                };

                for field in &constant.fields {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(field)?,
                    )?;
                }

                Ok(current_min)
            }

            Constant::Array(constant) => {
                let mut current_min = GlobalAccessibility::Public;

                for element in &constant.elements {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(element)?,
                    )?;
                }

                Ok(current_min)
            }

            Constant::Parameter(param) => {
                Ok(match self.get_accessibility(param.parent) {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(param.parent.target_id, id),
                    ),
                })
            }

            Constant::Error(_) | Constant::Phantom | Constant::Primitive(_) => {
                Ok(GlobalAccessibility::Public)
            }

            Constant::Enum(constant) => {
                let mut current_min = match self
                    .get_accessibility(constant.variant_id)
                {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(constant.variant_id.target_id, id),
                    ),
                };

                if let Some(associated_value) = &constant.associated_value {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(associated_value)?,
                    )?;
                }

                Ok(current_min)
            }

            Constant::Tuple(tuple) => {
                let mut current_min = GlobalAccessibility::Public;

                for element in &tuple.elements {
                    current_min = self.merge_accessibility_down(
                        current_min,
                        self.get_constant_accessibility(&element.term)?,
                    )?;
                }

                Ok(current_min)
            }
        }
    }

    fn get_lifetime_accessibility(
        &self,
        lifetime: &Lifetime<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError> {
        match lifetime {
            Lifetime::Parameter(lifetime_parameter_id) => {
                Ok(match self.get_accessibility(lifetime_parameter_id.parent) {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => {
                        GlobalAccessibility::Scoped(GlobalID::new(
                            lifetime_parameter_id.parent.target_id,
                            id,
                        ))
                    }
                })
            }

            Lifetime::Elided(elided_lifetime_id) => {
                Ok(match self.get_accessibility(elided_lifetime_id.parent) {
                    Accessibility::Public => GlobalAccessibility::Public,
                    Accessibility::Scoped(id) => GlobalAccessibility::Scoped(
                        GlobalID::new(elided_lifetime_id.parent.target_id, id),
                    ),
                })
            }

            Lifetime::Inference(never) => match *never {},

            Lifetime::Error(_) | Lifetime::Forall(_) | Lifetime::Static => {
                Ok(GlobalAccessibility::Public)
            }
        }
    }

    fn get_generic_arguments_accessibility(
        &self,
        generic_arguments: &GenericArguments<Default>,
    ) -> Result<GlobalAccessibility, GetTermAccessibilityError> {
        let mut current_min = GlobalAccessibility::Public;

        for lifetime in &generic_arguments.lifetimes {
            current_min = self.merge_accessibility_down(
                current_min,
                self.get_lifetime_accessibility(lifetime)?,
            )?;
        }

        for ty in &generic_arguments.types {
            current_min = self.merge_accessibility_down(
                current_min,
                self.get_type_accessibility(ty)?,
            )?;
        }

        for constant in &generic_arguments.constants {
            current_min = self.merge_accessibility_down(
                current_min,
                self.get_constant_accessibility(constant)?,
            )?;
        }

        Ok(current_min)
    }
}
