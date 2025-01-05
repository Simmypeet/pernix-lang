//! Contains the function used for creating the [`Reflector`] instance.

use pernixc_component::serde::{MergerFn, Reflector};

use super::{GlobalID, Table};
use crate::component::{
    Accessibility, ConstTraitImplementation, Extern, FinalImplementation,
    Implemented, Implements, Member, Name, Parent, SymbolKind,
};

impl Table {
    /// Gets the reflector instance that can be used to serialize the components
    /// of the table's storage.
    ///
    /// # Addition
    ///
    /// In the future, if new types of components are added, they should be
    /// added to the reflector instance as well.
    pub fn reflector() -> Reflector<String, GlobalID, String> {
        // FIXME: probably use macros to generate this code
        let mut reflector = Reflector::new();

        assert!(reflector.register_type_with_merger::<Accessibility>(
            "Accessibility".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Incompatible accessibility"))
                }
            }) as MergerFn<Accessibility, String>)
        ));
        assert!(reflector.register_type_with_merger::<Name>(
            "Name".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Name mismatch: {} != {}", a.0, b.0))
                }
            }) as MergerFn<Name, String>)
        ));
        assert!(reflector.register_type_with_merger::<Member>(
            "Member".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Incompatible Member"))
                }
            }) as MergerFn<Member, String>)
        ));
        assert!(reflector.register_type_with_merger::<Parent>(
            "Parent".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Incompatible Parent"))
                }
            }) as MergerFn<Parent, String>)
        ));
        assert!(reflector.register_type_with_merger::<Implements>(
            "Implements".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Incompatible Implements"))
                }
            }) as MergerFn<Implements, String>)
        ));
        assert!(reflector.register_type_with_merger::<SymbolKind>(
            "SymbolKind".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Incompatible SymbolKind"))
                }
            }) as MergerFn<SymbolKind, String>)
        ));
        assert!(reflector.register_type_with_merger::<Extern>(
            "Extern".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Incompatible Extern"))
                }
            }) as MergerFn<Extern, String>)
        ));
        assert!(reflector.register_type_with_merger::<Implemented>(
            "Implemented".to_string(),
            &((|a, b| {
                a.extend(b.0.into_iter());

                Ok(())
            }) as MergerFn<Implemented, String>)
        ));
        assert!(reflector.register_type_with_merger::<FinalImplementation>(
            "FinalImplementation".to_string(),
            &((|a, b| {
                if *a == b {
                    Ok(())
                } else {
                    Err(format!("Incompatible FinalImplementation"))
                }
            }) as MergerFn<FinalImplementation, String>)
        ));
        assert!(reflector
            .register_type_with_merger::<ConstTraitImplementation>(
                "ConstTraitImplementation".to_string(),
                &((|a, b| {
                    if *a == b {
                        Ok(())
                    } else {
                        Err(format!("Incompatible ConstTraitImplementation"))
                    }
                })
                    as MergerFn<ConstTraitImplementation, String>)
            ));

        reflector
    }
}
