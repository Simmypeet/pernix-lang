//! Contains the function used for creating the [`Reflector`] instance.

use pernixc_storage::{
    serde::{MergerFn, Reflector},
    ArcTrait,
};

use super::{GlobalID, Table};
use crate::component::{
    Accessibility, Extern, Implemented, Implements, LocationSpan, Member, Name,
    Parent, PositiveTraitImplementation, SymbolKind, TraitImplementation,
};

impl Table {
    /// Gets the reflector instance that can be used to serialize the components
    /// of the table's storage.
    ///
    /// # Addition
    ///
    /// In the future, if new types of components are added, they should be
    /// added to the reflector instance as well.
    #[must_use]
    #[allow(clippy::too_many_lines)]
    pub fn input_reflector() -> Reflector<GlobalID, ArcTrait, String, String> {
        // FIXME: probably use macros to generate this code
        let mut reflector = Reflector::new();

        assert!(reflector
            .register_type::<Accessibility>("Accessibility".to_owned()));
        assert!(reflector.register_type::<Name>("Name".to_owned()));
        assert!(reflector.register_type::<Member>("Member".to_owned()));
        assert!(reflector.register_type::<Parent>("Parent".to_owned()));
        assert!(reflector.register_type::<Implements>("Implements".to_owned()));
        assert!(reflector.register_type::<SymbolKind>("SymbolKind".to_owned()));
        assert!(reflector.register_type::<Extern>("Extern".to_owned()));
        assert!(
            reflector.register_type::<LocationSpan>("LocationSpan".to_owned())
        );
        assert!(reflector.register_type_with_merger::<Implemented>(
            "Implemented".to_owned(),
            &((|a, b| {
                a.extend(b.0.into_iter());

                Ok(())
            }) as MergerFn<Implemented, String>)
        ));
        assert!(reflector.register_type::<TraitImplementation>(
            "TraitImplementation".to_owned()
        ));
        assert!(reflector.register_type::<PositiveTraitImplementation>(
            "PositiveTraitImplementation".to_owned()
        ));

        reflector
    }
}
