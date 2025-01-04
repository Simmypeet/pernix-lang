//! Contains the function used for creating the [`Reflector`] instance.

use pernixc_component::serde::Reflector;

use super::{GlobalID, Table};
use crate::component::{
    Accessibility, Extern, Implemented, Implements, Member, Name, Parent,
    SymbolKind,
};

impl Table {
    /// Gets the reflector instance that can be used to serialize the components
    /// of the table's storage.
    ///
    /// # Addition
    ///
    /// In the future, if new types of components are added, they should be
    /// added to the reflector instance as well.
    pub fn reflector() -> Reflector<String, GlobalID> {
        // FIXME: probably use macros to generate this code
        let mut reflector = Reflector::new();

        assert!(reflector
            .register_type::<Accessibility>("Accessibility".to_string()));
        assert!(reflector.register_type::<Name>("Name".to_string()));
        assert!(reflector.register_type::<Member>("Member".to_string()));
        assert!(reflector.register_type::<Parent>("Parent".to_string()));
        assert!(reflector.register_type::<Implements>("Implements".to_string()));
        assert!(reflector.register_type::<SymbolKind>("SymbolKind".to_string()));
        assert!(reflector.register_type::<Extern>("Extern".to_string()));
        assert!(
            reflector.register_type::<Implemented>("Implemented".to_string())
        );

        reflector
    }
}
