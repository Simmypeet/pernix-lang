//! Procedural macros for the `pernixc_stable_type_id` crate.
//!
//! This crate provides the `#[derive(Identifiable)]` macro that automatically
//! implements the [`pernixc_stable_type_id::Identifiable`] trait for types.
//!
//! # Overview
//!
//! The derive macro generates stable, collision-resistant type identifiers
//! that remain consistent across different compiler runs. This is essential
//! for incremental compilation systems and other scenarios where type identity
//! must be preserved between executions.
//!
//! # Usage
//!
//! ```rust
//! use pernixc_stable_type_id::Identifiable;
//!
//! #[derive(Identifiable)]
//! struct MyType {
//!     field: i32,
//! }
//!
//! #[derive(Identifiable)]
//! enum MyEnum {
//!     Variant1,
//!     Variant2(String),
//! }
//!
//! #[derive(Identifiable)]
//! struct GenericType<T: Identifiable, U: Identifiable> {
//!     first: T,
//!     second: U,
//! }
//! ```
//!
//! # ID Generation Strategy
//!
//! The generated stable type ID is computed based on:
//!
//! 1. **Package Information**: Package name and version from `Cargo.toml`
//! 2. **Module Path**: The full module path where the type is defined
//! 3. **Type Name**: The exact name of the type
//! 4. **Generic Parameters**: For generic types, the IDs of concrete type
//!    arguments
//!
//! This ensures that:
//! - Different types have different IDs (collision resistance)
//! - The same type has the same ID across runs (stability)
//! - Generic instantiations are distinguished (e.g., `Vec<i32>` vs
//!   `Vec<String>`)
//!
//! # Restrictions
//!
//! The derive macro has the following limitations:
//!
//! - **No lifetime parameters**: Types with lifetime parameters cannot derive
//!   `Identifiable`
//! - **No const parameters**: Types with const generic parameters are not
//!   supported
//! - **Bounded type parameters**: All type parameters must implement
//!   `Identifiable`
//!
//! # Examples
//!
//! ## Simple Types
//!
//! ```rust
//! # use pernixc_stable_type_id::Identifiable;
//! #[derive(Identifiable)]
//! struct Point {
//!     x: f64,
//!     y: f64,
//! }
//!
//! // The ID is automatically computed and available as a constant
//! let id = Point::STABLE_TYPE_ID;
//! ```
//!
//! ## Generic Types
//!
//! ```rust
//! # use pernixc_stable_type_id::Identifiable;
//! #[derive(Identifiable)]
//! struct Container<T: Identifiable> {
//!     value: T,
//! }
//!
//! // Different instantiations have different IDs
//! type IntContainer = Container<i32>;
//! type StringContainer = Container<String>;
//! // IntContainer::STABLE_TYPE_ID != StringContainer::STABLE_TYPE_ID
//! ```
//!
//! ## Error Cases
//!
//! ```compile_fail
//! # use pernixc_stable_type_id::Identifiable;
//! // This will fail to compile - lifetime parameters not allowed
//! #[derive(Identifiable)]
//! struct WithLifetime<'a> {
//!     data: &'a str,
//! }
//! ```
//!
//! ```compile_fail
//! # use pernixc_stable_type_id::Identifiable;
//! // This will fail to compile - const parameters not allowed
//! #[derive(Identifiable)]
//! struct WithConst<const N: usize> {
//!     data: [i32; N],
//! }
//! ```

use proc_macro::TokenStream;

/// Derives the [`Identifiable`](pernixc_stable_type_id::Identifiable) trait for
/// a type.
///
/// This proc macro automatically implements the `Identifiable` trait, providing
/// a stable, collision-resistant type identifier that remains consistent across
/// different compiler runs.
///
/// # Generated Implementation
///
/// The macro generates an implementation that computes the stable type ID
/// using:
///
/// 1. **Base ID**: Computed from package name, version, module path, and type
///    name
/// 2. **Generic Combination**: For generic types, combines the base ID with the
///    stable IDs of all concrete type parameters
///
/// # Type Requirements
///
/// ## Allowed Type Parameters
/// - **Type parameters**: Must implement `Identifiable` (automatically
///   enforced)
///
/// ## Forbidden Parameters
/// - **Lifetime parameters**: Not supported and will cause a compile error
/// - **Const parameters**: Not supported and will cause a compile error
///
/// # Examples
///
/// ## Simple Struct
///
/// ```rust
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct User {
///     id: u64,
///     name: String,
/// }
///
/// // Generated implementation equivalent to:
/// // impl Identifiable for User {
/// //     const STABLE_TYPE_ID: StableTypeID = /* computed at compile time */;
/// // }
/// ```
///
/// ## Generic Struct
///
/// ```rust
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct Pair<T: Identifiable, U: Identifiable> {
///     first: T,
///     second: U,
/// }
///
/// // The generated ID varies based on concrete type parameters:
/// // Pair<i32, String>::STABLE_TYPE_ID != Pair<f64, Vec<u8>>::STABLE_TYPE_ID
/// ```
///
/// ## Enum Types
///
/// ```rust
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// enum Result<T: Identifiable, E: Identifiable> {
///     Ok(T),
///     Err(E),
/// }
/// ```
///
/// # Error Cases
///
/// The following will produce compile-time errors:
///
/// ```compile_fail
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct BadLifetime<'a> {  // ❌ Lifetime parameters not allowed
///     data: &'a str,
/// }
/// ```
///
/// ```compile_fail
/// # use pernixc_stable_type_id::Identifiable;
/// #[derive(Identifiable)]
/// struct BadConst<const N: usize> {  // ❌ Const parameters not allowed
///     buffer: [u8; N],
/// }
/// ```
///
/// # Implementation Details
///
/// For non-generic types, the stable ID is computed at compile time using:
/// ```text
/// StableTypeID::from_unique_type_name(
///     "package_name@version::module::path::TypeName"
/// )
/// ```
///
/// For generic types, the macro additionally combines the base ID with each
/// type parameter's stable ID:
/// ```text
/// base_id.combine(T::STABLE_TYPE_ID).combine(U::STABLE_TYPE_ID)...
/// ```
///
/// This ensures that `Vec<i32>` and `Vec<String>` have different stable IDs
/// while maintaining deterministic generation.
///
/// # Performance
///
/// The stable ID computation happens entirely at compile time and has zero
/// runtime cost. The generated constant can be used directly without any
/// function calls or allocations.
#[proc_macro_derive(Identifiable, attributes(pernixc_stable_type_id))]
#[allow(clippy::too_many_lines)]
pub fn derive_identifiable(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident.clone();
    let generics = input.generics;

    let pernixc_stable_type_id_crate: syn::Path = match input
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("pernixc_stable_type_id"))
    {
        Some(attr) => {
            let Ok(value) = attr.parse_args::<syn::Path>() else {
                return syn::Error::new_spanned(
                    attr,
                    "invalid `#[pernixc_query]` attribute on key type",
                )
                .to_compile_error()
                .into();
            };

            value
        }
        None => {
            syn::parse_quote!(::pernixc_stable_type_id)
        }
    };

    let identifiable_path =
        syn::parse_quote!(#pernixc_stable_type_id_crate::Identifiable);
    let stable_type_id =
        syn::parse_quote!(#pernixc_stable_type_id_crate::StableTypeID);

    pernixc_identifiable_derive_lib::implements_identifiable(
        &name,
        generics,
        Some(&identifiable_path),
        Some(&stable_type_id),
    )
    .into()
}
