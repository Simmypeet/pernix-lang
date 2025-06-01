//! Contains the procedural macros for the `pernixc_query` crate.

use proc_macro::TokenStream;

/// Derives the `Key` trait for structs and enums to be used in the query
/// system.
///
/// This procedural macro automatically implements the `Key` trait, which allows
/// types to be used as keys in the query database for storing and retrieving
/// values.
///
/// # Required Attributes
///
/// ## `#[value(Type)]`
///
/// Specifies the value type associated with this key. This is a required
/// attribute that defines what type of data this key will store in the
/// database.
///
/// ```ignore
/// #[derive(Key)]
/// #[value(i32)]
/// struct MyKey(String);
/// ```
///
/// # Optional Attributes
///
/// ## `#[pernixc_query(path)]`
///
/// Specifies a custom path to the `pernixc_query` crate. This is useful when
/// the crate is renamed or accessed through a different path. If not specified,
/// defaults to `::pernixc_query`.
///
/// ```ignore
/// #[derive(Key)]
/// #[pernixc_query(crate)]
/// #[value(String)]
/// struct MyKey;
/// ```
///
/// ## `#[merge(function)]`
///
/// Specifies a custom merge function to use instead of the default
/// equality-based merge behavior. The function must have the signature:
/// `fn(old: &mut ValueType, new: ValueType) -> Result<(), String>`
///
/// ```ignore
/// fn custom_merge(old: &mut i32, new: i32) -> Result<(), String> {
///     *old += new;
///     Ok(())
/// }
///
/// #[derive(Key)]
/// #[value(i32)]
/// #[merge(custom_merge)]
/// struct AdditiveKey;
/// ```
///
/// # Generated Implementation
///
/// The macro generates an implementation of the `Key` trait with:
///
/// - `type Value = <specified_type>` - The associated value type from the
///   `#[value]` attribute
/// - `unique_type_name()` - A stable, unique identifier combining package name,
///   version, module path, and type name
/// - `merge_value()` - Either the default equality-based merge or a custom
///   function if `#[merge]` is specified
///
/// # Examples
///
/// ## Basic Usage
///
/// ```ignore
/// use pernixc_query::Key;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Hash, Key)]
/// #[value(String)]
/// struct UserName(u32);
///
/// // Can now be used as a key in the database
/// let key = UserName(123);
/// map.insert(&key, "Alice".to_string());
/// ```
///
/// ## With Custom Merge Function
///
/// ```ignore
/// fn sum_merge(old: &mut i32, new: i32) -> Result<(), String> {
///     *old += new;
///     Ok(())
/// }
///
/// #[derive(Debug, Clone, PartialEq, Eq, Hash, Key)]
/// #[value(i32)]
/// #[merge(sum_merge)]
/// struct Counter(String);
///
/// // Multiple calls will sum the values
/// map.insert(&Counter("total".to_string()), 10);
/// map.insert(&Counter("total".to_string()), 5); // Result: 15
/// ```
///
/// ## With Custom Crate Path
///
/// ```ignore
/// use my_query_lib as query;
///
/// #[derive(Key)]
/// #[pernixc_query(query)]
/// #[value(Vec<String>)]
/// struct Items;
/// ```
///
/// # Requirements
///
/// Types deriving `Key` must also implement or derive:
/// - `Clone` - For cloning key instances
/// - `Eq` + `PartialEq` - For key equality comparison
/// - `Hash` - For use as hash map keys
/// - `Send + Sync` - For thread safety
/// - `'static` - For type erasure support
///
/// The value type must implement:
/// - `Clone` - For value cloning
/// - `Default` - For default value creation
/// - `Eq` + `PartialEq` - For merge conflict detection
/// - `Send + Sync + 'static` - For thread safety and type erasure
/// - `Serialize + Deserialize` - For database serialization
#[proc_macro_derive(Key, attributes(value, pernixc_query, merge))]
#[allow(clippy::too_many_lines)]
pub fn derive_key(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident.clone();
    let mut generics = input.generics;

    let Some(value_attr) =
        input.attrs.iter().find(|attr| attr.path().is_ident("value"))
    else {
        return syn::Error::new_spanned(
            name,
            "missing `#[value(Type)]` attribute on key type",
        )
        .to_compile_error()
        .into();
    };

    let Ok(value_type) = value_attr.parse_args::<syn::Type>() else {
        return syn::Error::new_spanned(
            value_attr,
            "invalid `#[value(Type)]` attribute on key type",
        )
        .to_compile_error()
        .into();
    };

    let pernixc_query_crate: syn::Path = match input
        .attrs
        .iter()
        .find(|attr| attr.path().is_ident("pernixc_query"))
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
            syn::parse_quote!(::pernixc_query)
        }
    };

    let merge_fn: Option<syn::Path> =
        match input.attrs.iter().find(|attr| attr.path().is_ident("merge")) {
            Some(attr) => {
                let Ok(value) = attr.parse_args::<syn::Path>() else {
                    return syn::Error::new_spanned(
                        attr,
                        "invalid `#[merge]` attribute on key type",
                    )
                    .to_compile_error()
                    .into();
                };

                Some(value)
            }
            None => None,
        };

    let merge_fn = merge_fn.map(|x| {
        quote::quote! {
            fn merge_value(old: &mut Self::Value, new: Self::Value) -> Result<bool, ::std::string::String> {
                #x(old, new)
            }
        }
    });

    // should not have lifetime or constant parameters, only type parameters are allowed
    if let Some(lt_param) = generics.lifetimes().next() {
        return syn::Error::new_spanned(
            lt_param,
            "lifetime parameters are not allowed in key types",
        )
        .to_compile_error()
        .into();
    }
    if let Some(const_param) = generics.const_params().next() {
        return syn::Error::new_spanned(
            const_param,
            "constant parameters are not allowed in key types",
        )
        .to_compile_error()
        .into();
    }

    let stable_type_id_computation = if generics.params.is_empty() {
        quote::quote! {
            {
                let unique_type_name = concat!(
                    env!("CARGO_PKG_NAME"),
                    "@",
                    env!("CARGO_PKG_VERSION"),
                    "::",
                    module_path!(),
                    "::",
                    stringify!(#name)
                );
                #pernixc_query_crate::key::StableTypeID::from_unique_type_name(
                    unique_type_name
                )
            }
        }
    } else {
        for ty_param in generics.type_params_mut() {
            ty_param
                .bounds
                .push(syn::parse_quote!(#pernixc_query_crate::key::Key));
        }

        let type_params = generics.type_params().map(|x| &x.ident);

        quote::quote! {
            {
                let unique_type_name = concat!(
                    env!("CARGO_PKG_NAME"),
                    "@",
                    env!("CARGO_PKG_VERSION"),
                    "::",
                    module_path!(),
                    "::",
                    stringify!(#name),
                );
                let mut hash = #pernixc_query_crate::key::StableTypeID::from_unique_type_name(
                    unique_type_name
                );

                #(
                    hash = <#type_params as #pernixc_query_crate::key::Key>::STABLE_TYPE_ID
                        .combine(hash);
                )*

                hash
            }
        }
    };

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics #pernixc_query_crate::key::Key for #name #ty_generics #where_clause {
            type Value = #value_type;

            const STABLE_TYPE_ID: #pernixc_query_crate::key::StableTypeID
                = #stable_type_id_computation;

            #merge_fn
        }
    }.into()
}
