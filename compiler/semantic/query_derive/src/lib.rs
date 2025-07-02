//! Contains the procedural macros for the `pernixc_query` crate.

use proc_macro::TokenStream;
use syn::{parenthesized, parse_quote};

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
/// - `Eq` + `PartialEq` - For merge conflict detection
/// - `Send + Sync + 'static` - For thread safety and type erasure
/// - `Serialize + Deserialize` - For database serialization
#[proc_macro_derive(Key, attributes(value, pernixc_query, scc_value))]
#[allow(clippy::too_many_lines)]
pub fn derive_key(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident.clone();
    let generics = input.generics;

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

    let scc_value_expr: Option<syn::Expr> =
        match input.attrs.iter().find(|attr| attr.path().is_ident("scc_value"))
        {
            Some(attr) => {
                let Ok(value) = attr.parse_args::<syn::Expr>() else {
                    return syn::Error::new_spanned(
                        attr,
                        "invalid `#[scc_value]` attribute on key type",
                    )
                    .to_compile_error()
                    .into();
                };

                Some(value)
            }
            None => None,
        };

    let scc_value_fn = scc_value_expr.map(|x| {
        quote::quote! {
            fn scc_value() -> Self::Value {
                #x
            }
        }
    });

    let identifiable_path: syn::Path =
        syn::parse_quote!(#pernixc_query_crate::Identifiable);

    let impl_identifiable =
        pernixc_identifiable_derive_lib::implements_identifiable(
            &name,
            generics.clone(),
            Some(&identifiable_path),
        );

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics #pernixc_query_crate::key::Key for #name #ty_generics #where_clause {
            type Value = #value_type;

            #scc_value_fn
        }

        #impl_identifiable
    }.into()
}

#[proc_macro_derive(Value, attributes(id, ext, value, key))]
#[allow(clippy::too_many_lines, clippy::redundant_clone)]
pub fn derive_value(input: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::DeriveInput);
    let name = input.ident.clone();
    let generics = input.generics;

    if !generics.params.is_empty() {
        return syn::Error::new_spanned(
            name,
            "value types cannot have generic parameters",
        )
        .to_compile_error()
        .into();
    }

    let Some(id_attr) =
        input.attrs.iter().find(|attr| attr.path().is_ident("id"))
    else {
        return syn::Error::new_spanned(
            name,
            "missing `#[id(Type)]` attribute on value type",
        )
        .to_compile_error()
        .into();
    };

    let Ok(id_type) = id_attr.parse_args::<syn::Type>() else {
        return syn::Error::new_spanned(
            id_attr,
            "invalid `#[id(Type)]` attribute on value type",
        )
        .to_compile_error()
        .into();
    };

    let key_ident: syn::Ident =
        match input.attrs.iter().find(|attr| attr.path().is_ident("key")) {
            Some(ident) => {
                let Ok(ident) = ident.parse_args::<syn::Ident>() else {
                    return syn::Error::new_spanned(
                        ident,
                        "`#[key(Ident)]` attribute must be a valid identifier",
                    )
                    .to_compile_error()
                    .into();
                };
                ident
            }
            None => {
                parse_quote!(Key)
            }
        };

    let value_type: syn::Type =
        match input.attrs.iter().find(|attr| attr.path().is_ident("value")) {
            Some(syn) => {
                let Ok(value) = syn.parse_args::<syn::Type>() else {
                    return syn::Error::new_spanned(
                        syn,
                        "invalid `#[value(Type)]` attribute on value type",
                    )
                    .to_compile_error()
                    .into();
                };
                value
            }
            None => parse_quote!(#name),
        };

    let ext = match input.attrs.iter().find(|attr| attr.path().is_ident("ext"))
    {
        Some(attr) => {
            let mut method = None;
            let mut trait_name = None;
            let mut dot_unwrap = None;

            match attr.parse_nested_meta(|meta| {
                // required `method(name)` argument
                if meta.path.is_ident("method") {
                    let ident;
                    parenthesized!(ident in meta.input);

                    let Ok(ident) = ident.parse::<syn::Ident>() else {
                        return Err(syn::Error::new(
                            ident.span(),
                            "`method` must be a valid identifier",
                        ));
                    };

                    method = Some(ident);
                    return Ok(());
                }

                // optional `trait(name)` argument
                if meta.path.is_ident("trait") {
                    let ident;
                    parenthesized!(ident in meta.input);

                    let Ok(ident) = ident.parse::<syn::Ident>() else {
                        return Err(syn::Error::new(
                            ident.span(),
                            "`trait` must be a valid identifier",
                        ));
                    };

                    trait_name = Some(ident);
                    return Ok(());
                }

                // optional `unwrap("msg")` argument
                if meta.path.is_ident("unwrap") {
                    let msg;
                    parenthesized!(msg in meta.input);

                    let Ok(msg) = msg.parse::<syn::LitStr>() else {
                        return Err(syn::Error::new(
                            msg.span(),
                            "`unwrap` must be a string literal",
                        ));
                    };

                    dot_unwrap = Some(quote::quote! {
                        .expect(#msg)
                    });
                    return Ok(());
                }

                Err(syn::Error::new_spanned(
                    meta.path,
                    "expected `method(name)`, `trait(name)`, or \
                     `unwrap(\"msg\")`",
                ))
            }) {
                Ok(()) => {
                    let Some(method) = method else {
                        return syn::Error::new_spanned(
                            attr,
                            "missing required `method(name)` argument",
                        )
                        .to_compile_error()
                        .into();
                    };

                    let trait_name =
                        trait_name.unwrap_or_else(|| parse_quote!(Ext));

                    let return_type: syn::Type = if dot_unwrap.is_some() {
                        value_type.clone()
                    } else {
                        parse_quote!(
                            ::pernixc_query::__internal::Result<
                                #value_type,
                                ::pernixc_query::runtime::executor::CyclicError
                            >
                        )
                    };

                    Some(quote::quote! {
                        #[doc = concat!(
                            "An extension trait for the `",
                            stringify!(#name),
                            "` value type.\n\n",
                            "This trait provides additional methods for retrieving values from the query database."
                        )]
                        pub trait #trait_name {
                            #[doc = concat!(
                                "Retrieves a value of type `",
                                stringify!(#value_type),
                                "` from the query database using the given ID.\n\n",
                                "This method is automatically implemented for the `",
                                stringify!(#name),
                                "` value type."
                            )]
                            fn #method(
                                &self,
                                id: #id_type
                            ) -> #return_type;
                        }

                        impl #trait_name for ::pernixc_query::TrackedEngine<'_> {
                            fn #method(
                                &self,
                                id: #id_type
                            ) -> #return_type {
                                self.query(
                                    &#key_ident(id)
                                )#dot_unwrap
                            }
                        }
                    })
                }
                Err(error) => {
                    return error.to_compile_error().into();
                }
            }
        }

        None => None,
    };

    let key_struct = quote::quote! {
        #[doc = concat!(
            "A key type for the `",
            stringify!(#name),
            "` value type. This is used to uniquely identify values in the query database.\n\n",
            "This key is derived from the `#[id(Type)]` attribute, which specifies the type of the ID used to identify this value."
        )]
        #[derive(
            Debug,
            Clone,
            Copy,
            PartialEq,
            Eq,
            PartialOrd,
            Ord,
            Hash,
            ::pernixc_query::__internal::Key,
            ::pernixc_query::__internal::Serialize,
            ::pernixc_query::__internal::Deserialize,
            ::pernixc_query::__internal::StableHash,
        )]
        #[value(#value_type)]
        pub struct #key_ident(pub #id_type);
    };

    quote::quote! {
        #key_struct
        #ext
    }
    .into()
}
