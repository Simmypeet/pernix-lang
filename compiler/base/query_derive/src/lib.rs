//! Contains the procedural macros for the `pernixc_query` crate.

use proc_macro::TokenStream;
use syn::{
    parenthesized, parse_quote, Attribute, Data, DataStruct, Fields, Meta,
};

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
#[proc_macro_derive(
    Key,
    attributes(value, pernixc_query, scc_value, extend, always_reverify)
)]
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

    let extend = if input.attrs.iter().any(|x| x.path().is_ident("extend")) {
        let Data::Struct(st) = &input.data else {
            return syn::Error::new_spanned(
                name,
                "the `#[extend]` attribute can only be used on structs",
            )
            .to_compile_error()
            .into();
        };

        // must be a tuple struct
        let DataStruct { fields: Fields::Unnamed(unnamed_files), .. } = st
        else {
            return syn::Error::new_spanned(
                name,
                "the `#[extend]` attribute can only be used on tuple structs",
            )
            .to_compile_error()
            .into();
        };

        // must have exactly one unnamed field
        if unnamed_files.unnamed.len() != 1 {
            return syn::Error::new_spanned(
                name,
                "the `#[extend]` attribute can only be used on tuple structs \
                 with exactly one unnamed field",
            )
            .to_compile_error()
            .into();
        }

        let field_type = &unnamed_files.unnamed[0].ty;

        get_ext(input.attrs.iter(), &value_type, field_type, &input.ident)
    } else {
        None
    };

    let re_verify_attr =
        input.attrs.iter().find(|attr| attr.path().is_ident("always_reverify"));

    if let Some(attr @ Attribute { meta, .. }) = re_verify_attr {
        // only plain ident is allowed
        if !matches!(meta, Meta::Path(_)) {
            return syn::Error::new_spanned(
                attr,
                "`#[always_reverify]` attribute must be a plain identifier",
            )
            .into_compile_error()
            .into();
        }
    }

    let re_verify = re_verify_attr.is_some().then(|| {
        quote::quote! {
            const ALWAYS_REVERIFY: bool = true;
        }
    });

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
        syn::parse_quote!(#pernixc_query_crate::__internal::Identifiable);

    let stable_type_id: syn::Path =
        syn::parse_quote!(#pernixc_query_crate::__internal::StableTypeID);

    let impl_identifiable =
        pernixc_identifiable_derive_lib::implements_identifiable(
            &name,
            generics.clone(),
            Some(&identifiable_path),
            Some(&stable_type_id),
        );

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    quote::quote! {
        impl #impl_generics #pernixc_query_crate::key::Key for #name #ty_generics #where_clause {
            #re_verify

            type Value = #value_type;

            #scc_value_fn
        }

        #impl_identifiable

        #extend
    }.into()
}

/// Derives a value type for the query system with automatic key generation and
/// optional extension traits.
///
/// This procedural macro simplifies the creation of value types for the query
/// database by automatically generating a corresponding key type and optionally
/// creating extension traits for convenient value retrieval.
///
/// # Required Attributes
///
/// ## `#[id(Type)]`
///
/// Specifies the identifier type used to uniquely identify values of this type
/// in the query database. This becomes the inner type of the generated key
/// struct.
///
/// ```ignore
/// #[derive(Value)]
/// #[id(u32)]
/// struct User {
///     name: String,
///     email: String,
/// }
/// // Generates: struct Key(pub u32);
/// ```
///
/// # Optional Attributes
///
/// ## `#[key(Name)]`
///
/// Specifies a custom name for the generated key struct. If not provided,
/// defaults to `Key`.
///
/// ```ignore
/// #[derive(Value)]
/// #[id(String)]
/// #[key(UserKey)]
/// struct User {
///     name: String,
/// }
/// // Generates: struct UserKey(pub String);
/// ```
///
/// ## `#[value(Type)]`
///
/// Specifies the actual value type stored in the database. If not provided,
/// defaults to the type being derived. This is useful when you want the key
/// to reference a different type than the struct itself.
///
/// ```ignore
/// #[derive(Value)]
/// #[id(u32)]
/// #[value(UserData)]
/// struct User;
/// // The key will store values of type UserData instead of User
/// ```
///
/// ## `#[extend(...)]`
///
/// Generates an extension trait for convenient value retrieval from the query
/// engine. This attribute accepts several sub-attributes:
///
/// ### Required: `method(name)`
///
/// Specifies the name of the method to generate in the extension trait.
///
/// ### Optional: `trait(name)`
///
/// Specifies the name of the extension trait. Defaults to `Ext`.
///
/// ### Optional: `unwrap("message")`
///
/// If provided, the generated method will unwrap the result and panic with the
/// given message on cyclic dependency errors. Without this, the method returns
/// a `Result` type.
///
/// ```ignore
/// #[derive(Value)]
/// #[id(u32)]
/// #[extend(method(get_user), trait(UserExt), unwrap("Failed to get user"))]
/// struct User {
///     name: String,
/// }
///
/// // Generates:
/// // pub trait UserExt {
/// //     fn get_user(&self, id: u32) -> User;
/// // }
/// //
/// // impl UserExt for TrackedEngine<'_> {
/// //     fn get_user(&self, id: u32) -> User {
/// //         self.query(&Key(id)).expect("Failed to get user")
/// //     }
/// // }
/// ```
///
/// # Generated Output
///
/// The macro generates:
///
/// 1. **Key Struct**: A newtype wrapper around the ID type that implements all
///    necessary traits for use as a query key, including `Key`, `Hash`,
///    `Serialize`, `Deserialize`, and `StableHash`.
///
/// 2. **Extension Trait** (if `#[extend]` is provided): A trait with methods
///    for convenient value retrieval, implemented for `TrackedEngine`.
///
/// # Examples
///
/// ## Basic Usage
///
/// ```ignore
/// use pernixc_query::Value;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Value)]
/// #[id(u32)]
/// struct User {
///     name: String,
///     email: String,
/// }
///
/// // Generated:
/// // pub struct Key(pub u32);
/// // Key implements all necessary traits for query database usage
/// ```
///
/// ## With Custom Key Name
///
/// ```ignore
/// #[derive(Value)]
/// #[id(String)]
/// #[key(UserKey)]
/// struct User {
///     name: String,
/// }
///
/// // Generated:
/// // pub struct UserKey(pub String);
/// ```
///
/// ## With Extension Trait
///
/// ```ignore
/// #[derive(Value)]
/// #[id(u32)]
/// #[extend(method(get_user))]
/// struct User {
///     name: String,
/// }
///
/// // Generated:
/// // pub struct Key(pub u32);
/// // pub trait Ext {
/// //     fn get_user(&self, id: u32) -> Result<User, CyclicError>;
/// // }
/// // impl Ext for TrackedEngine<'_> { ... }
///
/// // Usage:
/// // let user = engine.get_user(123)?;
/// ```
///
/// ## With Extension Trait and Unwrap
///
/// ```ignore
/// #[derive(Value)]
/// #[id(u32)]
/// #[extend(method(get_user), trait(UserExt), unwrap("User not found"))]
/// struct User {
///     name: String,
/// }
///
/// // Generated method panics instead of returning Result:
/// // fn get_user(&self, id: u32) -> User { ... }
/// ```
///
/// ## With Different Value Type
///
/// ```ignore
/// #[derive(Value)]
/// #[id(u32)]
/// #[value(UserData)]
/// struct UserKey;
///
/// struct UserData {
///     name: String,
///     email: String,
/// }
///
/// // The key will be associated with UserData values
/// ```
///
/// # Compile-Time Validation
///
/// The macro performs several compile-time checks:
///
/// - Ensures the `#[id(Type)]` attribute is present
/// - Validates that generic parameters are not used (value types must be
///   concrete)
/// - Ensures all attribute arguments are well-formed
/// - Validates that the `method(name)` argument is provided when using
///   `#[extend]`
///
/// # Generated Trait Implementations
///
/// The generated key struct automatically derives:
///
/// - `Debug, Clone, Copy` - For basic functionality
/// - `PartialEq, Eq, PartialOrd, Ord` - For comparison operations
/// - `Hash` - For use in hash-based collections
/// - `Key` - The core query system trait
/// - `Serialize, Deserialize` - For database persistence
/// - `StableHash` - For consistent hashing across runs
///
/// # Requirements
///
/// Value types should implement or derive:
/// - Standard traits like `Debug`, `Clone`, `PartialEq`, `Eq`
/// - `Serialize` and `Deserialize` for database storage
/// - Any other traits required by your specific use case
///
/// # Thread Safety
///
/// The generated key types are thread-safe and can be used across multiple
/// threads, making them suitable for concurrent query processing.
#[proc_macro_derive(Value, attributes(id, extend, key, value, always_reverify))]
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

    let value_type = if let Some(value_attr) =
        input.attrs.iter().find(|attr| attr.path().is_ident("value"))
    {
        let Ok(value_type) = value_attr.parse_args::<syn::Type>() else {
            return syn::Error::new_spanned(
                value_attr,
                "invalid `#[value(Type)]` attribute on value type",
            )
            .to_compile_error()
            .into();
        };
        value_type
    } else {
        parse_quote!(#name)
    };

    let re_verify_attr =
        input.attrs.iter().find(|attr| attr.path().is_ident("always_reverify"));

    if let Some(attr @ Attribute { meta, .. }) = re_verify_attr {
        // only plain ident is allowed
        if !matches!(meta, Meta::Path(_)) {
            return syn::Error::new_spanned(
                attr,
                "`#[always_reverify]` attribute must be a plain identifier",
            )
            .into_compile_error()
            .into();
        }
    }

    let re_verify = re_verify_attr.is_some().then(|| {
        quote::quote! {
            #[always_reverify]
        }
    });

    let ext = get_ext(input.attrs.iter(), &value_type, &id_type, &key_ident);

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
        #re_verify
        pub struct #key_ident(pub #id_type);
    };

    quote::quote! {
        #key_struct
        #ext
    }
    .into()
}

fn get_ext<'a>(
    attributes: impl IntoIterator<Item = &'a Attribute>,
    return_ty: &impl quote::ToTokens,
    id_type: &syn::Type,
    key_ident: &syn::Ident,
) -> Option<proc_macro2::TokenStream> {
    match attributes.into_iter().find(|attr| attr.path().is_ident("extend")) {
        Some(attr) => {
            let mut method = None;
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

                if meta.path.is_ident("no_cyclic") {
                    dot_unwrap = Some(quote::quote! {
                        .expect("should have no cyclic dependencies")
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

                    let return_type: syn::Type = if dot_unwrap.is_some() {
                        parse_quote!(
                            #return_ty
                        )
                    } else {
                        parse_quote!(
                            ::pernixc_query::__internal::Result<
                                #return_ty,
                                ::pernixc_query::runtime::executor::CyclicError
                            >
                        )
                    };

                    Some(quote::quote! {
                        #[doc = concat!(
                            "An extension trait for the `",
                            stringify!(#return_ty),
                            "` value type.\n\n",
                            "This trait provides additional methods for retrieving values from the query database."
                        )]
                        #[allow(non_camel_case_types)]
                        pub trait #method {
                            #[doc = concat!(
                                "Retrieves a value of type `",
                                stringify!(#return_type),
                                "` from the query database using the given ID.\n\n",
                                "This method is automatically implemented for the `",
                                stringify!(#return_ty),
                                "` value type."
                            )]
                            fn #method(
                                &self,
                                id: #id_type
                            ) -> #return_type;
                        }

                        impl #method for ::pernixc_query::TrackedEngine<'_> {
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
                Err(error) => error.to_compile_error().into(),
            }
        }

        None => None,
    }
}

/// Generates an executor struct that implements the `Executor` trait for query
/// processing.
///
/// This procedural macro transforms a function into a query executor by
/// generating a corresponding struct that implements the `Executor` trait. The
/// executor can then be registered with the query system to handle computation
/// of values for specific key types.
///
/// # Required Attributes
///
/// ## `key(KeyType)`
///
/// Specifies the key type that this executor will handle. The key type must
/// implement the `Key` trait and will be used to identify which queries this
/// executor can process.
///
/// ## `name(ExecutorName)`
///
/// Specifies the name of the generated executor struct. This struct will
/// implement the `Executor<KeyType>` trait and can be registered with the query
/// runtime.
///
/// # Function Requirements
///
/// The annotated function must have the following signature:
///
/// ```ignore
/// fn function_name(
///     key: &KeyType,
///     engine: &TrackedEngine,
/// ) -> Result<ValueType, CyclicError>
/// ```
///
/// Where:
/// - `KeyType` must match the type specified in the `key()` attribute
/// - `ValueType` must be the associated value type for the key
///   (`KeyType::Value`)
/// - The function must return `Result<ValueType, CyclicError>` to handle cyclic
///   dependencies
///
/// # Generated Code
///
/// The macro generates:
///
/// 1. **Public Executor Struct**: A zero-sized struct with the specified name
///    that derives common traits (`Debug`, `Clone`, `Copy`, `PartialEq`, `Eq`,
///    `PartialOrd`, `Ord`, `Hash`, `Default`). This struct is publicly visible
///    and can be instantiated.
///
/// 2. **Anonymous Const Block**: Contains the original function and trait
///    implementation in a private scope to avoid namespace pollution.
///
/// 3. **Executor Implementation**: An implementation of `Executor<KeyType>`
///    that delegates to the original function, contained within the anonymous
///    const block.
///
/// 4. **Private Function**: The original function is preserved but kept private
///    within the const scope, preventing it from polluting the public
///    namespace.
///
/// # Examples
///
/// ## Basic Usage
///
/// ```ignore
/// use pernixc_query::{Key, TrackedEngine};
/// use pernixc_query::runtime::executor::CyclicError;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Hash, Key)]
/// #[value(String)]
/// struct UserNameKey(u32);
///
/// #[executor(key(UserNameKey), name(UserNameExecutor))]
/// fn get_user_name(
///     key: &UserNameKey,
///     engine: &TrackedEngine,
/// ) -> Result<String, CyclicError> {
///     // Query implementation here
///     Ok(format!("User {}", key.0))
/// }
///
/// // Generated:
/// // #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
/// // pub struct UserNameExecutor;
/// //
/// // const _: () = {
/// //     fn get_user_name(
/// //         key: &UserNameKey,
/// //         engine: &TrackedEngine,
/// //     ) -> Result<String, CyclicError> {
/// //         Ok(format!("User {}", key.0))
/// //     }
/// //
/// //     impl Executor<UserNameKey> for UserNameExecutor {
/// //         fn execute(
/// //             &self,
/// //             engine: &TrackedEngine,
/// //             key: &UserNameKey,
/// //         ) -> Result<String, CyclicError> {
/// //             get_user_name(key, engine)
/// //         }
/// //     }
/// // };
/// ```
///
/// ## With Private Visibility
///
/// ```ignore
/// #[executor(key(PrivateKey), name(PrivateExecutor))]
/// fn private_query(
///     key: &PrivateKey,
///     engine: &TrackedEngine,
/// ) -> Result<i32, CyclicError> {
///     // Private implementation
///     Ok(42)
/// }
///
/// // The generated executor struct will have the same visibility as the function
/// ```
///
/// ## Registration with Runtime
///
/// ```ignore
/// use std::sync::Arc;
/// use pernixc_query::runtime::executor::Registry;
///
/// let mut registry = Registry::default();
/// registry.register::<UserNameKey, UserNameExecutor>(
///     Arc::new(UserNameExecutor)
/// );
/// ```
///
/// # Compile-Time Validation
///
/// The macro performs several compile-time checks:
///
/// - Ensures both `key()` and `name()` attributes are present and well-formed
/// - Validates that the function has the correct signature with proper
///   parameter types
/// - Ensures the function returns a `Result` type for proper error handling
/// - Checks that the key type matches between the attribute and function
///   parameter
///
/// # Error Handling
///
/// Executors must handle cyclic dependencies by returning `CyclicError` when a
/// strongly connected component (SCC) is detected in the query dependency
/// graph. The query system uses this to implement proper cycle detection and
/// resolution.
///
/// # Thread Safety
///
/// Generated executor structs are automatically `Send + Sync` due to their
/// zero-sized nature and the derived traits. This makes them suitable for use
/// in concurrent query processing environments.
///
/// # Performance Considerations
///
/// - Executor structs are zero-sized and have no runtime overhead
/// - Function calls are direct and inlined where possible
/// - The delegation pattern adds minimal indirection
///
/// # Integration with Query System
///
/// Executors generated by this macro integrate seamlessly with the query
/// system:
///
/// - They can be registered with `Registry::register()`
/// - The query engine automatically invokes them when needed
/// - They participate in incremental compilation and caching
/// - They support dependency tracking and cycle detection
#[proc_macro_attribute]
#[allow(clippy::too_many_lines)]
pub fn executor(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> TokenStream {
    let input = syn::parse_macro_input!(input as syn::ItemFn);

    // Parse the attribute parameters: key(KeyType), name(ExecutorName)
    let mut key_type: Option<syn::Type> = None;
    let mut executor_name: Option<syn::Ident> = None;

    // Create a dummy attribute to use parse_nested_meta
    let attr_tokens = proc_macro2::TokenStream::from(attr);
    let dummy_attr: syn::Attribute = syn::parse_quote!(#[dummy(#attr_tokens)]);

    match dummy_attr.parse_nested_meta(|meta| {
        if meta.path.is_ident("key") {
            let content;
            syn::parenthesized!(content in meta.input);
            match content.parse::<syn::Type>() {
                Ok(ty) => {
                    key_type = Some(ty);
                    Ok(())
                }
                Err(err) => Err(syn::Error::new_spanned(
                    meta.path,
                    format!("invalid key type: {err}"),
                )),
            }
        } else if meta.path.is_ident("name") {
            let content;
            syn::parenthesized!(content in meta.input);
            match content.parse::<syn::Ident>() {
                Ok(ident) => {
                    executor_name = Some(ident);
                    Ok(())
                }
                Err(err) => Err(syn::Error::new_spanned(
                    meta.path,
                    format!("invalid executor name: {err}"),
                )),
            }
        } else {
            Err(syn::Error::new_spanned(
                meta.path,
                "expected `key(KeyType)` or `name(ExecutorName)`",
            ))
        }
    }) {
        Ok(()) => {}
        Err(err) => return err.to_compile_error().into(),
    }

    let Some(key_type) = key_type else {
        return syn::Error::new_spanned(
            &input.sig.ident,
            "missing required `key(KeyType)` parameter",
        )
        .to_compile_error()
        .into();
    };

    let Some(executor_name) = executor_name else {
        return syn::Error::new_spanned(
            &input.sig.ident,
            "missing required `name(ExecutorName)` parameter",
        )
        .to_compile_error()
        .into();
    };

    // Validate function signature
    if input.sig.inputs.len() != 2 {
        return syn::Error::new_spanned(
            &input.sig,
            "executor function must have exactly 2 parameters: (key: \
             &KeyType, engine: &TrackedEngine)",
        )
        .to_compile_error()
        .into();
    }

    // Get the original function details
    let fn_name = &input.sig.ident;
    let fn_vis = &input.vis;

    // Extract return type from the function signature
    let return_type = match &input.sig.output {
        syn::ReturnType::Type(_, ty) => ty.as_ref(),
        syn::ReturnType::Default => {
            return syn::Error::new_spanned(
                &input.sig,
                "executor function must have an explicit return type: \
                 Result<ValueType, CyclicError>",
            )
            .to_compile_error()
            .into()
        }
    };

    // Generate the expanded code
    let expanded = quote::quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
        #[doc = concat!(
            "An executor for the `",
            stringify!(#key_type),
            "` key type. This executor is used to compute values for the query system.\n\n",
            "This executor is generated from the `#[executor(key(KeyType), name(ExecutorName))]` attribute."
        )]
        #fn_vis struct #executor_name;

        // Anonymous const block to avoid namespace pollution
        const _: () = {
            // Keep the original function in the const scope
            #input

            // Implement the Executor trait
            impl ::pernixc_query::runtime::executor::Executor<#key_type> for #executor_name {
                fn execute(
                    &self,
                    engine: &::pernixc_query::TrackedEngine,
                    key: &#key_type,
                ) -> #return_type {
                    #fn_name(key, engine)
                }
            }
        };
    };

    TokenStream::from(expanded)
}
