//! Contains the definition of the [`syntax_tree!`] macro, which is used to

/// Macro used for generating both syntax tree  
#[macro_export]
macro_rules! syntax_tree {
    (
        $struct_vis:vis
        struct
        $struct_name:ident
        $( < $generic_param:tt > )?
        {
            $(
                $field_vis:vis
                $field_name:ident: $field_type:ty
                = $parser_expr:expr
            ),* $(,)?
        }
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        #[allow(missing_docs)]
        $struct_vis
        struct
        $struct_name
        $( <$generic_param> )?
        {
        }
    };

    (
        $enum_vis:vis
        enum
        $enum_name:ident
        $( < $generic_param:tt > )?
        {
            $(
                $variant_name:ident
                (
                    $variant_type:ty
                    =
                    $parser_expr:expr
                )

            ),* $(,)?
        }
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq)]
        #[allow(missing_docs)]
        $enum_vis enum $enum_name {
            $(
                $variant_name($variant_type),
            )*
        }
    }
}
