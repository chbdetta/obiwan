extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident, LitInt, LitStr};

#[proc_macro_derive(Codegen)]
pub fn codegen(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Construct a string representation of the type definition
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let arms: Vec<_> = match input.data {
        Data::Enum(data) => data
            .variants
            .iter()
            .map(|v| {
                let ident = &v.ident;
                let ident_str = LitStr::new(&ident.to_string().to_lowercase(), Span::call_site());
                match &v.fields {
                    Fields::Unit => quote! {
                        Self::#ident => String::from(#ident_str)
                    },
                    Fields::Unnamed(unnamed) => {
                        if unnamed.unnamed.len() != 1 {
                            panic!("Codegen requires exactly one struct")
                        } else {
                            quote! {
                                Self::#ident(a) => a.to_code()
                            }
                        }
                    }
                    _ => panic!("Codegen does not support named variant fields"),
                }
            })
            .collect::<Vec<_>>(),
        _ => panic!("codegen derive only works on Enum"),
    };
    // Parse the string representation
    let expended = quote! {
        impl Codegen for #name {
            fn to_code(&self) -> String {
                match self {
                    #(#arms),*
                }
            }
        }
    };

    TokenStream::from(expended).into()
}

#[proc_macro_derive(Precedence, attributes(precedence))]
pub fn precedence(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Construct a string representation of the type definition
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let (variants, structs, precedences) = match &input.data {
        Data::Enum(data) => {
            let variants = data
                .variants
                .iter()
                .map(|v| v.ident.clone())
                .collect::<Vec<Ident>>();
            let (structs, precedences): (Vec<_>, Vec<_>) = data
                .variants
                .iter()
                .filter(|v| v.attrs.iter().any(|attr| attr.path.is_ident("precedence")))
                .map(|v| -> (_, LitInt) {
                    (
                        match &v.fields {
                            Fields::Unnamed(fields) => {
                                if fields.unnamed.len() > 1 {
                                    panic!("precedence only works on single unnamed variants");
                                } else {
                                    fields.unnamed[0].ty.clone()
                                }
                            }
                            _ => panic!("precedence only works on Unnamed variants"),
                        },
                        v.attrs
                            .iter()
                            .find(|attr| attr.path.is_ident("precedence"))
                            .unwrap()
                            .parse_args()
                            .expect("The precedence should be an u8"),
                    )
                })
                .unzip();

            (variants, structs, precedences)
        }
        _ => panic!("precedence derive only works on Enum"),
    };
    // Parse the string representation
    let expended = quote! {
        impl Precedence for #name {
            fn precedence(&self) -> u8 {
                match self {
                    #(Self::#variants(a) => a.precedence()),*
                }
            }
        }
        #(
            impl Precedence for #structs {
                fn precedence(&self) -> u8 {
                    #precedences
                }
            }
        )*
    };

    TokenStream::from(expended).into()
}

#[proc_macro_derive(Eval)]
pub fn eval(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let extended = quote! {
        impl Eval for #name {
            fn eval<U>(&self, evaluator: &mut U) -> crate::eval::Value
            where
                U: crate::eval::Evaluator<Self> {
                    evaluator.eval(self)
                }
        }
    };

    extended.into()
}
