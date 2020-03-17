extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident, LitInt};

#[proc_macro_derive(Codegen)]
pub fn codegen(input: TokenStream) -> TokenStream {
    // Construct a string representation of the type definition
    let input = parse_macro_input!(input as DeriveInput);

    let name = input.ident;
    let variants: Vec<Ident> = match input.data {
        Data::Enum(data) => data.variants.iter().map(|v| v.ident.clone()).collect(),
        _ => panic!("codegen derive only works on Enum"),
    };
    // Parse the string representation
    let expended = quote! {
        impl Codegen for #name {
            fn to_code(&self) -> String {
                match self {
                    #(Self::#variants(a) => a.to_code()),*
                }
            }
        }
    };

    TokenStream::from(expended)
}

#[proc_macro_derive(Precedence, attributes(precedence))]
pub fn precedence(input: TokenStream) -> TokenStream {
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

    TokenStream::from(expended)
}
