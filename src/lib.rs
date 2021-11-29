extern crate proc_macro;
use crate::proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::{
    parenthesized, parse, Attribute, Data, DeriveInput, Error, Field, Fields,
    Ident, Index, DataEnum
};
use proc_macro2::Span;
use proc_macro2::TokenStream as Tokens;

#[proc_macro_derive(Diff, attributes(diff))]
pub fn diff_derive(input: TokenStream) -> TokenStream
{
    let input: DeriveInput = syn::parse(input).unwrap();

    match input.data {
        Data::Struct(data_struct) => {
            let struct_attr = parse_struct_attributes(&input.attrs);
            let ident = &input.ident;
            let diff_ident = &struct_attr.name.unwrap_or(format_ident!("{}Diff", ident));
            let attr = &struct_attr.attrs.0;

            match &data_struct.fields {
                Fields::Named(fields) => derive_named(attr, ident, diff_ident, &fields.named),
                Fields::Unnamed(fields) => derive_unnamed(attr, ident, diff_ident, &fields.unnamed),
                Fields::Unit => derive_unit(ident),
            }
        }
        Data::Enum(data_enum) => {
            let struct_attr = parse_struct_attributes(&input.attrs);
            let ident = &input.ident;
            let diff_ident = &struct_attr.name.unwrap_or(format_ident!("{}Diff", ident));
            let attr = &struct_attr.attrs.0;
            derive_enum(attr, ident, diff_ident, &data_enum)
        }
        _ => todo!(),
    }.into()
}

fn derive_named(
    attr: &[Attribute],
    ident: &Ident,
    diff_ident: &Ident,
    fields: &Punctuated<Field, Comma>,
) -> Tokens {
    let names = fields.iter().map(|field| &field.ident).collect::<Vec<_>>();
    let types = fields.iter().map(|field| &field.ty).collect::<Vec<_>>();
    quote! {
        #(#attr)*
        pub struct #diff_ident {
            #(pub #names: <#types as Diff>::Repr),*
        }

        impl Diff for #ident {
            type Repr = #diff_ident;

            fn diff(&self, other: &Self) -> Self::Repr {
                #diff_ident {
                    #(#names: self.#names.diff(&other.#names)),*
                }
            }

            fn apply(&mut self, diff: &Self::Repr) {
                #(self.#names.apply(&diff.#names);)*
            }

            fn identity() -> Self {
                Self {
                    #(#names: <#types as Diff>::identity()),*
                }
            }
        }
    }
}

fn derive_unnamed(
    attr: &[Attribute],
    ident: &Ident,
    diff_ident: &Ident,
    fields: &Punctuated<Field, Comma>,
) -> Tokens {
    let (numbers, types): (Vec<_>, Vec<_>) = fields
        .iter()
        .map(|field| &field.ty)
        .enumerate()
        .map(|(a, b)| (Index::from(a), b))
        .unzip();
    quote! {
        #(#attr)*
        pub struct #diff_ident (
            #(pub <#types as Diff>::Repr),*
        );

        impl Diff for #ident {
            type Repr = #diff_ident;

            fn diff(&self, other: &Self) -> Self::Repr {
                #diff_ident (
                    #(self.#numbers.diff(&other.#numbers))*
                )
            }

            fn apply(&mut self, diff: &Self::Repr) {
                #(self.#numbers.apply(&diff.#numbers);)*
            }

            fn identity() -> Self {
                Self (
                    #(<#types as Diff>::identity()),*
                )
            }
        }
    }
}

#[derive(Default)]
struct StructAttributes {
    name: Option<Ident>,
    visible: bool,
    attrs: OuterAttributes,
}

/// A named attribute with unspecified tokens inside parentheses
struct ParenAttr {
    name: Ident,
    tokens: TokenStream,
}

impl Parse for ParenAttr {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        let name = input.parse()?;
        let content;
        parenthesized!(content in input);
        Ok(ParenAttr {
            name,
            tokens: content.parse::<proc_macro2::TokenStream>()?.into(),
        })
    }
}

#[derive(Default)]
struct OuterAttributes(Vec<Attribute>);

impl Parse for OuterAttributes {
    fn parse(input: ParseStream) -> Result<Self, Error> {
        Ok(Self(input.call(Attribute::parse_outer)?))
    }
}

fn parse_struct_attributes(attrs: &[Attribute]) -> StructAttributes {
    let mut struct_attrs = StructAttributes::default();
    attrs
        .iter()
        .filter(|a| a.path.is_ident("diff"))
        .for_each(|a| {
            let attr_named: ParenAttr = a.parse_args().unwrap();
            let name = attr_named.name.to_string();
            match name.as_ref() {
                "attr" => {
                    struct_attrs.attrs = parse(attr_named.tokens).unwrap();
                }
                _ => panic!(
                    "Unexpected name for diff attribute '{}'. Possible names: 'attr'",
                    name
                ),
            }
        });
    struct_attrs
}

fn derive_unit(
    ident: &Ident,
) -> Tokens {
    quote! {
        impl Diff for #ident {
            type Repr = ();

            fn diff(&self, other: &Self) -> Self::Repr {
                ()
            }

            fn apply(&mut self, diff: &Self::Repr) {
                ()
            }

            fn identity() -> Self {
                Self
            }
        }
    }
}

fn derive_enum(
    attr: &[Attribute],
    ident: &Ident,
    diff_ident: &Ident,
    data_enum: &DataEnum,
) -> Tokens {
    let first = data_enum.variants.first().unwrap();
    let first_ident = &first.ident;
    let first_names = first.fields.iter().map(|field| &field.ident).collect::<Vec<_>>();
    let first_types = first.fields.iter().map(|field| &field.ty).collect::<Vec<_>>();

    let variants_type_decl = data_enum.variants.iter().map(|variant| {
        let ident = &variant.ident;
        match &variant.fields {
            Fields::Named(fields) => {
                let names = fields.named.iter().map(|field| &field.ident).collect::<Vec<_>>();
                let types = fields.named.iter().map(|field| &field.ty).collect::<Vec<_>>();

                quote! { #ident{#(#names: <#types as Diff>::Repr),*} }
            },
            Fields::Unnamed(fields) => {
                let types = fields.unnamed.iter().map(|field| &field.ty).collect::<Vec<_>>();
                quote! { #ident(#(<#types as Diff>::Repr),*) }
            },
            Fields::Unit => quote! {
                #ident
            },
        }
    }).collect::<Vec<Tokens>>();

    let variants_diff_arms = data_enum.variants.iter().map(|variant| {
        let ident = &variant.ident;
        match &variant.fields {
            Fields::Named(fields) => {
                let t = fields.named.iter().map(|x|&x.ty).collect::<Vec<_>>();
                let i = fields.named.iter().map(|x|&x.ident).collect::<Vec<_>>();
                let a = fields.named.iter()
                    .map(|x| syn::Ident::new(&format!("a_{}", x.ident.as_ref().unwrap()), Span::call_site()))
                    .collect::<Vec<_>>();
                let b = fields.named.iter()
                    .map(|x| syn::Ident::new(&format!("b_{}", x.ident.as_ref().unwrap()), Span::call_site()))
                    .collect::<Vec<_>>();
                quote! {
                    (Self::#ident{#(#i: #a),*}, Self::#ident{#(#i: #b),*}) =>
                        if #(#a == #b)&&* {
                            Self::Repr::NoChange
                        } else {
                            #diff_ident::#ident{#(#i: #a.diff(#b)),*}
                        },
                    (_, Self::#ident{#(#i: #b),*}) =>
                        #diff_ident::#ident{#(#i: <#t as Diff>::identity().diff(#b)),*}
                }
            },
            Fields::Unnamed(fields) => {
                let t = fields.unnamed.iter().map(|x|&x.ty).collect::<Vec<_>>();
                let a = (0..fields.unnamed.len())
                    .map(|x| syn::Ident::new(&format!("a{}", x), Span::call_site()))
                    .collect::<Vec<_>>();
                let b = (0..fields.unnamed.len())
                    .map(|x| syn::Ident::new(&format!("b{}", x), Span::call_site()))
                    .collect::<Vec<_>>();
                quote! {
                    (Self::#ident(#(#a),*), Self::#ident(#(#b),*)) =>
                        if #(#a == #b)&&* {
                            Self::Repr::NoChange
                        } else {
                            #diff_ident::#ident(#(#a.diff(#b)),*)
                        },
                    (_, Self::#ident(#(#b),*)) =>
                        #diff_ident::#ident(#(<#t as Diff>::identity().diff(#b)),*)
                }
            },
            Fields::Unit => quote! {
                (Self::#ident, Self::#ident) => Self::Repr::NoChange,
                (_, Self::#ident) => Self::Repr::#ident
            },
        }
    }).collect::<Vec<Tokens>>();

    let variants_apply_arms = data_enum.variants.iter().map(|variant| {
        let ident = &variant.ident;
        match &variant.fields {
            Fields::Named(fields) => {
                let t = fields.named.iter().map(|x|&x.ty).collect::<Vec<_>>();
                let i = fields.named.iter().map(|x|&x.ident).collect::<Vec<_>>();
                let a = fields.named.iter()
                    .map(|x| syn::Ident::new(&format!("a_{}", x.ident.as_ref().unwrap()), Span::call_site()))
                    .collect::<Vec<_>>();
                let b = fields.named.iter()
                    .map(|x| syn::Ident::new(&format!("b_{}", x.ident.as_ref().unwrap()), Span::call_site()))
                    .collect::<Vec<_>>();
                quote! {
                    Self::Repr::#ident{#(#i: #b),*} => {
                        if let Self::#ident{#(#i: #a),*} = self {
                            #(#a.apply(#b));*;
                        } else {
                            *self = Self::#ident{#(#i: <#t as Diff>::identity().apply_new(#b)),*};
                        }
                    }
                }
            },
            Fields::Unnamed(fields) => {
                let t = fields.unnamed.iter().map(|x|&x.ty).collect::<Vec<_>>();
                let a = (0..fields.unnamed.len())
                    .map(|x| syn::Ident::new(&format!("a{}", x), Span::call_site()))
                    .collect::<Vec<_>>();
                let b = (0..fields.unnamed.len())
                    .map(|x| syn::Ident::new(&format!("b{}", x), Span::call_site()))
                    .collect::<Vec<_>>();
                quote! {
                    Self::Repr::#ident(#(#b),*) => {
                        if let Self::#ident(#(#a),*) = self {
                            #(#a.apply(#b));*;
                        } else {
                            *self = Self::#ident(#(<#t as Diff>::identity().apply_new(#b)),*);
                        }
                    }
                }
            },
            Fields::Unit => quote! {
                Self::Repr::#ident => *self = Self::#ident
            },
        }
    }).collect::<Vec<Tokens>>();

    let identity = match &first.fields {
        Fields::Named(_) => quote! {
            Self::#first_ident { #(#first_names: <#first_types as Diff>::identity()),* }
        },
        Fields::Unnamed(_) => quote! {
            Self::#first_ident ( #(<#first_types as Diff>::identity()),* )
        },
        Fields::Unit => quote! {
            Self::#first_ident
        },
    };

    quote! {
        #(#attr)*
        pub enum #diff_ident {
            NoChange,
            #(#variants_type_decl),*,
        }

        impl Diff for #ident {
            type Repr = #diff_ident;

            fn diff(&self, other: &Self) -> Self::Repr {
                match (self, other) {
                    #(#variants_diff_arms),*,
                }
            }

            fn apply(&mut self, diff: &Self::Repr) {
                match diff {
                    Self::Repr::NoChange => {},
                    #(#variants_apply_arms),*,
                }
            }

            fn identity() -> Self {
                #identity
            }
        }
    }
}


// fn parse_field_attributes(attrs: &[Attribute]) -> FieldAttributes {
//     let field_attrs = FieldAttributes::default();
//     // iter
//     field_attrs
// }
