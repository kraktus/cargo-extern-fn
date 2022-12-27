use clap::Args;

use log::trace;
use proc_macro2::TokenStream;
use syn::parse::{Parse, ParseStream};

use syn::visit::{self, Visit};
use syn::{
    visit_mut::{self, VisitMut},
    Attribute, ItemEnum, ItemStruct, Type, Visibility,
};
use syn::{Generics, ImplItemMethod, Item, ItemFn, ItemImpl};

use quote::{format_ident, quote, ToTokens};

use crate::utils::{
    attrs, call_function_from_sig, get_ident, get_ident_as_function, meta_is_extern_fn_skip,
    normalise_receiver_arg, union,
};

#[derive(Args, Debug)]
pub struct Cbindgen;

// add #[repr(C)]
// to all public types
struct AddReprC;

struct Attrs(pub Vec<Attribute>);

impl Parse for Attrs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(input.call(Attribute::parse_outer)?))
    }
}

fn outer_attr(input: &str) -> Attribute {
    let mut attrs: Attrs = syn::parse_str(input).unwrap();
    assert_eq!(attrs.0.len(), 1);
    attrs.0.remove(0)
}

impl VisitMut for AddReprC {
    fn visit_item_enum_mut(&mut self, enum_: &mut ItemEnum) {
        if matches!(enum_.vis, Visibility::Public(_))
            && enum_.attrs.iter().all(|a| !a.path.is_ident("repr"))
        {
            enum_.attrs.push(outer_attr("#[repr(C)]"));
        }
        visit_mut::visit_item_enum_mut(self, enum_);
    }

    fn visit_item_struct_mut(&mut self, struct_: &mut ItemStruct) {
        if matches!(struct_.vis, Visibility::Public(_))
            && struct_.attrs.iter().all(|a| !a.path.is_ident("repr"))
        {
            struct_.attrs.push(outer_attr("#[repr(C)]"));
        }
        visit_mut::visit_item_struct_mut(self, struct_);
    }

    fn visit_item_mut(&mut self, i: &mut Item) {
        if attrs(i).map_or(true, |attrs| {
            attrs
                .iter()
                .all(|a| !meta_is_extern_fn_skip(a.parse_meta()))
        }) {
            visit_mut::visit_item_mut(self, i)
        }
    }
}
// for each file, add at the end of it its externalised fn
// regular `pub fn foo(arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn ffi_foo(arg1: X, arg2: &Y) -> bool`
// method `pub fn foo_method(&self,arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn ffi_foo_method(self_: &Foo,arg1: X, arg2: &Y) -> bool`
#[derive(Debug, Clone, Default)]
struct ExternaliseFn {
    // only set if not a trait method
    current_impl_ty: Option<Type>,
    current_generic_bounds: Option<Generics>,
    externalised_fn_buf: Vec<ItemFn>,
}

impl ToTokens for ExternaliseFn {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        for item_fn in self.externalised_fn_buf.iter() {
            item_fn.to_tokens(tokens)
        }
    }
}

impl ExternaliseFn {
    fn handle_item_fn(&mut self, item_fn: &ItemFn, ty: Option<Type>, generics: Option<Generics>) {
        if item_fn.sig.asyncness.is_none()
            && item_fn.sig.abi.is_none()
            && matches!(item_fn.vis, Visibility::Public(_))
            // do not handle function with `cfg` attributes for the moment
            && item_fn.attrs.iter().all(|a| !a.path.is_ident("cfg") && !meta_is_extern_fn_skip(a.parse_meta()))
        {
            let mut extern_fn = item_fn.clone();
            trace!("handling fn {:?}", extern_fn.sig.ident);
            extern_fn.attrs.push(outer_attr("#[no_mangle]"));
            extern_fn.sig.abi = Some(syn::parse_str(r#"extern "C""#).unwrap());
            extern_fn.sig.ident = format_ident!(
                "ffi_{}{}",
                ty.as_ref()
                    .and_then(|ty| Some(get_ident_as_function(ty)?.to_string()))
                    .unwrap_or_default(),
                extern_fn.sig.ident
            );
            extern_fn.sig.generics = generics
                .clone()
                .map_or(extern_fn.sig.generics.clone(), |g1| {
                    union(g1, extern_fn.sig.generics)
                });
            for arg in extern_fn.sig.inputs.iter_mut() {
                if let Some(normalised_arg) =
                    normalise_receiver_arg(arg, &self.current_impl_ty, Some("_"))
                {
                    *arg = normalised_arg;
                }
            }
            // the body of the function should just be calling the original function
            extern_fn.block = syn::parse2(call_function_from_sig(
                self.current_impl_ty.as_ref(),
                &item_fn.sig,
            ))
            .unwrap();

            self.externalised_fn_buf.push(extern_fn);
        }
    }
}

impl<'ast> Visit<'ast> for ExternaliseFn {
    fn visit_item_impl(&mut self, item_impl: &'ast ItemImpl) {
        if item_impl.trait_.is_none() {
            self.current_impl_ty = Some(*item_impl.self_ty.clone());
            self.current_generic_bounds = Some(item_impl.generics.clone());
            trace!("Looking at impl of {:?}", get_ident(&*item_impl.self_ty))
        } else {
            self.current_impl_ty = None;
            self.current_generic_bounds = None;
        }
        visit::visit_item_impl(self, item_impl);
        self.current_impl_ty = None;
        self.current_generic_bounds = None;
    }

    fn visit_item(&mut self, i: &'ast Item) {
        if attrs(i).map_or(true, |attrs| {
            attrs
                .iter()
                .all(|a| !meta_is_extern_fn_skip(a.parse_meta()))
        }) {
            visit::visit_item(self, i)
        }
    }

    fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
        self.handle_item_fn(item_fn, None, None);
        visit::visit_item_fn(self, item_fn);
    }

    fn visit_impl_item_method(&mut self, item_method: &'ast ImplItemMethod) {
        let item_fn: ItemFn =
            syn::parse2(item_method.to_token_stream()).expect("from method to bare fn failed");
        self.handle_item_fn(
            &item_fn,
            self.current_impl_ty.clone(),
            self.current_generic_bounds.clone(),
        );
        visit::visit_impl_item_method(self, item_method);
    }
}

impl Cbindgen {
    pub fn handle_file(parsed_file: &mut syn::File) -> TokenStream {
        trace!("Starting AddReprC pass");
        AddReprC.visit_file_mut(parsed_file);
        trace!("Finished AddReprC pass");
        let mut externalised_fn = ExternaliseFn::default();
        trace!("Starting ExternaliseFn pass");
        externalised_fn.visit_file(&parsed_file);
        trace!("Finished ExternaliseFn pass");
        let mut parsed_file_tokens = quote!(#parsed_file);
        externalised_fn.to_tokens(&mut parsed_file_tokens);
        parsed_file_tokens
    }
}
