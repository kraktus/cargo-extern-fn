use std::collections::HashSet;
use std::fs::File;
use std::io::Read;
use std::path::Path;

use clap::Args;
use log::trace;
use proc_macro2::TokenStream;

use syn::visit::{self, Visit};
use syn::{Fields, FnArg, Generics, Ident, ImplItemMethod, Item, ItemFn, ItemImpl};
use syn::{ItemEnum, ItemStruct, Visibility};

use quote::{format_ident, quote, ToTokens};

use crate::cbindgen::{attrs, get_ident, meta_is_extern_fn_skip, normalise_receiver_arg};

#[derive(Default, Debug)]
pub struct Cxx {
    all_cxx_fn: Vec<syn::ItemFn>,
    all_cxx_struct_or_enum: Vec<StructOrEnum>, // TODO maybe switch to derive
}

#[derive(Debug)]
enum StructOrEnum {
    S(ItemStruct),
    E(ItemEnum),
}

#[derive(Debug, Clone)]
struct CreateRawStruct {
    raw_structs: Vec<ItemStruct>, // public enums always have their variants public, so no need to construct a sibling
    idents: HashSet<Ident>,       // idents of struct/enums. TODO union?
}

impl Default for CreateRawStruct {
    fn default() -> Self {
        Self {
            raw_structs: vec![],
            idents: HashSet::new(),
        }
    }
}

impl CreateRawStruct {
    fn results(self) -> (Vec<ItemStruct>, HashSet<Ident>) {
        let Self {
            raw_structs,
            idents,
        } = self;
        (raw_structs, idents)
    }
}

impl<'ast> Visit<'ast> for CreateRawStruct {
    fn visit_item_struct(&mut self, struct_: &'ast ItemStruct) {
        if matches!(struct_.vis, Visibility::Public(_)) && struct_.generics.params.is_empty()
        // generics not handled by cxx
        {
            trace!("Creating raw struct of {}", struct_.ident);
            let mut raw_struct = struct_.clone();
            raw_struct.ident = format_ident!("{}Raw", struct_.ident);
            for field_mut in raw_struct.fields.iter_mut() {
                field_mut.vis = struct_.vis.clone() // make it public, not sure if reusing the span will cause issue
            }
            self.raw_structs.push(raw_struct);
            self.idents.insert(struct_.ident.clone());
        }
        visit::visit_item_struct(self, struct_);
    }

    fn visit_item_enum(&mut self, enum_: &'ast ItemEnum) {
        if matches!(enum_.vis, Visibility::Public(_))
            && enum_.generics.params.is_empty()
            && enum_
                .variants
                .iter()
                .all(|v| matches!(v.fields, Fields::Unit))
        // generics not handled by cxx
        {
            self.idents.insert(enum_.ident.clone());
        }
        visit::visit_item_enum(self, enum_);
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
}

// for each file, add at the end of it its externalised fn
// regular `pub fn foo(arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn ffi_foo(arg1: X, arg2: &Y) -> bool`
// method `pub fn foo_method(&self,arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn ffi_foo_method(self_: &Foo,arg1: X, arg2: &Y) -> bool`
#[derive(Debug, Clone, Default)]
struct GatherSignatures {
    // only set if not a trait method
    current_impl_ty: Option<syn::Type>,
    allowed_idents: HashSet<Ident>,
    cxx_fn_buf: Vec<ItemFn>,
}

impl GatherSignatures {
    fn new(allowed_idents: HashSet<Ident>) -> Self {
        Self {
            current_impl_ty: None,
            allowed_idents,
            cxx_fn_buf: vec![],
        }
    }

    fn handle_item_fn(
        &mut self,
        item_fn: &ItemFn,
        prefix: Option<String>, // only set when handling associated functions
    ) {
        if item_fn.sig.asyncness.is_none()
            && item_fn.sig.abi.is_none()
            && matches!(item_fn.vis, Visibility::Public(_))
            // do not handle function with `cfg` attributes for the moment
            && item_fn.attrs.iter().all(|a| !a.path.is_ident("cfg") && !meta_is_extern_fn_skip(a.parse_meta()))
        {
            let mut extern_fn = item_fn.clone();
            trace!("handling fn {:?}", extern_fn.sig.ident);
            extern_fn.sig.ident =
                format_ident!("{}{}", prefix.unwrap_or_default(), extern_fn.sig.ident);
            for arg in extern_fn.sig.inputs.iter_mut() {
                if let Some(normalised_arg) =
                    normalise_receiver_arg(arg, &self.current_impl_ty, None)
                {
                    *arg = normalised_arg;
                }
            }

            self.cxx_fn_buf.push(extern_fn);
        }
    }
}

impl<'ast> Visit<'ast> for GatherSignatures {
    fn visit_item_impl(&mut self, item_impl: &'ast ItemImpl) {
        if item_impl.trait_.is_none()
            && get_ident(&item_impl.self_ty).map_or(false, |ident| {
                dbg!(dbg!(&self.allowed_idents).contains(dbg!(&ident)))
            })
        {
            self.current_impl_ty = Some(*item_impl.self_ty.clone());
            trace!("Looking at impl of {:?}", get_ident(&*item_impl.self_ty))
        } else {
            self.current_impl_ty = None;
        }
        visit::visit_item_impl(self, item_impl);
        self.current_impl_ty = None;
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
        self.handle_item_fn(
            item_fn,
            self.current_impl_ty
                .clone()
                .and_then(|ty| Some(get_ident(&ty)?.to_string().to_lowercase() + "_")),
        );
        visit::visit_item_fn(self, item_fn);
    }

    fn visit_impl_item_method(&mut self, item_method: &'ast ImplItemMethod) {
        let item_fn: ItemFn =
            syn::parse2(item_method.to_token_stream()).expect("from method to bare fn failed");
        self.handle_item_fn(&item_fn, None);
        visit::visit_impl_item_method(self, item_method);
    }
}

fn impl_from_x_to_y(x: &Ident, y: &Ident, fields: &Fields) -> TokenStream {
    let body = match fields {
        Fields::Named(nameds) => {
            let named_token = nameds.named.iter().map(|f| {
                let ident = f.ident.as_ref().expect("named field");
                quote!(#ident: x.#ident)
            });
            quote!({#(#named_token),*})
        }
        Fields::Unnamed(unnameds) => {
            let unnamed_token = (0..unnameds.unnamed.len()).map(syn::Index::from).map(|i| quote!(x.#i));
            quote!((#(#unnamed_token),*))
        }
        Fields::Unit => unimplemented!("TBD"),
    };
    quote!(impl From<#x> for #y {
        fn from(x: #x) -> Self {
            Self #body
        }
    })
}

impl Cxx {
    pub fn handle_file(&mut self, parsed_file: &mut syn::File) -> TokenStream {
        trace!("Starting CreateRawStruct pass");
        let mut create_raw_struct = CreateRawStruct::default();
        create_raw_struct.visit_file(parsed_file);
        trace!("Finished CreateRawStruct pass");
        let (raw_structs, idents) = create_raw_struct.results();
        // trace!("Starting GatherSignatures pass");
        // let mut gather_sig = GatherSignatures::new(idents);
        // gather_sig.visit_file(parsed_file);
        // trace!("Finished GatherSignatures pass");
        // self.all_cxx_fn.extend(gather_sig.cxx_fn_buf);

        let mut parsed_file_tokens = quote!(#parsed_file);
        for raw_struct in raw_structs {
            raw_struct.to_tokens(&mut parsed_file_tokens);
            let ident = raw_struct.ident.to_string();
            let ident_without_raw: Ident = syn::parse_str(&ident[0..ident.len() - 3]).unwrap();
            trace!("Generating conversion impl of {ident}");
            impl_from_x_to_y(&ident_without_raw, &raw_struct.ident, &raw_struct.fields)
                .to_tokens(&mut parsed_file_tokens);
            impl_from_x_to_y(&raw_struct.ident, &ident_without_raw, &raw_struct.fields)
                .to_tokens(&mut parsed_file_tokens);
            trace!("Finished conversion impl of {ident}");
        }
        parsed_file_tokens
    }

    pub fn generate_ffi_bridge_and_impl(self, code_dir: &Path) {
        todo!()
        // let mut lib = File::open(code_dir.join("lib.rs")).expect("reading lib.rs in src_dir failed");
        // let mut src_lib = String::new();
        // lib.read_to_string(&mut src_lib)
        //     .expect("Unable to read file");
        // let mut parsed_file = syn::parse_file(&src_lib).expect("Unable to parse file");
    }
}
