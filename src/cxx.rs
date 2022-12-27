use std::collections::HashSet;

use std::path::Path;

use log::trace;
use proc_macro2::TokenStream;

use syn::visit::{self, Visit};
use syn::{parse_quote, Fields, Ident, ImplItemMethod, Item, ItemFn, ItemImpl, ReturnType, Type};
use syn::{ItemEnum, ItemStruct, Visibility};

use quote::{format_ident, quote, ToTokens};

use crate::utils::{
    attrs, get_ident, get_ident_as_function, meta_is_extern_fn_skip, method_self_type,
    normalise_receiver_arg, SelfType,
};
use crate::utils::{call_function_from_sig, is_type};

#[derive(Default, Debug)]
pub struct Cxx {
    all_cxx_fn: Vec<CxxFn>,
    all_cxx_struct_or_enum: Vec<StructOrEnum>, // TODO maybe switch to derive
}

#[derive(Debug, Clone)]
enum StructOrEnum {
    S(ItemStruct),
    E(ItemEnum),
}

impl ToTokens for StructOrEnum {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match &self {
            StructOrEnum::S(x) => x.to_tokens(tokens),
            StructOrEnum::E(x) => x.to_tokens(tokens),
        }
    }
}

impl StructOrEnum {
    fn ident(&self) -> &Ident {
        match &self {
            StructOrEnum::S(x) => &x.ident,
            StructOrEnum::E(x) => &x.ident,
        }
    }

    fn ident_mut(&mut self) -> &mut Ident {
        match self {
            StructOrEnum::S(x) => &mut x.ident,
            StructOrEnum::E(x) => &mut x.ident,
        }
    }
}

#[derive(Debug, Clone)]
struct CreateRawStruct {
    pub_struct_or_enum: Vec<StructOrEnum>,
    idents: HashSet<Ident>, // idents of struct/enums. TODO union?
}

impl Default for CreateRawStruct {
    fn default() -> Self {
        Self {
            pub_struct_or_enum: vec![],
            idents: HashSet::new(),
        }
    }
}

impl CreateRawStruct {
    fn results(self) -> (Vec<StructOrEnum>, HashSet<Ident>) {
        let Self {
            pub_struct_or_enum,
            idents,
        } = self;
        (pub_struct_or_enum, idents)
    }
}

impl<'ast> Visit<'ast> for CreateRawStruct {
    fn visit_item_struct(&mut self, struct_: &'ast ItemStruct) {
        if matches!(struct_.vis, Visibility::Public(_)) && struct_.generics.params.is_empty()
        // generics not handled by cxx
        {
            trace!("Creating pub struct of {}", struct_.ident);
            let mut raw_struct = struct_.clone();
            for field_mut in raw_struct.fields.iter_mut() {
                field_mut.vis = struct_.vis.clone() // make it public, not sure if reusing the span will cause issue
            }
            self.pub_struct_or_enum.push(StructOrEnum::S(raw_struct));
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
            self.pub_struct_or_enum.push(StructOrEnum::E(enum_.clone()))
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
    cxx_fn_buf: Vec<CxxFn>,
}

#[derive(Debug, Clone)]
struct CxxFn {
    pub item_fn: ItemFn,
    ty: Option<Type>,
    pub return_is_opt: bool, // cxx does not handle `Option`... convert to `Result`
}

impl CxxFn {
    fn new(item_fn: ItemFn, ty: Option<Type>) -> Self {
        let return_is_opt = if let ReturnType::Type(_, ty) = item_fn.sig.output.clone() {
            is_type("Option", &*ty)
        } else {
            false
        };
        Self {
            item_fn,
            return_is_opt,
            ty,
        }
    }

    fn as_cxx_sig(&self) -> TokenStream {
        let mut cxx_sig = self.item_fn.sig.clone();
        for arg in cxx_sig.inputs.iter_mut() {
            if let Some(normalised_arg) = normalise_receiver_arg(arg, &self.ty, None) {
                *arg = normalised_arg;
            }
        }
        if self.return_is_opt {
            if let ReturnType::Type(_, ref mut ty) = cxx_sig.output {
                if let Type::Path(ref mut p) = **ty {
                    p.path.segments[0].ident = syn::parse_str("Result").unwrap();
                }
            }
        }
        quote!(#cxx_sig;)
    }

    // TokenStream of an ItemFn
    fn as_cxx_impl(&self) -> TokenStream {
        let mut cxx_item = self.item_fn.clone();
        if self.return_is_opt {
            if let ReturnType::Type(_, ref mut ty) = cxx_item.sig.output {
                if let Type::Path(ref mut p) = **ty {
                    p.path.segments[0].ident = syn::parse_str("ResultFfi").unwrap();
                }
            }
        }
        let self_type_opt = cxx_item.sig.inputs.iter().find_map(method_self_type);
        let before_call_fn = if let Some(self_type) = self_type_opt.as_ref() {
            let ty_ = self.ty.as_ref().expect("No type defined in method");
            match self_type {
                SelfType::ValueMut => quote!(let mut x = <#ty_>::from(self);),
                SelfType::RefMut => quote!(let mut x = <#ty_>::from(self.clone());),
                _ => TokenStream::new(),
            }
        } else {
            TokenStream::new()
        };

        let mut call_fn = if let Some(self_type) = self_type_opt.as_ref() {
            let ty_ = self.ty.as_ref().expect("No type defined in method");
            match self_type {
                SelfType::Value => {
                    call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!(self.into()))
                }
                SelfType::ValueMut => call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!(x)),
                SelfType::Ref => call_function_from_sig(
                    self.ty.as_ref(),
                    &cxx_item.sig,
                    quote!(<#ty_>::from(self.clone())),
                ),
                SelfType::RefMut => {
                    call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!(&mut x))
                }
            }
        } else {
            call_function_from_sig(self.ty.as_ref(), &cxx_item.sig, quote!())
        };
        if self.return_is_opt {
            call_fn = quote!(#call_fn.map(::std::convert::Into::into).ok_or(()));
        };

        let after_call_fn = if let Some(SelfType::ValueMut) = self_type_opt.as_ref() {
            quote!(*self = Self::from(x);)
        } else {
            TokenStream::new()
        };
        cxx_item.block = parse_quote!({#before_call_fn
            let res = #call_fn;
            #after_call_fn
            res});
        quote!(#cxx_item)
    }
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
        prefix_opt: Option<Ident>, // only set when handling associated functions
    ) {
        if item_fn.sig.asyncness.is_none()
            && item_fn.sig.abi.is_none()
            && matches!(item_fn.vis, Visibility::Public(_))
            // do not handle function with `cfg` attributes for the moment
            && item_fn.attrs.iter().all(|a| !a.path.is_ident("cfg") && !meta_is_extern_fn_skip(a.parse_meta()))
        {
            let mut extern_fn = item_fn.clone();
            trace!("handling fn {:?}", extern_fn.sig.ident);
            if let Some(prefix) = prefix_opt {
                extern_fn.sig.ident = format_ident!("{}{}", prefix, extern_fn.sig.ident);
            }

            self.cxx_fn_buf
                .push(CxxFn::new(extern_fn, self.current_impl_ty.clone()));
        }
    }
}

impl<'ast> Visit<'ast> for GatherSignatures {
    fn visit_item_impl(&mut self, item_impl: &'ast ItemImpl) {
        if item_impl.trait_.is_none()
            && get_ident(&item_impl.self_ty)
                .map_or(false, |ident| self.allowed_idents.contains(&ident))
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
                .and_then(|ty| get_ident_as_function(&ty)),
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
            let unnamed_token = (0..unnameds.unnamed.len())
                .map(syn::Index::from)
                .map(|i| quote!(x.#i));
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
    fn generate_cxx_types(&self) -> TokenStream {
        trace!("Generating FFI struct declarations");
        let mut buf = TokenStream::new();
        for struct_or_enum in self.all_cxx_struct_or_enum.iter() {
            let mut ffi_struct = struct_or_enum.clone();
            let ident_ffi: Ident = format_ident!("{}Ffi", struct_or_enum.ident());
            *ffi_struct.ident_mut() = ident_ffi.clone();
            ffi_struct.to_tokens(&mut buf);
        }
        trace!("Finished generating FFI struct declarations");
        buf
    }

    fn generate_cxx_signatures(&self) -> TokenStream {
        todo!()
    }

    pub fn handle_file(&mut self, parsed_file: &syn::File) -> TokenStream {
        trace!("Starting CreateRawStruct pass");
        let mut create_raw_struct = CreateRawStruct::default();
        create_raw_struct.visit_file(parsed_file);
        trace!("Finished CreateRawStruct pass");
        let (pub_struct_or_enum, idents) = create_raw_struct.results();
        trace!("Starting GatherSignatures pass");
        let mut gather_sig = GatherSignatures::new(idents);
        gather_sig.visit_file(parsed_file);
        trace!("Finished GatherSignatures pass");
        self.all_cxx_fn.extend(gather_sig.cxx_fn_buf);

        let mut parsed_file_tokens = quote!(#parsed_file);
        for struct_or_enum in pub_struct_or_enum.iter() {
            if let StructOrEnum::S(pub_struct) = struct_or_enum {
                let mut raw_struct = pub_struct.clone();
                let ident_raw: Ident = format_ident!("{}Raw", pub_struct.ident);
                raw_struct.ident = ident_raw.clone();
                raw_struct.to_tokens(&mut parsed_file_tokens);
                trace!("Generating conversion impl of {ident_raw}");
                impl_from_x_to_y(&pub_struct.ident, &ident_raw, &raw_struct.fields)
                    .to_tokens(&mut parsed_file_tokens);
                impl_from_x_to_y(&ident_raw, &pub_struct.ident, &raw_struct.fields)
                    .to_tokens(&mut parsed_file_tokens);
                trace!("Finished conversion impl of {ident_raw}");
            }
        }
        self.all_cxx_struct_or_enum.extend(pub_struct_or_enum);
        parsed_file_tokens
    }

    pub fn generate_ffi_bridge_and_impl(self, _code_dir: &Path) {
        todo!()
        // let mut file = File::open(code_dir.join("lib.rs"))
        //     .or_else(|_| {
        //         OpenOptions::new()
        //             .create(true)
        //             .write(true)
        //             .read(true)
        //             .open(code_dir.join("main.rs"))
        //     })
        //     .expect("reading lib.rs and main.rs in src_dir failed");
        // let mut src_file = String::new();
        // file.read_to_string(&mut src_file)
        //     .expect("Unable to read file");
        // let mut parsed_file = syn::parse_file(&src_file).expect("Unable to parse file");
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_cxx_sig() {
        let item_fn = syn::parse_str(
            r#"pub fn get_ident_as_function(ty: &Type) -> Ident{
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_sig = cxx_fn.as_cxx_sig();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function (ty : & Type) -> Ident ;"
        )
    }

    #[test]
    fn test_cxx_sig_opt() {
        let item_fn = syn::parse_str(
            r#"pub fn get_ident_as_function(ty: &Type) -> Option<Ident> {
    todo!()
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_sig = cxx_fn.as_cxx_sig();

        assert_eq!(
            format!("{cxx_sig}"),
            "fn get_ident_as_function (ty : & Type) -> Result < Ident > ;"
        )
    }

    #[test]
    fn test_cxx_impl() {
        let item_fn = syn::parse_str(
            r#"pub fn foo(u: usize) -> usize {
    u+1
    }"#,
        )
        .unwrap();
        let cxx_fn = CxxFn::new(item_fn, None);
        let cxx_impl = cxx_fn.as_cxx_impl();

        assert_eq!(
            format!("{cxx_impl}"),
            "pub fn foo (u : usize) -> usize { let res = foo (u) ; res }"
        )
    }
}
