use std::fs;
use std::{fs::File, io::Read, path::PathBuf};

use clap::Parser;
use syn::parse::{Parse, ParseStream};
use syn::visit::{self, Visit};
use syn::{
    FnArg, ImplItemMethod, ItemFn, ItemImpl, Pat, PatIdent, PatType,
    Signature,
};
use syn::{
    visit_mut::{self, VisitMut},
    Attribute, ItemEnum, ItemStruct, Type, Visibility,
};

use quote::{format_ident, quote, ToTokens};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(default_value = "src/", help = "directory to look for the code to modify")]
    dir: PathBuf,
    #[arg(default_value = "foo.rs", help = "list of files to ignore, separated by space")]
    ignore: Vec<String>,
    #[arg(short = 'n', long, help = "if true will perform a dry run, returning the files to the stdout")]
    dry: bool,
}

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
}
// for each file, add at the end of it externalised fn
// regular `pub fn foo(arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn foo_ffi(arg1: X, arg2: &Y) -> bool`
// method `pub fn foo_method(&self,arg1: X, arg2: &Y) -> bool`
// are converted to `#[no_mangle] pub extern "C" fn foo_method_ffi(self_: &Foo,arg1: X, arg2: &Y) -> bool`
#[derive(Debug, Clone, Default)]
struct ExternaliseFn {
    // only set if not a trait method
    current_impl_ty: Option<Type>,
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
    fn handle_item_fn(&mut self, item_fn: &ItemFn) {
        if item_fn.sig.asyncness.is_none()
            && item_fn.sig.abi.is_none()
            && matches!(item_fn.vis, Visibility::Public(_))
            && item_fn.attrs.is_empty()
        // let's start simple by not handling fn with attributes
        {
            let mut extern_fn = item_fn.clone();
            extern_fn.attrs.push(outer_attr("#[no_mangle]"));
            extern_fn.sig.abi = Some(syn::parse_str(r#"extern "C""#).unwrap());
            extern_fn.sig.ident = format_ident!("{}_ffi", extern_fn.sig.ident);
            for arg in extern_fn.sig.inputs.iter_mut() {
                if let FnArg::Receiver(rec) = arg {
                    let name = self
                        .current_impl_ty
                        .as_ref()
                        .expect("Method not in an struct/enum impl");
                    let fn_arg: FnArg =
                        syn::parse2(match (rec.reference.clone(), rec.mutability) {
                            (None, None) => quote!(self_: #name),
                            (None, Some(_)) => quote!(self_: mut #name),
                            (Some((_, lifetime_opt)), None) => {
                                let lifetime =
                                    lifetime_opt.map(|lt| quote!(#lt)).unwrap_or_default();
                                quote!(self_: &#lifetime #name)
                            }
                            (Some((_, lifetime_opt)), Some(_)) => {
                                let lifetime =
                                    lifetime_opt.map(|lt| quote!(#lt)).unwrap_or_default();
                                quote!(self_: &#lifetime mut #name)
                            }
                        })
                        .unwrap();
                    *arg = fn_arg;
                }
            }
            // the body of the function should just be calling the original function
            extern_fn.block = syn::parse2(self.call_function_from_sig(&item_fn.sig)).unwrap();

            self.externalised_fn_buf.push(extern_fn);
        }
    }

    fn call_function_from_sig(&self, sig: &Signature) -> proc_macro2::TokenStream {
        let fn_ident = format!(
            "{}{}",
            self.current_impl_ty
                .as_ref()
                .map(|ty| quote!(<#ty>::).to_string())
                .unwrap_or_default(),
            sig.ident
        );
        let mut args_buf = proc_macro2::TokenStream::new();
        let mut iter_peek = sig.inputs.iter().peekable();
        while let Some(arg) = iter_peek.next() {
            match arg {
                FnArg::Receiver(_) => quote!(self_).to_tokens(&mut args_buf),
                FnArg::Typed(PatType { pat, .. }) => {
                    if let Pat::Ident(PatIdent { ident, .. }) = (**pat).clone() {
                        quote!(#ident).to_tokens(&mut args_buf)
                    }
                }
            }
            if iter_peek.peek().is_some() {
                quote!(,).to_tokens(&mut args_buf)
            }
        }

        format!("{{ {fn_ident}({args_buf}) }}").parse().unwrap()
    }
}

impl<'ast> Visit<'ast> for ExternaliseFn {
    fn visit_item_impl(&mut self, item_impl: &'ast ItemImpl) {
        if item_impl.trait_.is_none() {
            self.current_impl_ty = Some(*item_impl.self_ty.clone());
        } else {
            self.current_impl_ty = None
        }
        visit::visit_item_impl(self, item_impl);
    }

    fn visit_item_fn(&mut self, item_fn: &'ast ItemFn) {
        self.current_impl_ty = None;
        self.handle_item_fn(item_fn);
        visit::visit_item_fn(self, item_fn);
    }

    fn visit_impl_item_method(&mut self, item_method: &'ast ImplItemMethod) {
        let item_fn: ItemFn =
            syn::parse2(item_method.to_token_stream()).expect("from method to bare fn failed");
        self.handle_item_fn(&item_fn);
        visit::visit_impl_item_method(self, item_method);
    }
}

fn main() {
    let args = Cli::parse();
    println!("looking at... {}", args.dir.display());
    let entries = args.dir.read_dir().expect("read_dir call failed");
    for entry_res in entries {
        let entry = entry_res.unwrap();
        if entry.file_type().expect("file_type failed").is_file()
            && entry
                .path()
                .file_name()
                .map(|n| n.to_string_lossy())
                .map_or(true, |n| !args.ignore.contains(&n.to_string()))
        {
            let mut file = File::open(entry.path()).expect("reading file in src/ failed");
            let mut src = String::new();
            file.read_to_string(&mut src).expect("Unable to read file");
            let mut parsed_file = syn::parse_file(&src).expect("Unable to parse file");
            AddReprC.visit_file_mut(&mut parsed_file);
            let mut externalised_fn = ExternaliseFn::default();
            externalised_fn.visit_file(&parsed_file);
            let mut parsed_file_tokens = quote!(#parsed_file);
            externalised_fn.to_tokens(&mut parsed_file_tokens);
            if args.dry {
                println!("{parsed_file_tokens}")
            } else {
                fs::write(entry.path(), format!("{parsed_file_tokens}")).expect("saving code changes failed");
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_call_function_from_sig() {
        let sig: Signature = syn::parse_str("fn foo(f: Foo, x: u64) -> bool").unwrap();
        let ext = ExternaliseFn::default();
        assert_eq!(
            "{ foo (f , x) }",
            format!("{}", ext.call_function_from_sig(&sig))
        )
    }

    #[test]
    fn test_call_method_from_sig() {
        let sig: Signature = syn::parse_str("fn foo(&self) -> u8").unwrap();
        let mut ext = ExternaliseFn::default();
        ext.current_impl_ty = Some(syn::parse_str("bar::Bar").unwrap());
        assert_eq!(
            "{ < bar :: Bar > :: foo (self_) }",
            format!("{}", ext.call_function_from_sig(&sig))
        )
    }
}
