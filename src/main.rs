use std::{fs::File, io::Read, path::PathBuf};

use clap::{ArgAction, Parser};
use syn::{
    parse_quote,
    visit_mut::{self, VisitMut},
    Data, Expr, ItemEnum, Lit, LitInt, Type, Visibility, Attribute,
};
use syn::parse::{Parse, ParseStream};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(long, default_value = "./")]
    dir: PathBuf,
}

// add #[repr(C)]
// to all public types
struct AddReprC;

const REPR_C: &'static str = "#[repr(C)]";

struct ReprC(pub Vec<Attribute>);

impl Parse for ReprC {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self(input.call(Attribute::parse_outer)?))
    }
}

impl VisitMut for AddReprC {

    fn visit_item_enum_mut(&mut self, enum_: &mut ItemEnum) {
        if matches!(enum_.vis, Visibility::Public(_))
            && enum_.attrs.iter().all(|a| !a.path.is_ident("repr"))
        {
            let repr_c_attr: ReprC = syn::parse_str(REPR_C).unwrap();
            assert_eq!(repr_c_attr.0.len(), 1);
            enum_.attrs.extend(repr_c_attr.0);
        }
        visit_mut::visit_item_enum_mut(self, enum_);
    }
}

fn main() {
    let args = Cli::parse();
    let entries = args
        .dir
        .join("src")
        .read_dir()
        .expect("read_dir call failed");
    for entry_res in entries {
        let entry = entry_res.unwrap();
        if entry.file_type().expect("file_type failed").is_file() {
            let mut file = File::open(entry.path()).expect("reading file in src/ failed");
            let mut src = String::new();
            file.read_to_string(&mut src).expect("Unable to read file");
            let mut parsed_file = syn::parse_file(&src).expect("Unable to parse file");
            AddReprC.visit_file_mut(&mut parsed_file);
        }
    }
}
