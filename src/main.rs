use std::{fs::File, io::Read, path::PathBuf};

use clap::{ArgAction, Parser};
use syn::{
    parse_quote,
    visit_mut::{self, VisitMut},
    Data, Expr, Lit, LitInt, Type,
};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    #[arg(long, default_value = "./")]
    dir: PathBuf,
}

// add #[repr(C)]
// to all public types
struct AddReprC;

impl VisitMut for AddReprC {
    fn visit_data_mut(&mut self, data: &mut Data) {
        // Delegate to the default impl to visit nested expressions.
        visit_mut::visit_data_mut(self, data);
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
