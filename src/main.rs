use std::{
    ffi::{OsStr},
    fs::{DirEntry, File, OpenOptions},
    io::{Read, Write},
    path::PathBuf,
};

use crate::cxx::Cxx;
use clap::{ArgAction, Args, Parser};
use env_logger::Builder;
use log::{debug, info, trace, LevelFilter};

use proc_macro2::Ident;
use quote::format_ident;
use syn::parse_quote;

mod cxx;
mod utils;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    /// automatically set by cargo as first paramater, TODO fix it
    #[arg(default_value = "extern-fn")]
    ghost_value: String,
    #[command(flatten)]
    common: CommonArgs,
}

#[derive(Args, Debug, Clone)]
struct CommonArgs {
    #[arg(short, long, action = ArgAction::Count, default_value_t = 2)]
    verbose: u8,
    #[arg(
        short,
        long,
        default_value = "src/",
        help = "directory to look for the code to be externalised"
    )]
    dir: PathBuf,
    #[arg(
        short,
        long,
        default_value = "foo.rs",
        help = "list of files to ignore, separated by space"
    )]
    ignore: Vec<String>,
    #[arg(
        short = 'n',
        long,
        help = "if set will perform a dry run, returning the modified content of files to the stdout"
    )]
    dry: bool,
}

impl CommonArgs {
    fn entries(&self) -> impl Iterator<Item = DirEntry> + '_ {
        self.dir
            .read_dir()
            .expect("read_dir call failed")
            .filter_map(|entry_res| {
                entry_res.ok().and_then(|entry| {
                    (entry.file_type().expect("file_type failed").is_file()
                        && entry
                            .path()
                            .file_name()
                            .map(|n| n.to_string_lossy())
                            .map_or(true, |n| {
                                let file_name = n.to_string();
                                let extension = file_name.split_once('.').unwrap().1;
                                extension == "rs"
                                    && !self.ignore.contains(&file_name)
                                    && !file_name.contains("ffi")
                            }))
                    .then_some(entry)
                })
            })
    }
}
fn main() {
    let args = Cli::parse();
    let mut builder = Builder::new();
    builder
        .filter(
            None,
            match args.common.verbose {
                0 => LevelFilter::Error,
                1 => LevelFilter::Info,
                2 => LevelFilter::Debug,
                _ => LevelFilter::Trace,
            },
        )
        .default_format()
        .format_timestamp(None)
        .init();

    let mut cxx = Cxx::default();
    trace!("args values: {:?}", &args);

    debug!("looking at... {}", args.common.dir.display());
    debug!("Beginning scanning visit");
    for entry in args.common.entries() {
        info!("scanning file: {:?}", entry.path());
        let mut file = File::open(entry.path()).expect("reading file in src/ failed");
        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");
        let parsed_file = syn::parse_file(&src).expect("Unable to parse file");
        let module = module(&entry);
        cxx.gather_data_struct_and_sign(&parsed_file, module, args.common.dry);
        trace!("Finished handling the file");
    }
    debug!("Finished scanning visit");

    cxx.generate_ffi_bridge_and_impl(&args.common.dir, args.common.dry);
    debug!("Beginning writing visit");
    for entry in args.common.entries() {
        info!("scanning file 2nd time: {:?}", entry.path());
        let mut file = OpenOptions::new()
            .read(true)
            .append(true)
            .open(entry.path())
            .expect("2 reading file in src/ failed");
        let mut src = String::new();
        file.read_to_string(&mut src).expect("Unable to read file");
        let parsed_file = syn::parse_file(&src).expect("Unable to parse file");
        let module = module(&entry);
        let ffi_conversion = cxx.ffi_conversion(&parsed_file, args.common.dry, module);
        trace!("Finished handling the file 2nd time");
        trace!("Conversion raw ts {}", format!("{ffi_conversion}"));
        let ffi_conversion_formated = prettyplease::unparse(&parse_quote!(#ffi_conversion));
        if args.common.dry {
            println!("{src}");
            println!("{ffi_conversion_formated}")
        } else {
            file.write_all(ffi_conversion_formated.as_bytes())
                .expect("appening ffi conversion failed");
        }
    }
    debug!("Finished writing visit");
}

fn module(entry: &DirEntry) -> Ident {
    format_ident!(
        "{}",
        entry
            .path()
            .file_stem()
            .map(OsStr::to_string_lossy)
            .expect("file name without extension exist")
    )
}
