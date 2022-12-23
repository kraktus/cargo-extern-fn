use std::{
    fs::{self, File},
    io::Read,
    path::PathBuf,
};

use cbindgen::Cbindgen;
use clap::{ArgAction, Args, Parser, Subcommand};
use env_logger::Builder;
use log::{debug, info, trace, LevelFilter};
use proc_macro2::TokenStream;

mod cbindgen;
mod cxx;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Cli {
    /// automatically set by cargo as first paramater, TODO fix it
    #[arg(default_value = "extern-fn")]
    ghost_value: String,
    #[command(subcommand)]
    cmd: Cmd,
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

#[derive(Subcommand, Debug)]
enum Cmd {
    Cbindgen(Cbindgen),
    Cxx,
}

impl Cli {
    fn handle_file(&self, file: &mut syn::File) -> TokenStream {
        match &self.cmd {
            Cmd::Cbindgen(cbindgen) => Cbindgen::handle_file(file),
            Cmd::Cxx => todo!(),
        }
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

    debug!("looking at... {}", args.common.dir.display());
    let entries = args.common.dir.read_dir().expect("read_dir call failed");
    for entry_res in entries {
        let entry = entry_res.unwrap();
        if entry.file_type().expect("file_type failed").is_file()
            && entry
                .path()
                .file_name()
                .map(|n| n.to_string_lossy())
                .map_or(true, |n| {
                    let file_name = n.to_string();
                    let extension = file_name.split_once('.').unwrap().1;
                    extension == "rs"
                        && !args.common.ignore.contains(&file_name)
                        && !file_name.contains("ffi")
                })
        {
            info!("scanning file: {:?}", entry.path());
            let mut file = File::open(entry.path()).expect("reading file in src/ failed");
            let mut src = String::new();
            file.read_to_string(&mut src).expect("Unable to read file");
            let mut parsed_file = syn::parse_file(&src).expect("Unable to parse file");
            let parsed_file_tokens = args.handle_file(&mut parsed_file);
            if args.common.dry {
                println!("{parsed_file_tokens}")
            } else {
                fs::write(entry.path(), format!("{parsed_file_tokens}"))
                    .expect("saving code changes failed");
            }
        }
    }
}
