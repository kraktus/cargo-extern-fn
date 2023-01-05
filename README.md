# cargo-extern-fn

A cargo subcommand to be used as a pre-processor to various FFI generation libs from a rust crate. Currently support:
- [cxx](https://cxx.rs/index.html): generate `cxx::bridge` from the public APIs.

Use the `/// extern_fn:skip` like an attribute to skip items.

## Status

This tool is very much a work and progress and it is expected to polish the FFI boundaries by hand afterwards.

## Installation

`cargo install cargo-extern-fn`

## Usage

`cargo extern-fn --dir [SRC_DIR] && cargo fmt`