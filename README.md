# cargo-extern-fn

A cargo subcommand to be used as a pre-processor to various FFI generation libs from a rust crate. Currently support:
- [cbindgen](https://github.com/eqrion/cbindgen): generate extern fns from the public APIs.
- [cxx](https://cxx.rs/index.html): generate `cxx::bridge` from the public APIs.

Use the `/// extern_fn:skip` like an attribute to skip items.

## Status

This tool is very much a work and progress and it is expected to polish the FFI boundaries by hand afterwards.

## Installation

`cargo install cargo-extern-fn`

## Usage

`cargo extern-fn --dir [SRC_DIR] && cargo fmt`

## Example

With the following input
```rust
// foo.rs

pub enum Baz {
    A,
    B,
}

impl Baz {
    pub fn to_b(&mut self) {
        *self = Self::B;
    }

    /// extern_fn:skip
    pub fn foo() -> String {
        "foo".into()
    }
}

pub struct Gen<T>(T);

impl<T: Display + Copy> Gen<T> {
    pub fn to_string(&self) -> String {
        format!("{}", self.0)
    }
}

pub fn foo(f: Foo, x: u64) -> bool {
    true
}
```

`extern-fn cbdingen` returns:
Note that you have to manually clean up the functions that are not FFI-safe afterwards. PRs to help not select incompatible are welcome.
```rust
// foo.rs


#[repr(C)]
pub enum Baz {
    A,
    B,
}
impl Baz {
    pub fn to_b(&mut self) {
        *self = Self::B;
    }

    /// extern_fn:skip
    pub fn foo() -> String {
        "foo".into()
    }
}
#[repr(C)]
pub struct Gen<T>(T);
impl<T: Display + Copy> Gen<T> {
    pub fn to_string(&self) -> String {
        format!("{}", self.0)
    }
}
pub fn foo(f: Foo, x: u64) -> bool {
    true
}

#[no_mangle]
pub extern "C" fn ffi_baz_to_b(self_: &mut Baz) {
    <Baz>::to_b(self_)
}
#[no_mangle]
pub extern "C" fn ffi_gen_to_string<T: Display + Copy>(self_: &Gen<T>) -> String {
    <Gen<T>>::to_string(self_)
}
```