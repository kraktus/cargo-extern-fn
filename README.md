# cargo-extern-fn

A cargo subcommand used to convert rust crate into a set of extern-fns suitable for [cbindgen](https://github.com/eqrion/cbindgen).

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

`extern-fn` returns:

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