use std::fmt::Display;
#[allow(unused_variables, dead_code)]
#[repr(C)]
pub struct Foo;
enum Bar {
    X,
    Y,
}
#[repr(C)]
pub enum Baz {
    A,
    B,
}
impl Baz {
    pub fn is_a(&self) -> bool {
        match self {
            Self::A => true,
            _ => false,
        }
    }
    pub fn to_b(&mut self) {
        *self = Self::B;
    }
    pub fn to_str(self) -> &'static str {
        match self {
            Self::A => "A",
            Self::B => "B",
        }
    }
}
#[repr(C)]
pub struct Gen<T>(T);
impl<T: Display + Copy> Gen<T> {
    pub fn to_string(&self) -> String {
        format!("{}", self.0)
    }
}
pub fn foo(_f: Foo, _x: u64) -> bool {
    true
}
fn bar(_f: Foo, _x: u64) -> bool {
    false
}
fn main() {}
#[no_mangle]
pub extern "C" fn ffi_baz_is_a(self_: &Baz) -> bool {
    <Baz>::is_a(self_)
}
#[no_mangle]
pub extern "C" fn ffi_baz_to_b(self_: &mut Baz) {
    <Baz>::to_b(self_)
}
#[no_mangle]
pub extern "C" fn ffi_baz_to_str(self_: Baz) -> &'static str {
    <Baz>::to_str(self_)
}
#[no_mangle]
pub extern "C" fn ffi_gen_to_string<T: Display + Copy>(self_: &Gen<T>) -> String {
    <Gen<T>>::to_string(self_)
}
#[no_mangle]
pub extern "C" fn ffi_foo(f: Foo, x: u64) -> bool {
    foo(f, x)
}