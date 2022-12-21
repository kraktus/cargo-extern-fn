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
pub fn foo(f: Foo, x: u64) -> bool {
    true
}
fn bar(f: Foo, x: u64) -> bool {
    false
}
fn main() {}
#[no_mangle]
pub extern "C" fn foo_ffi(f: Foo, x: u64) -> bool {
    true
}
