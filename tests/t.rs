#[allow(unused_variables, dead_code)]

pub struct Foo;

enum Bar {
    X,
    Y,
}

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
}

pub fn foo(f: Foo, x: u64) -> bool {
    true
}

fn bar(f: Foo, x: u64) -> bool {
    false
}

fn main() {}
