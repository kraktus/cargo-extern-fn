#[allow(unused_variables, dead_code)]
#[repr(C)]
pub struct Person {
    age: usize,
    name: String,
}
/// This is a doc comment!
#[repr(C)]
pub enum Citizen<T> {
    Adult(T),
    Minor,
}
impl Person {
    pub fn new(age: usize, name: String) -> Person {
        Self { age, name }
    }
    pub fn is_adult(&self) -> bool {
        self.age >= 18
    }
    pub fn bday(&mut self) {
        self.age += 1;
    }
    pub fn to_citizen(self) -> Citizen<Person> {
        if self.is_adult() { Citizen::Adult(self) } else { Citizen::Minor }
    }
    /// extern_fn:skip
    pub fn name(&self) -> &str {
        &self.name
    }
}
fn main() {
    let p = Person::new(45, "john".to_string());
    println!("{}", p.name())
}
#[no_mangle]
pub extern "C" fn ffi_person_new(age: usize, name: String) -> Person {
    <Person>::new(age, name)
}
#[no_mangle]
pub extern "C" fn ffi_person_is_adult(self_: &Person) -> bool {
    <Person>::is_adult(self_)
}
#[no_mangle]
pub extern "C" fn ffi_person_bday(self_: &mut Person) {
    <Person>::bday(self_)
}
#[no_mangle]
pub extern "C" fn ffi_person_to_citizen(self_: Person) -> Citizen<Person> {
    <Person>::to_citizen(self_)
}

