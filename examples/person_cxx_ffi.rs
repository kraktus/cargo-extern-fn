pub struct Age(u8);
pub struct Person {
    age: Age,
    name: String,
}
/// This is a doc comment!
pub enum Citizen {
    Adult,
    Minor,
}
impl Person {
    pub fn new(age: u8, name: String) -> Person {
        Self {
            age: Age(age),
            name,
        }
    }
    pub fn is_adult(&self) -> bool {
        self.age.0 >= 18
    }
    pub fn bday(&mut self) {
        self.age.0 += 1;
    }
    pub fn to_citizen(self) -> Citizen {
        if self.is_adult() {
            Citizen::Adult
        } else {
            Citizen::Minor
        }
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
pub struct AgeRaw {
    pub n0: u8,
}
impl From<Age> for AgeRaw {
    fn from(x: Age) -> Self {
        Self { n0: x.0.into() }.into()
    }
}
impl From<AgeRaw> for Age {
    fn from(x: AgeRaw) -> Self {
        Self(x.n0.into()).into()
    }
}
pub struct PersonRaw {
    pub age: Age,
    pub name: String,
}
impl From<Person> for PersonRaw {
    fn from(x: Person) -> Self {
        Self {
            age: x.age.into(),
            name: x.name.into(),
        }
        .into()
    }
}
impl From<PersonRaw> for Person {
    fn from(x: PersonRaw) -> Self {
        Self {
            age: x.age.into(),
            name: x.name.into(),
        }
        .into()
    }
}

// Auto generated code with `cargo-extern-fn`
// #[cxx::bridge]
// pub mod ffi {
//     pub struct AgeFfi {
//         pub n0: u8,
//     }
//     pub struct PersonFfi {
//         pub age: AgeFfi,
//         pub name: String,
//     }
//     /// This is a doc comment!
//     pub enum CitizenFfi {
//         Adult,
//         Minor,
//     }
//     extern "Rust" {
//         fn new(age: u8, name: String) -> PersonFfi;
//         fn is_adult(self: &PersonFfi) -> bool;
//         fn bday(self: &mut PersonFfi);
//         fn to_citizen(self: PersonFfi) -> CitizenFfi;
//     }
// }
