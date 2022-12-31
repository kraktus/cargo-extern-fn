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
        Self { n0: x.0 }
    }
}
impl From<AgeRaw> for Age {
    fn from(x: AgeRaw) -> Self {
        Self(x.n0)
    }
}
pub struct PersonRaw {
    pub age: Age,
    pub name: String,
}
impl From<Person> for PersonRaw {
    fn from(x: Person) -> Self {
        Self {
            age: x.age,
            name: x.name,
        }
    }
}
impl From<PersonRaw> for Person {
    fn from(x: PersonRaw) -> Self {
        Self {
            age: x.age,
            name: x.name,
        }
    }
}

fn main() {
    unimplemented!();
}

#[cxx::bridge]
pub mod ffi {
    pub struct AgeFfi {
        pub n0: u8,
    }
    pub struct PersonFfi {
        pub age: Age,
        pub name: String,
    }
    /// This is a doc comment!
    pub enum CitizenFfi {
        Adult,
        Minor,
    }
    extern "Rust" {
        fn new(age: u8, name: String) -> Person;
        fn is_adult(self: &PersonFfi) -> bool;
        fn bday(self: &mut PersonFfi);
        fn to_citizen(self: PersonFfi) -> Citizen;
    }
}
