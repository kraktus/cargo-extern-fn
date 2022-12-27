pub struct Age(u8);
pub struct Person {
    age: Age,
    name: String,
}
/// This is a doc comment!
pub enum Citizen<T> {
    Adult(T),
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
    pub fn to_citizen(self) -> Citizen<Person> {
        if self.is_adult() {
            Citizen::Adult(self)
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
pub struct AgeRaw(pub u8);
impl From<Age> for AgeRaw {
    fn from(x: Age) -> Self {
        Self(x.0)
    }
}
impl From<AgeRaw> for Age {
    fn from(x: AgeRaw) -> Self {
        Self(x.0)
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
