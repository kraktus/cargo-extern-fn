#[allow(unused_variables, dead_code)]

// this is a comment
pub struct Person {
    age: usize,
    name: String,
}

/// This is a doc comment!
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
        self.age += 1
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
