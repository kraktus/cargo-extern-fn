#[derive(Clone)]
pub struct Age(u8);

// this is a comment
#[derive(Clone)]
pub struct Person {
    age: Age,
    name: String,
}

/// This is a doc comment!
#[derive(Clone)]
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
        self.age.0 += 1
    }

    /// extern_fn:skip
    // by value not supported for the moment
    pub fn to_citizen(self) -> Citizen {
        if self.is_adult() {
            Citizen::Adult
        } else {
            Citizen::Minor
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

#[inline]
#[must_use]
pub fn foo(u: usize) -> u8 {
    u.try_into().unwrap()
}

fn main() {
    let p = Person::new(45, "john".to_string());
    println!("{}", p.name())
}
