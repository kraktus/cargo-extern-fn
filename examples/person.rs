#[derive(Clone)]
pub struct Age(u8);

impl Age {
    pub fn is_old(&self) -> bool {
        self.0 >= 90
    }
}

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

// should not be included in the bridge
pub struct NoAnswer;

impl Citizen {
    pub fn is_adult(self) -> bool {
        matches!(self, Self::Adult)
    }
}

impl Person {
    pub fn new(age: u8, name: String) -> Person {
        Self {
            age: Age(age),
            name,
        }
    }

    // function skipped because bridge does not support tuples
    pub fn new_from_tuple((age, name): (u8, String)) -> Person {
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

    // function skipped because bridge does not support tuples
    pub fn name_and_age(self) -> (String, Age) {
        (self.name, self.age)
    }

    pub fn ask_age(&self) -> Result<Age, NoAnswer> {
        Ok(self.age.clone())
    }

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

// should be turned into `unsafe_bar`
pub unsafe fn bar(u: usize) -> u8 {
    u.try_into().unwrap()
}

fn main() {
    let p = Person::new(45, "john".to_string());
    println!("{}", p.name())
}
