use crate::List::{Cons, Nil};

fn main() {
    let b = Box::new(5);
    println!("b = {}", b);

    let list = Cons(1, Cons(2, Cons(3, Nil)));

}

enum List {
    Cons(i32, List),
    Nil,
}

