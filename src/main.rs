#![allow(unused_imports)]

extern crate core;

mod lexer;
mod parser;
mod bruh;

use lexer::lexer;

fn main() {

    let mut tuple_0 = ();
    let mut tuple_1 = (0i64, );
    let mut tuple_2 = (0i64, 0i64);

    let mut tuple_1_lol: (i64, ) = tuple_1;
}

