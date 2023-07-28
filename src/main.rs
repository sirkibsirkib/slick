mod ast;
mod parse;

fn main() {
    let x = b"A says X :- A likes B, B says X.";
    let r = parse::wsr(parse::rule)(x);
    println!("{:#?}", r);
}
