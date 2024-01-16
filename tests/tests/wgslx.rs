use internal::{compile_module};

#[test]
fn import_twice () {
  internal::compile_module2("tests/import-twice", false).unwrap(); 
}

// #[test]
// fn import_twice_relative () {
//   compile_module("tests/import-twice-relative", false).unwrap(); 
// }
