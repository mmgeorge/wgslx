use internal::{compile_module};

#[test]
fn import_twice () {
  compile_module("tests/import-twice/lib.wgslx", false).unwrap(); 
}
