
#[test]
fn import_twice () {
  internal::compile_module("tests/import-twice", false).unwrap(); 
}

#[test]
fn import_twice_relative () {
  internal::compile_module("tests/import-twice-nested", false).unwrap(); 
}
