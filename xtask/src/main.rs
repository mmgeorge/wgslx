use std::{
  env,
  error::Error,
  fs,
  path::{Path, PathBuf},
  process::Command,
};

type DynError = Box<dyn Error>;
type Result<T> = std::result::Result<T, DynError>;

fn main() -> Result<()> {
  if let Err(error) = try_main() {
    eprintln!("{}", error);
    std::process::exit(1);
  }

  Ok(())
}

fn try_main() -> Result<()> {
  let task = env::args().nth(1);

  match task.as_deref() {
    Some("vscode") => try_vscode(),
    _ => Ok(print_help()),
  }
}

fn try_vscode() -> Result<()> {
  let root = project_root();
  println!("Compiling language server at {:#?}", root);

  let cargo = env::var("CARGO").unwrap_or_else(|_| "cargo".to_string());

  // Currently only build debug
  // Command::new(cargo).current_dir(&root).args(&["build", "--release"]).status()?;
  Command::new(cargo).current_dir(&root).args(&["build"]).status()?;

  let out_path = root.join("editors/vscode/out");

  if out_path.exists() {
    fs::remove_dir_all(&out_path)?;
  }

  println!("Moving executable to editor output directory {:#?}", out_path);
  fs::create_dir(&out_path)?;
  fs::rename(root.join("target/debug/wgslx-lsp"), out_path.join("wgslx-lsp"))?;

  println!("Packaging extension");
  Command::new("npx")
    .current_dir(&root.join("editors/vscode"))
    .args(&["vsce", "package", "--skip-license", "--out", "out/"])
    .status()?;

  Ok(())
}

fn print_help() {
  eprintln!(
    "Tasks: 
  
  vscode       Builds an extension for vscode"
  )
}

fn project_root() -> PathBuf {
  Path::new(&env!("CARGO_MANIFEST_DIR")).ancestors().nth(1).unwrap().to_path_buf()
}
