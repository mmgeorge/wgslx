use std::ops::Range;
use codespan_reporting::files;
use codespan_reporting::files::Files;
use naga::front::wgsl::source_provider::SourceProvider;
use std::cell::UnsafeCell;
use std::fs;
use std::path::Path;
use naga::front::wgsl::source_provider::File;
use naga::front::wgsl::source_provider::FileId;
use std::path::PathBuf;
use std::collections::HashMap;

pub struct FileProvider {
  pub inner: UnsafeCell<FileProviderInner>
}

impl FileProvider {
  pub fn new() -> Self {
    Self {
      inner: FileProviderInner::new().into()
    }
  }
}

pub struct FileProviderInner {
  pub paths: HashMap<PathBuf, FileId>,
  pub files: HashMap<FileId, File>,
  pub id_counter: FileId, 
}

impl FileProviderInner {
  pub fn new() -> Self {
    Self {
      files: HashMap::new(),
      paths: HashMap::new(),
      id_counter: 0,
    }
  }

  pub fn visit(&mut self, path: impl AsRef<Path>) -> Option<FileId> {
    let path = path.as_ref(); 
    let id_entry = self.paths.entry(path.to_path_buf())
      .or_insert_with(|| {
        self.id_counter += 1;
        let source = fs::read_to_string(path).expect("Unable to parse file"); 
        self.files.insert(self.id_counter, File::new(self.id_counter, path.to_path_buf(), source)); 
        self.id_counter
      });

    Some(*id_entry)
  }

  pub fn get(&self, id: FileId) -> Option<&File> {
    self.files.get(&id)
  }
}

impl SourceProvider<'_> for FileProvider {
  fn visit(&self, path: impl AsRef<Path>) -> Option<FileId> {
    // SAFETY: We never remove keys from the hashmap, nor remove them. All mutability
    // happens just for caching values
    unsafe {
      let inner = &mut *self.inner.get(); 

      inner.visit(path)
    }
  }

  fn get(&self, id: FileId) -> Option<&naga::front::wgsl::source_provider::File> {
    // SAFETY: No mutabability
    unsafe {
      let inner = &*self.inner.get();

      inner.get(id)
    }
  }
}

impl<'a> Files<'a> for FileProvider {
  type Source = &'a str;
  type FileId = FileId;
  type Name = &'a str;
  
  fn name(&'a self, file_id: FileId) -> Result<Self::Name, files::Error> {
    let file = self.get(file_id).ok_or(files::Error::FileMissing)?; 

    Ok(file.name())
  }

  fn source(&self, file_id: FileId) -> Result<&str, files::Error> {
    let file = self.get(file_id).ok_or(files::Error::FileMissing)?; 

    Ok(file.source())
  }

  fn line_index(&self, file_id: FileId, byte_index: usize) -> Result<usize, files::Error> {
    let file = self.get(file_id).ok_or(files::Error::FileMissing)?; 

    file.line_index((), byte_index)
  }

  fn line_range(&self, file_id: FileId, line_index: usize) -> Result<Range<usize>, files::Error> {
    let file = self.get(file_id).ok_or(files::Error::FileMissing)?; 

    file.line_range((), line_index)
  }
}
