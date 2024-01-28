use naga::front::wgsl::source_provider::SourceProvider;
use naga::front::wgsl::source_provider;
use std::path::Path;
use std::fs;
use naga::front::wgsl::source_provider::File;
use naga::front::wgsl::source_provider::FileId;
use std::path::PathBuf;
use std::collections::HashMap;
use std::cell::UnsafeCell;

#[derive(Debug)]
pub struct FileSources {
  pub(crate) inner: UnsafeCell::<FileSourcesInner>
}

#[derive(Debug)]
pub struct FileSourcesInner {
  pub ids: HashMap<PathBuf, Option<FileId>>, 
  pub files: HashMap<FileId, File>,
  pub counter: u32, 
}

impl FileSourcesInner {
  pub fn ensure(&mut self, path: impl AsRef<Path>) -> Option<FileId> {
    let path = path.as_ref().to_path_buf(); 
    let entry = self.ids.entry(path.clone())
      .or_insert_with(|| {
        self.counter += 1;
        let source = fs::read_to_string(&path).expect("Unable to read file"); 
        self.files.insert(
          self.counter,
          File::new(self.counter, path, source)); 
        Some(self.counter)
      });

    *entry
  }

  // pub fn _insert(&mut self, path: PathBuf, source: String) -> FileId {
  //   let id = self.ensure(path).unwrap();
  //   let file = self.files.get_mut(&id).unwrap();

  //   file.source = source;

  //   id
  // }

  // pub fn _update(&mut self, path: PathBuf) -> FileId {
  //   let id = self.ensure(&path).unwrap();
  //   let file = self.files.get_mut(&id).unwrap();
  //   let source = fs::read_to_string(&path)
  //     .expect("Unable to read string"); 

  //   file.source = source;

  //   id
  // }
}

impl FileSources {
  // pub fn new() -> Self {
  //   Self {
  //     inner: FileSourcesInner {
  //       ids: HashMap::new(),
  //       files: HashMap::new(),
  //       counter: 0
  //     }.into()
  //   }
  // }
}

impl FileSources {
  // pub fn _insert(&self, path: impl AsRef<Path>, source: &str) -> FileId {
  //   unsafe {
  //     let inner = &mut *self.inner.get();

  //     inner.insert(path.as_ref().to_owned(), source.to_owned())
  //   }
  // }

  // pub fn _update(&self, path: impl AsRef<Path>) -> FileId {
  //   unsafe {
  //     let inner = &mut *self.inner.get();

  //     inner.update(path.as_ref().to_owned())
  //   }
  // }

  // pub fn _span_source(&self, span: &naga::Span) -> Option<&str> {
  //   let id = span.file_id?;
  //   let file = self.get(id)?;

  //   Some(file.source())
  // }
}

impl source_provider::SourceProvider<'_> for FileSources {

  fn visit(&self, path: impl AsRef<Path>) -> Option<FileId> {
    unsafe {
      let inner = &mut *self.inner.get();

      inner.ensure(path)
    }
  }

  fn get(&self, id: FileId) -> Option<&source_provider::File> {
    unsafe {
      let inner = &*self.inner.get();

      inner.files.get(&id)
    }
  }
}

impl<'a> source_provider::Files<'a> for FileSources {
  type Source = &'a str;
  type FileId = FileId;
  type Name = &'a str;
  
  fn name(&'a self, file_id: FileId) -> Result<Self::Name, source_provider::Error> {
    let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

    Ok(file.name())
  }

  fn source(&self, file_id: FileId) -> Result<&str, source_provider::Error> {
    let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

    Ok(file.source().as_ref())
  }

  fn line_index(&self, file_id: FileId, byte_index: usize) -> Result<usize, source_provider::Error> {
    let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

    file.line_index((), byte_index)
  }

  fn line_range(&self, file_id: FileId, line_index: usize) -> Result<std::ops::Range<usize>, source_provider::Error> {
    let file = self.get(file_id).ok_or(source_provider::Error::FileMissing)?; 

    file.line_range((), line_index)
  }
}
