use std::fs;
use std::path::{Path, PathBuf};

use naga::front::wgsl::{parse_str};
use naga::valid::{Validator, ValidationFlags, Capabilities};
use tower_lsp::{lsp_types::*, jsonrpc};
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct WgslxLanguageServer {
  client: Client,
}


#[tower_lsp::async_trait]
impl LanguageServer for WgslxLanguageServer {
  
  //--------------------------------------------------------------------------
  //
  //  Lifecycle
  //
  //--------------------------------------------------------------------------

  async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult, jsonrpc::Error> {
    eprintln!("Initializing {:#?}", params);
    
    Ok(InitializeResult {
      capabilities: ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
          TextDocumentSyncKind::FULL
        )),
        ..Default::default()
      }, 
      ..Default::default()
    })
  }

  async fn initialized(&self, _params: InitializedParams) {
    self.client.log_message(MessageType::INFO, "DID server initialized!").await;
  }

  async fn shutdown(&self) -> Result<(), jsonrpc::Error> {
    Ok(())
  }

  async fn did_open(&self, params: DidOpenTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "file opened!"); 
  }

  async fn did_change(&self, params: DidChangeTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "changed!").await;

    let path = Path::new(params.text_document.uri.path());
    let other_file_path = path.parent()
      .and_then(|path| Some(path.join(PathBuf::from("other.wgsl")))).unwrap(); 
     
    // eprintln!("{:#?} Path: {:?}", params.text_document, other_file_path); 

    let other_file_text = fs::read_to_string(other_file_path).expect("Unable to parse other file");

    // For now, we get back the entire source as we only support TextDocumentSyncKind::Full
    // let mut text = params.content_changes[0].text.clone();
    let line_start = other_file_text.as_bytes().iter().filter(|&&c| c == b'\n').count() as u32 + 1;
    let text = vec![other_file_text, params.content_changes[0].text.clone()].join("\n");

    // eprintln!("text: {:?}", text); 

    let mut diagnostics: Vec<Diagnostic> = Vec::new();

    if let Err(error) = self.diagnostics(&text) {
      match error {
        Error::Parse(error) => {
          let labels = error.labels();

          for (span, _label) in labels {
            let location = span.location(&text);

            let start = Position { line: location.line_number - 1 - line_start, character: location.line_position - 1 };
            let end = Position { line: location.line_number - 1 - line_start, character: location.line_position - 1 + location.length }; 
            
            diagnostics.push(Diagnostic::new_simple(
              Range::new(start, end),
              format!("[Parse] {}", error.emit_to_string(&text))
            ));         
          }    
        },
        Error::Validation(error) => {
          // Naga validation errors include a vector of spans (with labels, such as "naga::Expression [8]")
          // for marking code, as well as the actual error message which to view in it's entirety requires traversing
          // errors sources
          //
          // For instance, for a function Foo with an expression pow(1, 2.), naga will return a
          // 
          //   ValidationError::Function = Function [6] 'Foo' is invalid:
          //     source: FunctionError::Expression =  Expression [8] is invalid
          //       source: ExpressionError::InvalidArgumentType = Argument [0] to Pow as expression [4] has an invalid type.
          // 
          // We ignore everything but the inner error, and associate it with the last span
          if let Some((span, _label)) = error.spans().last() {
            // Get the most inner error
            let mut source: &dyn std::error::Error = &error.as_inner();
            while let Some(next) = std::error::Error::source(source) {
              source = next;
            }

            let location = span.location(&text);
            let start = Position { line: location.line_number - 1 - line_start, character: location.line_position - 1 };
            let end = Position { line: location.line_number - 1 - line_start, character: location.line_position - 1 + location.length };

            // Append with the debug name of the enum to get a better sense of what the error is about
            // Should we remove everything after the parens? (e.g., InvalidArgumentType(Pow, 0, [4]))
            let mut message = format!("{:?}: ", source);

            message.push_str(&source.to_string()); 

            diagnostics.push(Diagnostic::new_simple(
              Range::new(start, end),
              format!("[Validation] {}", message)
            ));           
          }
        }
      }
    }

    self.client.publish_diagnostics(params.text_document.uri, diagnostics, None).await; 
  }

  async fn did_save(&self, params: DidSaveTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "saved!").await;
  }
}

enum Error {
  Parse(naga::front::wgsl::ParseError) ,
  Validation(naga::WithSpan<naga::valid::ValidationError>), 
}

impl From<naga::front::wgsl::ParseError> for Error {
  fn from(value: naga::front::wgsl::ParseError) -> Self {
    Self::Parse(value)
  }
}

impl From<naga::WithSpan<naga::valid::ValidationError>> for Error {
  fn from(value: naga::WithSpan<naga::valid::ValidationError>) -> Self {
    Self::Validation(value)
  }
}

impl WgslxLanguageServer {
  fn diagnostics(&self, source: &str) -> Result<(), Error>{
    let module = parse_str(source)?;

    eprintln!("Parsed Module {:#?}", module);
    
    let mut validator = Validator::new(ValidationFlags::all(), Capabilities::all());

    validator.validate(&module)?; 

    Ok(())
  }
}

#[tokio::main]
async fn main() {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| WgslxLanguageServer { client });
  Server::new(stdin, stdout, socket).serve(service).await;
}
