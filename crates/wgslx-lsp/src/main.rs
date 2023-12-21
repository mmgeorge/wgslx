use std::path::{Path};
use change_state::ChangeState;
pub use file_sources::FileSources;
use module::{Module};
use module::search_position::SearchPosition;
use naga::front::wgsl::source_provider::{SourceProvider};
use tower_lsp::{lsp_types::*, jsonrpc};
use tower_lsp::{Client, LanguageServer, LspService, Server};

mod change_state; 


struct WgslxLanguageServer {
  client: Client,
  changes: ChangeState,

  sources_saved: FileSources, 
  sources_changed: FileSources, 
}

unsafe impl Send for WgslxLanguageServer {}
unsafe impl Sync for WgslxLanguageServer {}

mod file_sources;
mod module;

impl WgslxLanguageServer {

  fn do_goto(&self, pos: SearchPosition) -> Option<GotoDefinitionResponse> {
    let sources = &self.sources_saved; 
    let module = Module::from(sources, pos.file_id).ok()?;
    let definition = module.find_definition_at(sources, pos)?;
    let span = definition.get_span(&module); //module.find_span_at(sources, pos)?;

    // We may get back an empty span, created with Span::default(), e.g., in the case of a standard type
    // being returned from the types Arena
    let id = span.file_id?; 
    let source = sources.get(id).unwrap().source(); 
    let location = span.location(source);

    let start = Position { line: location.line_number - 1, character: location.line_position - 1 };
    let end = Position { line: location.line_number - 1, character: location.line_position - 1 + location.length };

    let file = sources.get(id);
    let path = Url::from_file_path(file.unwrap().path()).unwrap();
    
    Some(GotoDefinitionResponse::Scalar(Location::new(path, Range::new(start, end))))
  }

  fn do_hover(&self, pos: SearchPosition) -> Option<Hover> {
    let module = Module::from(&self.sources_saved, pos.file_id).ok()?;
    let hoverable = module.find_hoverable_at(&self.sources_saved, pos)?;
    let contents = hoverable.get_hoverable_text(module.as_inner()); 
    let hover = Hover {
      contents: HoverContents::Scalar(MarkedString::String(contents)),
      range: None
    }; 
    
    Some(hover)
  }

  fn do_complete(&self, pos: SearchPosition, pos_changed: SearchPosition) -> Option<CompletionResponse> {
    let module = Module::from(&self.sources_saved, pos.file_id).ok()?;
    let candidates = module.find_completion_candidates_at2(&self.sources_saved, &self.sources_changed, pos, pos_changed)?;
    let response = CompletionResponse::Array(
      candidates.into_iter().map(Into::into).collect());

    Some(response)
  }
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
        workspace: Some(WorkspaceServerCapabilities {
          file_operations: None,
          workspace_folders: Some(WorkspaceFoldersServerCapabilities {
            change_notifications: Some(OneOf::Left(true)),
            supported: Some(true), 
          })
        }),
        definition_provider: Some(OneOf::Left(true)),
        diagnostic_provider: Some(DiagnosticServerCapabilities::Options(DiagnosticOptions {
          inter_file_dependencies: true,
          ..Default::default()
        })),
        position_encoding: Some(PositionEncodingKind::UTF8),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        completion_provider: Some(CompletionOptions {
          ..Default::default()
        }),
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
    self.client.log_message(MessageType::INFO, "file opened!").await; 
  }

  async fn did_change(&self, params: DidChangeTextDocumentParams) {
    self.client.log_message(MessageType::INFO, format!("changed! {}", params.text_document.uri)).await;

    self.sources_changed.insert(params.text_document.uri.path(), &params.content_changes[0].text); 
    // self.changes.insert(params);
  }

  async fn did_save(&self, params: DidSaveTextDocumentParams) {
    self.client.log_message(MessageType::INFO, "saved!").await;

    let id = self.sources_saved.update(params.text_document.uri.path());
    let diagnostics = Module::diagnostics(&self.sources_saved, id)
      .into_iter()
      .filter(|diagnostic| diagnostic.span.file_id == Some(id))
      .map(|diagnostic| diagnostic.to_lsp(&self.sources_saved))
      .collect();

    self.client.publish_diagnostics(params.text_document.uri, diagnostics, None).await; 
  }
  
  async fn goto_definition(&self, params: GotoDefinitionParams) -> Result<Option<GotoDefinitionResponse>, jsonrpc::Error> {
    self.client.log_message(MessageType::INFO, format!("Request definition! {:#?}", params)).await;

    let uri = params.text_document_position_params.text_document.uri; 
    let id = self.sources_saved.visit(uri.path()).unwrap();
    let pos = SearchPosition::new(&self.sources_saved, params.text_document_position_params.position, id);
    let response = self.do_goto(pos);
    
    Ok(response)
  }

  async fn hover(&self, params: HoverParams) -> Result<Option<Hover>, jsonrpc::Error> {
    eprintln!("{:?}", params);

    let uri = params.text_document_position_params.text_document.uri; 
    let id = self.sources_saved.visit(Path::new(uri.path())).unwrap();
    let pos = SearchPosition::new(&self.sources_saved, params.text_document_position_params.position, id);
    let hover = self.do_hover(pos);

    Ok(hover)
  }

  async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>, jsonrpc::Error> {
    let uri = params.text_document_position.text_document.uri; 
    let id = self.sources_saved.visit(uri.path()).unwrap();
    let pos = SearchPosition::new(&self.sources_saved, params.text_document_position.position, id);

    let id_changed = self.sources_changed.visit(uri.path()).unwrap();
    let pos_changed = SearchPosition::new(&self.sources_saved, params.text_document_position.position, id_changed);
    
    let response = self.do_complete(pos, pos_changed);
    
    Ok(response)
  }
}

#[tokio::main]
async fn main() {
  let stdin = tokio::io::stdin();
  let stdout = tokio::io::stdout();

  let (service, socket) = LspService::new(|client| WgslxLanguageServer {
    client,
    changes: ChangeState::new(),
    sources_changed: FileSources::new(), 
    sources_saved: FileSources::new()
  });
  
  Server::new(stdin, stdout, socket).serve(service).await;
}
