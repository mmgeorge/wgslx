import path from 'path';
import fs from "fs";
import { promisify } from 'util';
import * as vscode from 'vscode';

import { ServerOptions, BaseLanguageClient, LanguageClient, LanguageClientOptions, TransportKind } from "vscode-languageclient/node";

let client: LanguageClient | null = null

export function activate(context: vscode.ExtensionContext) {
  console.log("Extension activated");
  let serverModule = getLanguageServerPath(context);

  // let disposable = vscode.commands.registerCommand('wgslx.hello', () => {
  //   vscode.window.showInformationMessage("Hi wgslx");
  // });
  // context.subscriptions.push(disposable);

  let serverOptions: ServerOptions = {
    run: { command: serverModule, transport: TransportKind.stdio },
    debug: {
      command: serverModule,
      transport: TransportKind.stdio,
    }
  };

  let clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "wgslx" }],
    synchronize: {
      // Notify the server about file changes to '.clientrc files contained in the workspa
      fileEvents: vscode.workspace.createFileSystemWatcher('**/.clientrc')
    }
  };

  client = new LanguageClient(
    "wgslx",
    serverOptions,
    clientOptions
  );

  client.start()
    .catch((error) => console.error("Got error when establishing connection", error));
}

function getLanguageServerPath(context: vscode.ExtensionContext): string {
  const extension = process.platform === "win32" ? ".exe" : "";
  const base = path.resolve(__dirname, `wgslx-lsp${extension}`);

  fs.existsSync(base);

  console.log("Found server path:", base);

  return base;
}

export function deactivate() {
  if (client?.isRunning) {
    client.stop();
    client = null;
  }
}