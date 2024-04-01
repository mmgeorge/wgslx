# WebGPU Shading Language eXtended Extension for VSCode

Includes the syntax highlighting and some very experimental language server support. Syntax highlighting derived from [wgsl-analyzer](https://github.com/wgsl-analyzer/wgsl-analyzer/tree/main/editors/code) with minor modifications. 

## Installation
Due to the experimental nature of the language server, the extension is not currently distributed. To build locally, see the instructions below.

## Building 
To build both the language server & extension, run:
```sh
cargo xtask vscode
```

Then install the extension locally with:
```sh
code --install-extension ./out/wgslx-0.0.1.vsix
```

