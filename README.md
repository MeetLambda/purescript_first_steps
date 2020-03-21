# PureScript first steps

## Initial setup and first build

Compiling the [`purescript`](http://www.purescript.org) code, supposing [`nvm`](http://nvm.sh) is already installed, is as simple as typing:

    > nvm install --lts (12.16.0)
    > npm install -g purescript@0.13.6
    > npm install -g spago@0.14.0

## VSCode integration

To support editing Purescript files, there are two useful VSCode plugins:
- PureScript IDE: https://github.com/nwolverson/vscode-ide-purescript
- PureScript Language Support: https://github.com/nwolverson/vscode-language-purescript

In order to have errors highlighted directly into VSCode, you need to set one option into the "PureScript IDE" module:
- "purescript.editorMode": true (Editor Mode: Whether to set the editor-mode flag on the IDE server)

## Running the code
To **build** the application, type in the terminal windows:
- `spago build`

To **run** the application, type in the terminal windows:
- `spago run`

To keep **building** the application when a file is saved, use the following command:
- `spago build --watch --clear-screen`

To check the other options available when using `spago`, check its [documentation](https://github.com/purescript/spago#build-and-run-my-project)

## Docker and VSCode Purescript Environment Setup

A docker container with Spago and Purescript is available on docker hub public repository at *gior/purescriptwithspago*. thanks to Giorgia Rondinini

To run the container:
- `docker run -ti gior/purescriptwithspago bash`

To interface VSCode with the running container, install "Remote Development" extension, available at https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack

This environment works on Windows and Mac (with Docker Desktop), and Linux

