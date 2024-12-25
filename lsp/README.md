# Install the Language Server Protocol

First, install the server to the system using cargo install command on the
root of the repository. 

```bash
cargo install --path lsp/pernix_server --root ~/.pernix
```

This command was tested on MacOS and hopefully works on Linux as well. If you
are using Windows, you can try to install the server using the following
command:

```bash
cargo install --path lsp/pernix_server --root %USERPROFILE%\.pernix
```

## Visual Studio Code Client Extension

To use the server with Visual Studio Code, go to the **Run and Debug** tab and
select "Launch LSP Client Extension" configuration. Then, press the green
play button to start the client. This will open a new Visual Studio Code
window with the client extension running.
