import os = require('os');
import path = require('path');
import { workspace, window, ExtensionContext } from 'vscode';

import {
    Executable,
    LanguageClient,
    LanguageClientOptions,
    ServerOptions,
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: ExtensionContext) {
    // The server should be in ~/.pernix/bin/pernix_server
    let serverModule = path.join(os.homedir(), '.pernix', 'bin', 'pernix_server');

    const traceOutputChannel = window.createOutputChannel("Pernix Language Server Trace");
    const run: Executable = {
        command: serverModule,
        options: { env: { ...process.env, RUST_LOG: "debug" } },
    };

    let serverOptions: ServerOptions = {
        run,
        debug: run
    };

    // Options to control the language client
    let clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'pernix' }],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/.clientrc')
        },
    };


    // Create the language client and start the client.
    client = new LanguageClient(
        'pernix-language-server',
        'Pernix Language Server',
        serverOptions,
        clientOptions
    );

    // Start the client. This will also launch the server
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}