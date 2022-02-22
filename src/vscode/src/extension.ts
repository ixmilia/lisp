import * as path from 'path';
import * as vscode from 'vscode';
import * as languageclient from 'vscode-languageclient/node';

let client: languageclient.LanguageClient;

const languageName = 'lisp';

export async function activate(context: vscode.ExtensionContext) {
    let dotnetPath = 'dotnet';
    if (context.extensionMode === vscode.ExtensionMode.Production) {
        const acquireContext = {
            version: '6.0',
            requestingExtensionId: context.extension.id,
        };
        const dotnetResult = <any>await vscode.commands.executeCommand('dotnet.acquire', acquireContext);
        dotnetPath = dotnetResult?.dotnetPath || dotnetPath;
    }

    const serverOptions: languageclient.ServerOptions = {
        run: {
            command: dotnetPath,
            args: [path.join(__dirname, '..', 'server', 'IxMilia.Lisp.LanguageServer.App.dll')],
            transport: languageclient.TransportKind.stdio,
        },
        debug: {
            command: dotnetPath,
            args: ['run', '--project', path.join(__dirname, '..', '..', 'IxMilia.Lisp.LanguageServer.App', 'IxMilia.Lisp.LanguageServer.App.csproj')],
            transport: languageclient.TransportKind.stdio,
        }
    };
    const clientOptions: languageclient.LanguageClientOptions = {
        documentSelector: [
            { language: languageName }
        ],
    };
    client = new languageclient.LanguageClient(languageName, 'IxMilia.Lisp Language Server', serverOptions, clientOptions);
    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }

    return client.stop();
}
