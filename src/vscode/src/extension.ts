import * as cp from 'child_process';
import * as path from 'path';
import * as vscode from 'vscode';
import * as languageclient from 'vscode-languageclient/node';

let client: languageclient.LanguageClient;

const languageName = 'lisp';
const outputChannelName = 'IxMilia.Lisp Language Server';

export async function activate(context: vscode.ExtensionContext) {
    const exeSuffix = process.platform === 'win32' ? '.exe' : '';
    let dotnetPath = `dotnet${exeSuffix}`;
    if (context.extensionMode === vscode.ExtensionMode.Production) {
        const acquireContext = {
            version: '6.0',
            requestingExtensionId: context.extension.id,
        };
        const dotnetResult = <any>await vscode.commands.executeCommand('dotnet.acquire', acquireContext);
        dotnetPath = dotnetResult?.dotnetPath || dotnetPath;
    }

    const debugArgs = [
        path.join(__dirname, '..', '..', '..', 'artifacts', 'bin', 'IxMilia.Lisp.LanguageServer.App', 'Debug', 'net6.0', 'IxMilia.Lisp.LanguageServer.App.dll')
    ];
    const releaseArgs = [
        path.join(__dirname, '..', 'server', 'IxMilia.Lisp.LanguageServer.App.dll')
    ];
    const args = context.extensionMode === vscode.ExtensionMode.Development ? debugArgs : releaseArgs;
    const outputChannel = vscode.window.createOutputChannel(outputChannelName);
    const serverProcess = startServer(dotnetPath, args, outputChannel);
    serverProcess.stderr.on('data', (data) => {
        const message = data.toString('utf-8');
        outputChannel.appendLine(message);
    });

    const serverOptions: languageclient.ServerOptions = () => Promise.resolve(serverProcess);
    const clientOptions: languageclient.LanguageClientOptions = {
        documentSelector: [
            { scheme: 'file', language: languageName },
            { scheme: 'untitled', language: languageName },
        ],
    };
    client = new languageclient.LanguageClient(languageName, outputChannelName, serverOptions, clientOptions);
    context.subscriptions.push(client.start());
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }

    return client.stop();
}

function startServer(command: string, args: string[], outputChannel: vscode.OutputChannel): cp.ChildProcess {
    const process = cp.spawn(command, args);
    outputChannel.appendLine(`Server PID ${process.pid} started with args ${args}`);
    return process;
}
