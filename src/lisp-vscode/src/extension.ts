import * as cp from 'child_process';
import * as path from 'path';
import * as vscode from 'vscode';
import * as languageclient from 'vscode-languageclient/node';

let client: languageclient.LanguageClient;
let outputChannel: vscode.OutputChannel;
let serverProcess: cp.ChildProcess;

const exeSuffix = process.platform === 'win32' ? '.exe' : '';
let dotnetPath = `dotnet${exeSuffix}`;
let args: string[];

const languageName = 'lisp';
const outputChannelName = 'IxMilia.Lisp Language Server';

export async function activate(context: vscode.ExtensionContext) {
    if (context.extensionMode === vscode.ExtensionMode.Production) {
        const acquireContext = {
            version: '6.0',
            requestingExtensionId: context.extension.id,
        };
        const dotnetResult = <any>await vscode.commands.executeCommand('dotnet.acquire', acquireContext);
        dotnetPath = dotnetResult?.dotnetPath || dotnetPath;
    }

    const debugArgs = [
        path.join(__dirname, '..', '..', '..', 'artifacts', 'bin', 'IxMilia.Lisp.EditorServer', 'Debug', 'net6.0', 'IxMilia.Lisp.EditorServer.dll')
    ];
    const releaseArgs = [
        path.join(__dirname, '..', 'server', 'IxMilia.Lisp.EditorServer.dll')
    ];
    args = context.extensionMode === vscode.ExtensionMode.Development ? debugArgs : releaseArgs;
    args = [...args, 'lsp'];
    outputChannel = vscode.window.createOutputChannel(outputChannelName);
    serverProcess = startServer();
    serverProcess.stderr.on('data', (data) => {
        const message = data.toString('utf-8');
        outputChannel.appendLine(message);
    });

    registerCommands(context);

    const serverOptions: languageclient.ServerOptions = () => Promise.resolve(new ServerStreamWrapper());
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

function startServer(): cp.ChildProcess {
    const process = cp.spawn(dotnetPath, args);
    outputChannel.appendLine(`Server PID ${process.pid} started with args ${args}`);
    return process;
}

function registerCommands(context: vscode.ExtensionContext) {
    context.subscriptions.push(vscode.commands.registerCommand('ixmilia-lisp.eval', async () => {
        const currentDocument = vscode.window.activeTextEditor.document;
        const uri = currentDocument.uri.toString();
        const rawResult = await client.sendRequest('textDocument/eval', { textDocument: { uri } });
        const result: { isError: string, content: string } = <any>rawResult;
        outputChannel.show(true);
        const errorPrefix = result.isError ? 'err ' : '';
        const lines = result.content.split('\n');
        const resultLines = lines.map(line => {
            return `${errorPrefix}${uri}: ${line}`;
        });
        const fullResult = resultLines.join('\n');
        outputChannel.appendLine(fullResult);
    }));

    context.subscriptions.push(vscode.commands.registerCommand('ixmilia-lisp.restart', async () => {
        outputChannel.appendLine(`Killing server process ${serverProcess.pid}`);
        serverProcess.kill();
        serverProcess = startServer();
    }));
}

class ServerStreamWrapper {
    constructor() {
    }

    get writer(): NodeJS.WritableStream {
        return serverProcess.stdin;
    }

    get reader(): NodeJS.ReadableStream {
        return serverProcess.stdout;
    }
}
