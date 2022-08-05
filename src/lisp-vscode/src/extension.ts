import * as cp from 'child_process';
import * as path from 'path';
import * as vscode from 'vscode';
import * as languageclient from 'vscode-languageclient/node';

let client: languageclient.LanguageClient;
let outputChannel: vscode.OutputChannel;
let serverProcess: cp.ChildProcess;

const exeSuffix = process.platform === 'win32' ? '.exe' : '';
let dotnetPath = `dotnet${exeSuffix}`;
let baseArgs: string[];
let lspArgs: string[];
let debuggerArgs: string[];

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
    baseArgs = context.extensionMode === vscode.ExtensionMode.Development ? debugArgs : releaseArgs;
    lspArgs = [...baseArgs, 'lsp'];
    debuggerArgs = [...baseArgs, 'debug'];
    outputChannel = vscode.window.createOutputChannel(outputChannelName);
    serverProcess = startLspServer();
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

    prepareDebugger(context);

    await client.start();
}

function prepareDebugger(context: vscode.ExtensionContext) {
    // prepare debugger configuration
    const debugConfigurationProvider: vscode.DebugConfigurationProvider = {
        resolveDebugConfiguration: async (folder: vscode.WorkspaceFolder | undefined, config: vscode.DebugConfiguration) => {
            if (!config.program) {
                // TODO: vscode.window.activeTextEditor: get contents if file uri is untitled
                config.program = '${file}'; // ensure this is set
            }

            return config;
        }
    };
    context.subscriptions.push(vscode.debug.registerDebugConfigurationProvider('ixmilia-lisp', debugConfigurationProvider));

    // prepare debugger
    const debugExecutable = new vscode.DebugAdapterExecutable(dotnetPath, debuggerArgs);
    const debugFactory: vscode.DebugAdapterDescriptorFactory = {
        createDebugAdapterDescriptor(session: vscode.DebugSession, executable: vscode.DebugAdapterExecutable): vscode.ProviderResult<vscode.DebugAdapterDescriptor> {
            return debugExecutable;
        }
    };
    context.subscriptions.push(vscode.debug.registerDebugAdapterDescriptorFactory('ixmilia-lisp', debugFactory));
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }

    return client.stop();
}

function startLspServer(): cp.ChildProcess {
    const process = cp.spawn(dotnetPath, lspArgs);
    outputChannel.appendLine(`Server PID ${process.pid} started with args ${lspArgs}`);
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
        serverProcess = startLspServer();
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
