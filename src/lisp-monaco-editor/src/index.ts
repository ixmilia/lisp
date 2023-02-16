import * as monaco from 'monaco-editor';
import * as grammar from './grammar/grammar';
import './index.css';

interface CompletionItem {
    label: string;
    detail: string;
    documentation: {
        kind: "plaintext" | "markdown";
        value: string;
    };
}

interface CompletionList {
    isIncomplete: boolean;
    items: CompletionItem[];
}

interface LispAdapter {
    init(code: string): Promise<void>;
    setContent(code: string): Promise<void>;
    eval(): Promise<string>;
    hover(line: number, column: number): Promise<string>;
    completion(line: number, column: number): Promise<CompletionList | undefined>;
}

interface DotNetInvoker {
    invokeMethod(methodName: string, ...args: any[]);
    invokeMethodAsync(methodName: string, ...args: any[]): Promise<any>;
}

class DotNetWasmLispAdapter implements LispAdapter {
    constructor(private readonly invoker: DotNetInvoker) {
    }

    async init(code: string): Promise<void> {
        await this.invoker.invokeMethodAsync('InitAsync', code);
    }

    async setContent(code: string): Promise<void> {
        await this.invoker.invokeMethodAsync('SetContentAsync', code);
    }

    async eval(): Promise<string> {
        const result = await this.invoker.invokeMethodAsync('EvalAsync');
        return result;
    }

    async hover(line: number, column: number): Promise<string> {
        const result = await this.invoker.invokeMethodAsync('HoverAsync', line, column);
        return result;
    }

    async completion(line: number, column: number): Promise<CompletionList | undefined> {
        const result = await this.invoker.invokeMethodAsync('CompletionAsync', line, column);
        return result;
    }
}

async function createEditor(adapter: DotNetInvoker): Promise<void> {

    const lisp = new DotNetWasmLispAdapter(adapter);

    // @ts-ignore
    self.MonacoEnvironment = {
        getWorkerUrl: function (moduleId, label) {
            return './editor.worker.bundle.js';
        }
    };

    monaco.languages.register({
        id: 'lisp'
    });
    monaco.languages.setMonarchTokensProvider('lisp', grammar.languageGrammar);
    const editor = monaco.editor.create(document.getElementById('editor'), {
        value: ['; comment', '(+ 1 1)'].join('\n'),
        language: 'lisp',
    });

    async function lispEval(): Promise<void> {
        const code = editor.getValue();
        await lisp.setContent(code);
        const result = await lisp.eval();
        const textArea = <HTMLTextAreaElement>document.getElementById('output');
        if (textArea.value.length > 0) {
            textArea.value += '\n';
        }

        textArea.value += result;
        textArea.scrollTop = textArea.scrollHeight;
    }

    editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, async () => {
        await lispEval();
    });

    document.getElementById('eval-button')!.addEventListener('click', async () => {
        await lispEval();
    });

    document.getElementById('clear-button')!.addEventListener('click', () => {
        (<HTMLTextAreaElement>document.getElementById('output'))!.value = '';
    });

    const hoverProvider = {
        provideHover: async function (model: monaco.editor.ITextModel, position: monaco.Position, token: monaco.CancellationToken): Promise<monaco.languages.Hover> {
            await lisp.setContent(model.getValue());
            const hover = await lisp.hover(position.lineNumber, position.column);
            return {
                contents: [{
                    value: hover,
                }]
            };
        }
    };
    const completionProvider = {
        triggerCharacters: [':', '('],
        provideCompletionItems: async function (model: monaco.editor.ITextModel, position: monaco.Position, context: monaco.languages.CompletionContext, token: monaco.CancellationToken): Promise<monaco.languages.CompletionList> {
            await lisp.setContent(model.getValue());
            const completionList = await lisp.completion(position.lineNumber, position.column);
            return {
                suggestions: <any>completionList.items,
            };
        }
    };

    lisp.init(editor.getModel().getValue());
    monaco.languages.registerHoverProvider('lisp', hoverProvider);
    monaco.languages.registerCompletionItemProvider('lisp', completionProvider);
}

// @ts-ignore
window.createEditor = createEditor;
