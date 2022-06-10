using IxMilia.Lisp.LanguageServer.Protocol;
using Microsoft.JSInterop;
using LS = IxMilia.Lisp.LanguageServer;

namespace IxMilia.Lisp.Wasm
{
    public class LispAdapter
    {
        private const string ReplUri = "*REPL*";
        private LS.LanguageServer? _languageServer;

        [JSInvokable]
        public async Task InitAsync(string code)
        {
            _languageServer = new LS.LanguageServer(new MemoryStream(), new MemoryStream());
            _languageServer.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            await _languageServer.TextDocumentDidOpenAsync(new DidOpenTextDocumentParams(new TextDocumentItem(ReplUri, "lisp", 0, code)));
        }

        [JSInvokable]
        public async Task SetContentAsync(string code)
        {
            if (_languageServer is { })
            {
                await _languageServer.TextDocumentDidChangeAsync(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier(ReplUri, 0), new[] { new TextDocumentContentChangeEvent(null, null, code) }));
            }
        }

        [JSInvokable]
        public async Task<string?> EvalAsync()
        {
            if (_languageServer is { })
            {
                var result = await _languageServer.TextDocumentEvalAsync(new EvalTextDocumentParams(new TextDocumentIdentifier(ReplUri)));
                return result.Content;
            }

            return null;
        }

        [JSInvokable]
        public async Task<CompletionList?> CompletionAsync(uint line, uint column)
        {
            if (_languageServer is { })
            {
                var result = await _languageServer.TextDocumentCompletionAsync(new CompletionParams(new CompletionContext(CompletionTriggerKind.TriggerCharacter, ' '), new TextDocumentIdentifier(ReplUri), new Position(line - 1, column - 1)));
                return result;
            }

            return null;
        }

        [JSInvokable]
        public async Task<string?> HoverAsync(uint line, uint column)
        {
            if (_languageServer is { })
            {
                var result = await _languageServer.TextDocumentHoverAsync(new HoverParams(new TextDocumentIdentifier(ReplUri), new Position(line - 1, column - 1)));
                return result.Contents.Value;
            }

            return null;
        }
    }
}
