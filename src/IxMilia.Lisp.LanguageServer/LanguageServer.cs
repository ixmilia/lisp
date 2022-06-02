using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using IxMilia.Lisp.LanguageServer.Protocol;
using StreamJsonRpc;

namespace IxMilia.Lisp.LanguageServer
{
    public class LanguageServer
    {
        private JsonRpc _rpc;
        private Dictionary<string, (LispRepl Repl, string Content)> _documentContents = new Dictionary<string, (LispRepl, string)>();

        internal LanguageServer()
        {
        }

        public LanguageServer(Stream sendingStream, Stream receivingStream)
            : this()
        {
            var encoding = new UTF8Encoding(false);
            var formatter = new JsonMessageFormatter(encoding);
            Serializer.ConfigureSerializer(formatter.JsonSerializer);
            var messageHandler = new HeaderDelimitedMessageHandler(sendingStream, receivingStream, formatter);
            _rpc = new JsonRpc(messageHandler, this);
            _rpc.TraceSource = new TraceSource("debugging-trace-listener", SourceLevels.All);
            _rpc.TraceSource.Listeners.Add(new DebuggingTraceListener());
        }

        private class DebuggingTraceListener : TraceListener
        {
            public override void Write(string message)
            {
            }

            public override void WriteLine(string message)
            {
            }
        }

        public void Start()
        {
            _rpc.StartListening();
        }

        internal string GetDocumentContents(string uri)
        {
            var path = Converters.PathFromUri(uri);
            return _documentContents[path].Content;
        }

        private void SetDocumentContents(string path, string newContent)
        {
            var repl = _documentContents.TryGetValue(path, out var pair)
                ? pair.Repl
                : new LispRepl();
            _documentContents[path] = (repl, newContent);
        }

        [LspMethod("initialize")]
        public InitializeResult Initialize(InitializeParams param)
        {
            return new InitializeResult(TextDocumentSyncKind.Incremental);
        }

        [LspMethod("textDocument/completion")]
        public CompletionList TextDocumentCompletion(CompletionParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            var position = Converters.SourcePositionFromPosition(param.Position);
            var items = Enumerable.Empty<CompletionItem>();
            if (_documentContents.TryGetValue(path, out var pair))
            {
                var parseResult = pair.Repl.ParseUntilSourceLocation(pair.Content, position);

                // don't return anything if we're in a string
                if (!(parseResult.Object is LispString))
                {
                    var visibleValues = parseResult.VisibleValues.Values;
                    if (parseResult.Object is LispResolvedSymbol resolved)
                    {
                        visibleValues = visibleValues.Where(v => v.Symbol.PackageName == resolved.PackageName);
                    }

                    items = visibleValues.Select(
                        v => new CompletionItem(
                            v.Symbol.ToDisplayString(pair.Repl.Host.CurrentPackage),
                            v.Symbol.Value,
                            v.Value is LispFunction f ? new MarkupContent(MarkupKind.Markdown, f.Documentation) : null));
                }
            }

            return new CompletionList(false, items);
        }

        [LspMethod("textDocument/didChange")]
        public void TextDocumentDidChange(DidChangeTextDocumentParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            foreach (var contentChanges in param.ContentChanges)
            {
                if (_documentContents.TryGetValue(path, out var pair))
                {
                    string updatedContent;
                    if (contentChanges.Range is object)
                    {
                        // incremental update
                        var contents = pair.Content;
                        var startIndex = contentChanges.Range.Start.GetIndex(contents);
                        var endIndex = contentChanges.Range.End.GetIndex(contents);
                        var preText = contents.Substring(0, startIndex);
                        var postText = contents.Substring(endIndex);
                        updatedContent = string.Concat(preText, contentChanges.Text, postText);
                    }
                    else
                    {
                        // full update
                        updatedContent = contentChanges.Text;
                    }

                    SetDocumentContents(path, updatedContent);
                }
            }
        }

        [LspMethod("textDocument/didClose")]
        public void TextDocumentDidClose(DidCloseTextDocumentParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            _documentContents.Remove(path);
        }

        [LspMethod("textDocument/didOpen")]
        public void TextDocumentDidOpen(DidOpenTextDocumentParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            SetDocumentContents(path, param.TextDocument.Text);
        }

        [LspMethod("textDocument/hover")]
        public Hover TextDocumentHover(HoverParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            var position = Converters.SourcePositionFromPosition(param.Position);
            if (_documentContents.TryGetValue(path, out var pair))
            {
                var parseResult = pair.Repl.ParseUntilSourceLocation(pair.Content, position);
                var markdown = parseResult.GetMarkdownDisplay();
                return new Hover(new MarkupContent(MarkupKind.Markdown, markdown));
            }

            return null;
        }

        [LspMethod("textDocument/semanticTokens/full")]
        public SemanticTokens TextDocumentSemanticTokensFull(SemanticTokensParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            if (_documentContents.TryGetValue(path, out var pair))
            {
                var legend = new SemanticTokensLegend();
                var builder = new SemanticTokensBuilder(legend.TokenTypes, legend.TokenModifiers);
                var objects = pair.Repl.ParseAll(pair.Content);
                foreach (var obj in objects)
                {
                    foreach (var token in obj.GetSemanticTokens(pair.Repl.Host))
                    {
                        var start = Converters.PositionFromSourcePosition(token.Start);
                        var end = Converters.PositionFromSourcePosition(token.End);
                        var startIndex = start.GetIndex(pair.Content);
                        var endIndex = end.GetIndex(pair.Content);
                        var length = endIndex - startIndex;
                        builder.AddToken(start.Line, start.Character, (uint)length, token.Type.AsTokenTypeString());
                    }
                }

                var result = builder.Build();
                return result;
            }

            return null;
        }
    }
}
