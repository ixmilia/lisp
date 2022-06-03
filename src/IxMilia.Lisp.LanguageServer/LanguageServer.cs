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

        public LanguageServer(Stream sendingStream, Stream receivingStream)
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
            return _documentContents[uri].Content;
        }

        private void SetDocumentContents(string uri, string newContent)
        {
            var repl = _documentContents.TryGetValue(uri, out var pair)
                ? pair.Repl
                : new LispRepl(output: new ResettableTextWriter());
            _documentContents[uri] = (repl, newContent);
            var diagnostics = ComputeDiagnostics(uri);
            TextDocumentPublishDiagnostics(uri, diagnostics);
        }

        [LspMethod("initialize")]
        public InitializeResult Initialize(InitializeParams param)
        {
            return new InitializeResult(TextDocumentSyncKind.Incremental);
        }

        [LspMethod("textDocument/eval")]
        public EvalResult TextDocumentEval(EvalTextDocumentParams param)
        {
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
            {
                var diagnostics = new List<Diagnostic>();
                var resettableTextWriter = pair.Repl.Output as ResettableTextWriter;
                resettableTextWriter?.Reset();

                var evalResult = pair.Repl.Eval(pair.Content, consumeIncompleteInput: false);
                var error = evalResult.LastResult as LispError;

                if (error is { })
                {
                    AddErrorToDiagnosticCollection(diagnostics, error);
                }

                var fullOutput = new StringBuilder();
                var stdout = resettableTextWriter?.GetText();
                if (stdout != null)
                {
                    fullOutput.Append(stdout);
                    if (stdout.Length > 0 && !stdout.EndsWith("\n"))
                    {
                        fullOutput.Append('\n');
                    }
                }

                fullOutput.Append(evalResult.LastResult.ToString());

                TextDocumentPublishDiagnostics(param.TextDocument.Uri, diagnostics.ToArray());

                var isError = error is { };
                var result = new EvalResult(isError, fullOutput.ToString());
                return result;
            }

            return new EvalResult(true, $"File '{param.TextDocument.Uri}' not found.");
        }

        [LspMethod("textDocument/completion")]
        public CompletionList TextDocumentCompletion(CompletionParams param)
        {
            var position = Converters.SourcePositionFromPosition(param.Position);
            var items = Enumerable.Empty<CompletionItem>();
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
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

        [LspMethod("textDocument/diagnostic")]
        public DocumentDiagnosticReport TextDocumentDiagnostic(DocumentDiagnosticParams param)
        {
            var diagnostics = ComputeDiagnostics(param.TextDocument.Uri);
            var result = new FullDocumentDiagnosticReport(diagnostics);
            return result;
        }

        [LspMethod("textDocument/didChange")]
        public void TextDocumentDidChange(DidChangeTextDocumentParams param)
        {
            foreach (var contentChanges in param.ContentChanges)
            {
                if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
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

                    SetDocumentContents(param.TextDocument.Uri, updatedContent);
                }
            }
        }

        [LspMethod("textDocument/didClose")]
        public void TextDocumentDidClose(DidCloseTextDocumentParams param)
        {
            _documentContents.Remove(param.TextDocument.Uri);
        }

        [LspMethod("textDocument/didOpen")]
        public void TextDocumentDidOpen(DidOpenTextDocumentParams param)
        {
            SetDocumentContents(param.TextDocument.Uri, param.TextDocument.Text);
        }

        [LspMethod("textDocument/hover")]
        public Hover TextDocumentHover(HoverParams param)
        {
            var position = Converters.SourcePositionFromPosition(param.Position);
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
            {
                var parseResult = pair.Repl.ParseUntilSourceLocation(pair.Content, position);
                var markdown = parseResult.GetMarkdownDisplay();
                return new Hover(new MarkupContent(MarkupKind.Markdown, markdown));
            }

            return null;
        }

        private Diagnostic[] ComputeDiagnostics(string uri)
        {
            var diagnostics = new List<Diagnostic>();
            if (_documentContents.TryGetValue(uri, out var pair))
            {
                foreach (var parsedObj in pair.Repl.ParseAll(pair.Content))
                {
                    if (parsedObj is LispError error)
                    {
                        AddErrorToDiagnosticCollection(diagnostics, error);
                    }
                }
            }

            return diagnostics.ToArray();
        }

        private static void AddErrorToDiagnosticCollection(List<Diagnostic> diagnostics, LispError error)
        {
            var range = RangeFromError(error);
            if (range is { })
            {
                var diagnostic = new Diagnostic(range, DiagnosticSeverity.Error, error.Message);
                diagnostics.Add(diagnostic);
            }
        }

        private static Range RangeFromError(LispError error)
        {
            var sourceLocation = error.SourceLocation ?? error.StackFrame.Root.SourceLocation;
            if (sourceLocation.HasValue)
            {
                return Converters.RangeFromSoureLocation(sourceLocation.Value);
            }

            return null;
        }

        private void TextDocumentPublishDiagnostics(string uri, Diagnostic[] diagnostics)
        {
            var param = new PublishDiagnosticsParams(uri, diagnostics);
            var _ = _rpc.NotifyAsync("textDocument/publishDiagnostics", param);
        }

        [LspMethod("textDocument/semanticTokens/full")]
        public SemanticTokens TextDocumentSemanticTokensFull(SemanticTokensParams param)
        {
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
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
