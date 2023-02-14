using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using IxMilia.Lisp.LanguageServer.Protocol;
using StreamJsonRpc;

namespace IxMilia.Lisp.LanguageServer
{
    public class LanguageServer
    {
        private JsonRpc _rpc;
        private Dictionary<string, (LispHost Host, string Content, ResettableTextWriter TextWriter)> _documentContents = new Dictionary<string, (LispHost, string, ResettableTextWriter)>();

        public LanguageServer(Stream sendingStream, Stream receivingStream)
        {
            var messageHandler = CreateMessageHandler(sendingStream, receivingStream);
            _rpc = new JsonRpc(messageHandler, this);
            _rpc.TraceSource = new TraceSource("debugging-trace-listener", SourceLevels.All);
            _rpc.TraceSource.Listeners.Add(new DebuggingTraceListener());
        }

        internal static IJsonRpcMessageHandler CreateMessageHandler(Stream sendingStream, Stream receivingStream)
        {
            var encoding = new UTF8Encoding(false);
            var formatter = new JsonMessageFormatter(encoding);
            Serializer.ConfigureSerializer(formatter.JsonSerializer);
            var messageHandler = new HeaderDelimitedMessageHandler(sendingStream, receivingStream, formatter);
            return messageHandler;
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

        private async Task SetDocumentContentsAsync(string uri, string newContent, CancellationToken cancellationToken)
        {
            LispHost host;
            ResettableTextWriter textWriter;
            if (_documentContents.TryGetValue(uri, out var pair))
            {
                host = pair.Host;
                textWriter = pair.TextWriter;
            }
            else
            {
                var output = new ResettableTextWriter();
                var configuration = new LispHostConfiguration(output: output);
                host = await LispHost.CreateAsync(configuration);
                textWriter = output;
            }

            _documentContents[uri] = (host, newContent, textWriter);
            var diagnostics = await ComputeDiagnosticsAsync(uri, cancellationToken);
            TextDocumentPublishDiagnostics(uri, diagnostics);
        }

        [LspMethod("initialize")]
        public InitializeResult Initialize(InitializeParams param)
        {
            return new InitializeResult(TextDocumentSyncKind.Incremental);
        }

        [LspMethod("textDocument/eval")]
        public async Task<EvalResult> TextDocumentEvalAsync(EvalTextDocumentParams param)
        {
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
            {
                var diagnostics = new List<Diagnostic>();
                var resettableTextWriter = pair.TextWriter;
                resettableTextWriter.Reset();

                var executionState = pair.Host.CreateExecutionState();
                var inputName = param.TextDocument.Uri;
                var content = pair.Content;
                if (param.Range is { })
                {
                    inputName += $":{param.Range}";
                    var startIndex = param.Range.Start.GetIndex(content);
                    var endIndex = param.Range.End.GetIndex(content);
                    content = content.Substring(startIndex, endIndex - startIndex);
                }

                var evalResult = await pair.Host.EvalAsync(inputName, content, executionState);
                var error = executionState.LastReportedError;

                if (error is { })
                {
                    AddErrorToDiagnosticCollection(diagnostics, error, param.Range?.Start);
                }

                var fullOutput = new StringBuilder();
                var stdout = resettableTextWriter.GetText();
                if (stdout != null)
                {
                    fullOutput.Append(stdout);
                    if (stdout.Length > 0 && !stdout.EndsWith("\n"))
                    {
                        fullOutput.Append('\n');
                    }
                }

                fullOutput.Append(evalResult.Value?.ToString());

                TextDocumentPublishDiagnostics(param.TextDocument.Uri, diagnostics.ToArray());

                var isError = error is { };
                var result = new EvalResult(isError, fullOutput.ToString());
                return result;
            }

            return new EvalResult(true, $"File '{param.TextDocument.Uri}' not found.");
        }

        [LspMethod("textDocument/completion")]
        public async Task<CompletionList> TextDocumentCompletionAsync(CompletionParams param)
        {
            var position = Converters.SourcePositionFromPosition(param.Position);
            var items = Enumerable.Empty<CompletionItem>();
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
            {
                var parseResult = await pair.Host.ParseUntilSourceLocationAsync(param.TextDocument.Uri, pair.Content, position);
                items = parseResult.GetReducedCompletionItems(pair.Host.CurrentPackage,
                    (symbol, value) =>
                        new CompletionItem(
                            symbol.ToDisplayString(pair.Host.CurrentPackage),
                            symbol.Value,
                            value is LispFunction f ? new MarkupContent(MarkupKind.Markdown, f.Documentation) : null),
                    name => new CompletionItem(name, name, null));
            }

            return new CompletionList(false, items);
        }

        [LspMethod("textDocument/diagnostic")]
        public async Task<DocumentDiagnosticReport> TextDocumentDiagnosticAsync(DocumentDiagnosticParams param)
        {
            var diagnostics = await ComputeDiagnosticsAsync(param.TextDocument.Uri, CancellationToken.None);
            var result = new FullDocumentDiagnosticReport(diagnostics);
            return result;
        }

        [LspMethod("textDocument/didChange")]
        public async Task TextDocumentDidChangeAsync(DidChangeTextDocumentParams param)
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

                    await SetDocumentContentsAsync(param.TextDocument.Uri, updatedContent, CancellationToken.None);
                }
            }
        }

        [LspMethod("textDocument/didClose")]
        public void TextDocumentDidClose(DidCloseTextDocumentParams param)
        {
            _documentContents.Remove(param.TextDocument.Uri);
        }

        [LspMethod("textDocument/didOpen")]
        public Task TextDocumentDidOpenAsync(DidOpenTextDocumentParams param)
        {
            return SetDocumentContentsAsync(param.TextDocument.Uri, param.TextDocument.Text, CancellationToken.None);
        }

        [LspMethod("textDocument/hover")]
        public async Task<Hover> TextDocumentHoverAsync(HoverParams param)
        {
            var position = Converters.SourcePositionFromPosition(param.Position);
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
            {
                var parseResult = await pair.Host.ParseUntilSourceLocationAsync(param.TextDocument.Uri, pair.Content, position);
                var markdown = parseResult.GetMarkdownDisplay();
                return new Hover(new MarkupContent(MarkupKind.Markdown, markdown));
            }

            return null;
        }

        private async Task<Diagnostic[]> ComputeDiagnosticsAsync(string uri, CancellationToken cancellationToken)
        {
            var diagnostics = new List<Diagnostic>();
            if (_documentContents.TryGetValue(uri, out var pair))
            {
                var parsedObjects = await pair.Host.ParseAllAsync(pair.Content, cancellationToken);
                foreach (var parsedObj in parsedObjects)
                {
                    if (parsedObj is LispError error)
                    {
                        AddErrorToDiagnosticCollection(diagnostics, error);
                    }
                }
            }

            return diagnostics.ToArray();
        }

        private static void AddErrorToDiagnosticCollection(List<Diagnostic> diagnostics, LispError error, Position offset = null)
        {
            var range = RangeFromError(error);
            if (range is { })
            {
                if (offset is { })
                {
                    range = new Range(
                        new Position(range.Start.Line + offset.Line, range.Start.Character + offset.Character),
                        new Position(range.End.Line + offset.Line, range.End.Character + offset.Character));
                }

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
            Notify("textDocument/publishDiagnostics", param);
        }

        private void Notify(string targetName, object argument = null)
        {
            var _ = _rpc.NotifyWithParameterObjectAsync(targetName, argument);
        }

        [LspMethod("textDocument/semanticTokens/full")]
        public async Task<SemanticTokens> TextDocumentSemanticTokensFullAsync(SemanticTokensParams param)
        {
            if (_documentContents.TryGetValue(param.TextDocument.Uri, out var pair))
            {
                var legend = new SemanticTokensLegend();
                var builder = new SemanticTokensBuilder(legend.TokenTypes, legend.TokenModifiers);
                var objects = await pair.Host.ParseAllAsync(pair.Content);
                foreach (var obj in objects)
                {
                    foreach (var token in obj.GetSemanticTokens(pair.Host))
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
