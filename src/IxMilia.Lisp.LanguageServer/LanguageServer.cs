using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;
using IxMilia.Lisp.LanguageServer.Protocol;
using StreamJsonRpc;

namespace IxMilia.Lisp.LanguageServer
{
    public class LanguageServer
    {
        private JsonRpc _rpc;
        private LispRepl _repl;
        private Dictionary<string, string> _documentContents = new Dictionary<string, string>();

        internal LanguageServer()
        {
            _repl = new LispRepl();
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
            return _documentContents[path];
        }

        [JsonRpcMethod("initialize", UseSingleObjectParameterDeserialization = true)]
        public InitializeResult Initialize(InitializeParams param)
        {
            return new InitializeResult(TextDocumentSyncKind.Incremental);
        }

        [JsonRpcMethod("textDocument/didChange", UseSingleObjectParameterDeserialization = true)]
        public void TextDocumentDidChange(DidChangeTextDocumentParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            foreach (var contentChanges in param.ContentChanges)
            {
                if (_documentContents.TryGetValue(path, out var contents))
                {
                    if (contentChanges.Range is object)
                    {
                        // incremental update
                        var startIndex = contentChanges.Range.Start.GetIndex(contents);
                        var endIndex = contentChanges.Range.End.GetIndex(contents);
                        var preText = contents.Substring(0, startIndex);
                        var postText = contents.Substring(endIndex);
                        var updatedContent = string.Concat(preText, contentChanges.Text, postText);
                        _documentContents[path] = updatedContent;
                    }
                    else
                    {
                        // full update
                        _documentContents[path] = contentChanges.Text;
                    }
                }
            }
        }

        [JsonRpcMethod("textDocument/didClose", UseSingleObjectParameterDeserialization = true)]
        public void TextDocumentDidClose(DidCloseTextDocumentParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            _documentContents.Remove(path);
        }

        [JsonRpcMethod("textDocument/didOpen", UseSingleObjectParameterDeserialization = true)]
        public void TextDocumentDidOpen(DidOpenTextDocumentParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            _documentContents[path] = param.TextDocument.Text;
        }

        [JsonRpcMethod("textDocument/hover", UseSingleObjectParameterDeserialization = true)]
        public Hover TextDocumentHover(HoverParams param)
        {
            var path = Converters.PathFromUri(param.TextDocument.Uri);
            var position = Converters.SourcePositionFromPosition(param.Position);
            if (_documentContents.TryGetValue(path, out var code))
            {
                var obj = _repl.GetObjectAtLocation(code, position);
                var markdown = _repl.GetMarkdownDisplayFromSourceObject(obj);
                return new Hover(new MarkupContent(MarkupKind.Markdown, markdown));
            }

            return null;
        }
    }
}
