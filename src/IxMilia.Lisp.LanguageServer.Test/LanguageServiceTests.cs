using System;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Threading.Tasks;
using IxMilia.Lisp.LanguageServer.Protocol;
using IxMilia.Lisp.Test;
using Nerdbank.Streams;
using StreamJsonRpc;
using Xunit;

namespace IxMilia.Lisp.LanguageServer.Test
{
    public class LanguageServiceTests : TestBase
    {
        private Task<LanguageServer> GetServerWithFileContentAsync(string fileUri, string markedCode, out Position position)
        {
            GetCodeAndPosition(markedCode, out var code, out var lispPosition);
            position = Converters.PositionFromSourcePosition(lispPosition);
            return GetServerWithFileContentAsync(fileUri, code);
        }

        private Task<LanguageServer> GetServerWithFileContentAsync(string fileUri, string code)
        {
            return GetServerWithFileContentAsync((fileUri, code));
        }

        private async Task<LanguageServer> GetServerWithFileContentAsync(params (string fileUri, string code)[] pairs)
        {
            var server = new LanguageServer(new MemoryStream(), new MemoryStream());
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            foreach (var pair in pairs)
            {
                await server.TextDocumentDidOpenAsync(new DidOpenTextDocumentParams(new TextDocumentItem(pair.fileUri, "some-language-id", 1, pair.code)));
            }

            return server;
        }

        [Fact]
        public void PositionToIndex()
        {
            var value = "123\n123456\n123";
            // indicies: 0123 456789
            //                   ^ // line 1, character 3
            var position = new Position(1, 3);
            var actual = position.GetIndex(value);
            Assert.Equal(7, actual);
        }

        [Fact]
        public async Task GetHoverTextFromDocument()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", "(se$$tf sum (+ 1 1))", out var position);
            var hover = await server.TextDocumentHoverAsync(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), position));
            Assert.Contains("(DEFSPECIAL SETF (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public async Task DocumentIsUpdatedWithFullChangeEvent()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", "(defun add (a b) (+ a b))");
            // full update sets `Range` and `RangeLength` to null
            await server.TextDocumentDidChangeAsync(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(null, null, "(defmacro add (a b) (+ a b))") }));
            var contents = server.GetDocumentContents("file:///some-uri");
            Assert.Equal("(defmacro add (a b) (+ a b))", contents);
        }

        [Fact]
        public async Task DocumentIsUpdatedWithIncrementalChangeEvent()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", "(defun add (a b) (+ a b))");
            // incremental update sets `Range` and `RangeLength` to non-null values
            await server.TextDocumentDidChangeAsync(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(new Protocol.Range(new Position(0, 1), new Position(0, 6)), 5, "defmacro") }));
            var contents = server.GetDocumentContents("file:///some-uri");
            Assert.Equal("(defmacro add (a b) (+ a b))", contents);
        }

        [Fact]
        public async Task GetCompletionItems()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", "(def$$", out var position);
            var completionList = await server.TextDocumentCompletionAsync(new CompletionParams(new CompletionContext(CompletionTriggerKind.TriggerCharacter, '('), new TextDocumentIdentifier("file:///some-uri"), position));
            Assert.False(completionList.IsIncomplete);
            Assert.Contains(completionList.Items, item => item.Label == "DEFUN" && item.Detail == "COMMON-LISP:DEFUN");
        }

        [Fact]
        public async Task CompletionItemsAreScopedToTheSpecifiedPackage()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", "common-lisp-user:$$", out var position);
            var completionList = await server.TextDocumentCompletionAsync(new CompletionParams(new CompletionContext(CompletionTriggerKind.TriggerCharacter, ':'), new TextDocumentIdentifier("file:///some-uri"), position));
            Assert.All(
                completionList.Items,
                item => Assert.StartsWith("COMMON-LISP-USER:", item.Detail));
        }

        [Fact]
        public async Task DiagnosticsCanBePulled()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", @"""unclosed string");
            var diagnosticReport = (FullDocumentDiagnosticReport)await server.TextDocumentDiagnosticAsync(new DocumentDiagnosticParams(new TextDocumentIdentifier("file:///some-uri")));
            Assert.Equal(DocumentDiagnosticReportKind.Full, diagnosticReport.Kind);
            var diagnostic = diagnosticReport.Items.Single();
            Assert.Equal("(0, 0)-(0, 16)", diagnostic.Range.ToString());
            Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
            Assert.Equal("Unexpected end of string", diagnostic.Message);
        }

        [Fact]
        public async Task GetHoverTextAfterFullUpdate()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", "(defun add (a b) (+ a b))");
            // full update sets `Range` and `RangeLength` to null
            await server.TextDocumentDidChangeAsync(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(null, null, "(defmacro add (a b) (+ a b))") }));
            var hover = await server.TextDocumentHoverAsync(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), new Position(0, 3)));
            Assert.Contains("(DEFMACRO DEFMACRO (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public async Task GetHoverTextAfterIncrementalUpdate()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", "(defun add (a b) (+ a b))");
            // incremental update sets `Range` and `RangeLength` to non-null values
            await server.TextDocumentDidChangeAsync(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(new Protocol.Range(new Position(0, 1), new Position(0, 6)), 5, "defmacro") }));
            var hover = await server.TextDocumentHoverAsync(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), new Position(0, 3)));
            Assert.Contains("(DEFMACRO DEFMACRO (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public async Task SemanticTokenEncoding()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", " 42 \"abc\" ");
            var tokens = await server.TextDocumentSemanticTokensFullAsync(new SemanticTokensParams() { TextDocument = new TextDocumentIdentifier("file:///some-uri") });
            var expected = new uint[]
            {
                // 42
                0, // line offset
                1, // character offset
                2, // length
                18, // token type (number)
                0, // token modifiers
                // "abc"
                0, // line offset
                3, // character offset
                5, // length
                17, // token type (string)
                0, // token modifiers
            };
            Assert.Equal(expected, tokens.Data);
        }

        [Fact]
        public async Task EvalHasSeparateRuntimesForSeparatePaths()
        {
            var server = await GetServerWithFileContentAsync(
                ("file:///some-uri-1", "42"),
                ("file:///some-uri-2", "(setf x 43) x"));
            var evalResult1 = await server.TextDocumentEvalAsync(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri-1")));
            var evalResult2 = await server.TextDocumentEvalAsync(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri-2")));
            Assert.False(evalResult1.IsError);
            Assert.False(evalResult2.IsError);
            Assert.Equal("42", evalResult1.Content);
            Assert.Equal("43", evalResult2.Content);
        }

        [Fact]
        public async Task EvalConsoleOutputIsReturnedPerEval()
        {
            var server = await GetServerWithFileContentAsync("file:///some-uri", @"(format t ""stdout"")");
            var evalResult1 = await server.TextDocumentEvalAsync(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri")));
            Assert.False(evalResult1.IsError);
            Assert.Equal("stdout\n()", evalResult1.Content);

            await server.TextDocumentDidChangeAsync(new DidChangeTextDocumentParams(
                new VersionedTextDocumentIdentifier("file:///some-uri", 2),
                new[]
                {
                    new TextDocumentContentChangeEvent(null, null, "(+ 1 1)")
                }));
            var evalResult2 = await server.TextDocumentEvalAsync(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri")));
            Assert.False(evalResult2.IsError);
            Assert.Equal("2", evalResult2.Content); // previous `stdout` isn't returned again
        }

        [Fact]
        public async Task TextDocumentDidOpenPublishesDiagnostics()
        {
            var (stream1, stream2) = FullDuplexStream.CreatePair();
            var server = new LanguageServer(stream1, stream1);
            server.Start();

            var messageHandler = LanguageServer.CreateMessageHandler(stream2, stream2);
            var client = new JsonRpc(messageHandler);
            var publishDiagnosticsCompletionSource = new TaskCompletionSource<Diagnostic[]>();
            var diagnosticPublishEntryCount = 0;
            Delegate publishDiagnostics = (PublishDiagnosticsParams param) =>
            {
                diagnosticPublishEntryCount++;
                if (diagnosticPublishEntryCount >= 1)
                {
                    publishDiagnosticsCompletionSource.SetResult(param.Diagnostics);
                }
            };
            client.AddLocalRpcMethod(publishDiagnostics.GetMethodInfo(), publishDiagnostics.Target, new LspMethodAttribute("textDocument/publishDiagnostics"));
            client.StartListening();
            await server.TextDocumentDidOpenAsync(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "lisp", 1, @"""unterminated string")));
            await Task.Yield();
            var publishDiagnosticsTimeout = Task.Delay(1000);
            var result = await Task.WhenAny(publishDiagnosticsCompletionSource.Task, publishDiagnosticsTimeout);
            Assert.NotStrictEqual(result, publishDiagnosticsTimeout);
            var diagnostics = await publishDiagnosticsCompletionSource.Task;
            var diagnostic = diagnostics.Single();
            Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
            Assert.Equal("Unexpected end of string", diagnostic.Message);
            Assert.Equal("(0, 0)-(0, 20)", diagnostic.Range.ToString());
        }

        [Fact]
        public async Task TextDocumentDidChangePublishesDiagnostics()
        {
            var (stream1, stream2) = FullDuplexStream.CreatePair();
            var server = new LanguageServer(stream1, stream1);
            server.Start();

            var messageHandler = LanguageServer.CreateMessageHandler(stream2, stream2);
            var client = new JsonRpc(messageHandler);
            var publishDiagnosticsCompletionSource = new TaskCompletionSource<Diagnostic[]>();
            var diagnosticPublishEntryCount = 0;
            Delegate publishDiagnostics = (PublishDiagnosticsParams param) =>
            {
                diagnosticPublishEntryCount++;
                if (diagnosticPublishEntryCount >= 2)
                {
                    publishDiagnosticsCompletionSource.SetResult(param.Diagnostics);
                }
            };
            client.AddLocalRpcMethod(publishDiagnostics.GetMethodInfo(), publishDiagnostics.Target, new LspMethodAttribute("textDocument/publishDiagnostics"));
            client.StartListening();
            await server.TextDocumentDidOpenAsync(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "lisp", 1, @"()")));
            await server.TextDocumentDidChangeAsync(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(null, null, @"""unterminated string") }));
            await Task.Yield();
            var publishDiagnosticsTimeout = Task.Delay(1000);
            var result = await Task.WhenAny(publishDiagnosticsCompletionSource.Task, publishDiagnosticsTimeout);
            Assert.NotStrictEqual(result, publishDiagnosticsTimeout);
            var diagnostics = await publishDiagnosticsCompletionSource.Task;
            var diagnostic = diagnostics.Single();
            Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
            Assert.Equal("Unexpected end of string", diagnostic.Message);
            Assert.Equal("(0, 0)-(0, 20)", diagnostic.Range.ToString());
        }

        [Fact]
        public async Task EvalPublishesDiagnostics()
        {
            var (stream1, stream2) = FullDuplexStream.CreatePair();
            var server = new LanguageServer(stream1, stream1);
            server.Start();

            var messageHandler = LanguageServer.CreateMessageHandler(stream2, stream2);
            var client = new JsonRpc(messageHandler);
            var publishDiagnosticsCompletionSource = new TaskCompletionSource<Diagnostic[]>();
            var diagnosticPublishEntryCount = 0;
            Delegate publishDiagnostics = (PublishDiagnosticsParams param) =>
            {
                diagnosticPublishEntryCount++;
                if (diagnosticPublishEntryCount >= 2)
                {
                    publishDiagnosticsCompletionSource.SetResult(param.Diagnostics);
                }
            };
            client.AddLocalRpcMethod(publishDiagnostics.GetMethodInfo(), publishDiagnostics.Target, new LspMethodAttribute("textDocument/publishDiagnostics"));
            client.StartListening();
            await server.TextDocumentDidOpenAsync(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "lisp", 1, @"(+ 1 ())")));
            var evalResult = await server.TextDocumentEvalAsync(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri")));
            await Task.Yield();
            var publishDiagnosticsTimeout = Task.Delay(1000);
            var result = await Task.WhenAny(publishDiagnosticsCompletionSource.Task, publishDiagnosticsTimeout);
            Assert.NotStrictEqual(result, publishDiagnosticsTimeout);
            Assert.True(evalResult.IsError);
            var diagnostics = await publishDiagnosticsCompletionSource.Task;
            var diagnostic = diagnostics.Single();
            Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
            Assert.Equal("Expected exactly two numbers", diagnostic.Message);
            Assert.Equal("(0, 5)-(0, 7)", diagnostic.Range.ToString());
        }
    }
}
