using System;
using System.IO;
using System.Linq;
using IxMilia.Lisp.LanguageServer.Protocol;
using IxMilia.Lisp.Test;
using Xunit;

namespace IxMilia.Lisp.LanguageServer.Test
{
    public class LanguageServiceTests : TestBase
    {
        private LanguageServer GetServerWithFileContent(string fileUri, string markedCode, out Position position)
        {
            GetCodeAndPosition(markedCode, out var code, out var lispPosition);
            position = Converters.PositionFromSourcePosition(lispPosition);
            return GetServerWithFileContent(fileUri, code);
        }

        private LanguageServer GetServerWithFileContent(string fileUri, string code)
        {
            return GetServerWithFileContent((fileUri, code));
        }

        private LanguageServer GetServerWithFileContent(params (string fileUri, string code)[] pairs)
        {
            var server = new LanguageServer(new MemoryStream(), new MemoryStream());
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            foreach (var pair in pairs)
            {
                server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem(pair.fileUri, "some-language-id", 1, pair.code)));
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
        public void GetHoverTextFromDocument()
        {
            var server = GetServerWithFileContent("file:///some-uri", "(se$$tf sum (+ 1 1))", out var position);
            var hover = server.TextDocumentHover(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), position));
            Assert.Contains("(DEFMACRO SETF (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public void DocumentIsUpdatedWithFullChangeEvent()
        {
            var server = GetServerWithFileContent("file:///some-uri", "(defun add (a b) (+ a b))");
            // full update sets `Range` and `RangeLength` to null
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(null, null, "(defmacro add (a b) (+ a b))") }));
            var contents = server.GetDocumentContents("file:///some-uri");
            Assert.Equal("(defmacro add (a b) (+ a b))", contents);
        }

        [Fact]
        public void DocumentIsUpdatedWithIncrementalChangeEvent()
        {
            var server = GetServerWithFileContent("file:///some-uri", "(defun add (a b) (+ a b))");
            // incremental update sets `Range` and `RangeLength` to non-null values
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(new Protocol.Range(new Position(0, 1), new Position(0, 6)), 5, "defmacro") }));
            var contents = server.GetDocumentContents("file:///some-uri");
            Assert.Equal("(defmacro add (a b) (+ a b))", contents);
        }

        [Fact]
        public void GetCompletionItems()
        {
            var server = GetServerWithFileContent("file:///some-uri", "(def$$", out var position);
            var completionList = server.TextDocumentCompletion(new CompletionParams(new CompletionContext(CompletionTriggerKind.TriggerCharacter, '('), new TextDocumentIdentifier("file:///some-uri"), position));
            Assert.False(completionList.IsIncomplete);
            Assert.Contains(completionList.Items, item => item.Label == "DEFUN" && item.Detail == "COMMON-LISP:DEFUN");
        }

        [Fact]
        public void CompletionItemsAreScopedToTheSpecifiedPackage()
        {
            var server = GetServerWithFileContent("file:///some-uri", "common-lisp-user:$$", out var position);
            var completionList = server.TextDocumentCompletion(new CompletionParams(new CompletionContext(CompletionTriggerKind.TriggerCharacter, ':'), new TextDocumentIdentifier("file:///some-uri"), position));
            Assert.All(
                completionList.Items,
                item => Assert.StartsWith("COMMON-LISP-USER:", item.Detail));
        }

        [Fact]
        public void DiagnosticsCanBePulled()
        {
            var server = GetServerWithFileContent("file:///some-uri", @"""unclosed string");
            var diagnosticReport = (FullDocumentDiagnosticReport)server.TextDocumentDiagnostic(new DocumentDiagnosticParams(new TextDocumentIdentifier("file:///some-uri")));
            Assert.Equal(DocumentDiagnosticReportKind.Full, diagnosticReport.Kind);
            var diagnostic = diagnosticReport.Items.Single();
            Assert.Equal("(0, 0)-(0, 16)", diagnostic.Range.ToString());
            Assert.Equal(DiagnosticSeverity.Error, diagnostic.Severity);
            Assert.Equal("EOF", diagnostic.Message);
        }

        [Fact]
        public void NoCompletionItemsInATerminatedString()
        {
            var server = GetServerWithFileContent("file:///some-uri", "\"in a string $$\"", out var position);
            var completionList = server.TextDocumentCompletion(new CompletionParams(new CompletionContext(CompletionTriggerKind.TriggerCharacter, ' '), new TextDocumentIdentifier("file:///some-uri"), position));
            Assert.Empty(completionList.Items);
        }

        [Fact]
        public void GetHoverTextAfterFullUpdate()
        {
            var server = GetServerWithFileContent("file:///some-uri", "(defun add (a b) (+ a b))");
            // full update sets `Range` and `RangeLength` to null
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(null, null, "(defmacro add (a b) (+ a b))") }));
            var hover = server.TextDocumentHover(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), new Position(0, 3)));
            Assert.Contains("(DEFMACRO DEFMACRO (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public void GetHoverTextAfterIncrementalUpdate()
        {
            var server = GetServerWithFileContent("file:///some-uri", "(defun add (a b) (+ a b))");
            // incremental update sets `Range` and `RangeLength` to non-null values
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(new Protocol.Range(new Position(0, 1), new Position(0, 6)), 5, "defmacro") }));
            var hover = server.TextDocumentHover(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), new Position(0, 3)));
            Assert.Contains("(DEFMACRO DEFMACRO (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public void SemanticTokenEncoding()
        {
            var server = GetServerWithFileContent("file:///some-uri", " 42 \"abc\" ");
            var tokens = server.TextDocumentSemanticTokensFull(new SemanticTokensParams() { TextDocument = new TextDocumentIdentifier("file:///some-uri") });
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
        public void EvalHasSeparateRuntimesForSeparatePaths()
        {
            var server = GetServerWithFileContent(
                ("file:///some-uri-1", "42"),
                ("file:///some-uri-2", "(setf x 43) x"));
            var evalResult1 = server.TextDocumentEval(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri-1")));
            var evalResult2 = server.TextDocumentEval(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri-2")));
            Assert.False(evalResult1.IsError);
            Assert.False(evalResult2.IsError);
            Assert.Equal("42", evalResult1.Content);
            Assert.Equal("43", evalResult2.Content);
        }

        [Fact]
        public void EvalConsoleOutputIsReturnedPerEval()
        {
            var server = GetServerWithFileContent("file:///some-uri", @"(format t ""stdout"")");

            var evalResult1 = server.TextDocumentEval(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri")));
            Assert.False(evalResult1.IsError);
            Assert.Equal("stdout\n()", evalResult1.Content);

            server.TextDocumentDidChange(new DidChangeTextDocumentParams(
                new VersionedTextDocumentIdentifier("file:///some-uri", 2),
                new[]
                {
                    new TextDocumentContentChangeEvent(null, null, "(+ 1 1)")
                }));
            var evalResult2 = server.TextDocumentEval(new EvalTextDocumentParams(new TextDocumentIdentifier("file:///some-uri")));
            Assert.False(evalResult2.IsError);
            Assert.Equal("2", evalResult2.Content); // previous `stdout` isn't returned again
        }
    }
}
