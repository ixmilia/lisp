using System;
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
            var server = new LanguageServer();
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem(fileUri, "some-language-id", 1, code)));
            return server;
        }

        [Theory]
        [InlineData("file:///c%3A/path/to/file.lisp", "c:/path/to/file.lisp")]
        [InlineData("file:///usr/test/path/to/file.lisp", "/usr/test/path/to/file.lisp")]
        [InlineData("untitled:Untitled-1", "Untitled-1")]
        public void PathCanBeExtractedFromUri(string uri, string expectedPath)
        {
            var actualPath = Converters.PathFromUri(uri);
            Assert.Equal(expectedPath, actualPath);
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
    }
}
