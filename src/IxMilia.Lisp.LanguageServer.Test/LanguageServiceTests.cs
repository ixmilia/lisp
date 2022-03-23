using System;
using IxMilia.Lisp.LanguageServer.Protocol;
using Xunit;

namespace IxMilia.Lisp.LanguageServer.Test
{
    public class LanguageServiceTests
    {
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
            var server = new LanguageServer();
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "some-language-id", 1, "(setf sum (+ 1 1))")));
            var hover = server.TextDocumentHover(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), new Position(0, 3)));
            Assert.Contains("(DEFMACRO SETF (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public void DocumentIsUpdatedWithFullChangeEvent()
        {
            var server = new LanguageServer();
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "some-language-id", 1, "(defun add (a b) (+ a b))")));
            // full update sets `Range` and `RangeLength` to null
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(null, null, "(defmacro add (a b) (+ a b))") }));
            var contents = server.GetDocumentContents("file:///some-uri");
            Assert.Equal("(defmacro add (a b) (+ a b))", contents);
        }

        [Fact]
        public void DocumentIsUpdatedWithIncrementalChangeEvent()
        {
            var server = new LanguageServer();
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "some-language-id", 1, "(defun add (a b) (+ a b))")));
            // incremental update sets `Range` and `RangeLength` to non-null values
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(new Protocol.Range(new Position(0, 1), new Position(0, 6)), 5, "defmacro") }));
            var contents = server.GetDocumentContents("file:///some-uri");
            Assert.Equal("(defmacro add (a b) (+ a b))", contents);
        }

        [Fact]
        public void GetCompletionItems()
        {
            var server = new LanguageServer();
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "some-language-id", 1, "(def")));
            var completionList = server.TextDocumentCompletion(new CompletionParams(new CompletionContext(CompletionTriggerKind.TriggerCharacter, '('), new TextDocumentIdentifier("file:///some-uri"), new Position(0, 4)));
            Assert.False(completionList.IsIncomplete);
            Assert.Contains(completionList.Items, item => item.Label == "DEFUN" && item.Detail == "COMMON-LISP:DEFUN");
        }

        [Fact]
        public void GetHoverTextAfterFullUpdate()
        {
            var server = new LanguageServer();
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "some-language-id", 1, "(defun add (a b) (+ a b))")));
            // full update sets `Range` and `RangeLength` to null
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(null, null, "(defmacro add (a b) (+ a b))") }));
            var hover = server.TextDocumentHover(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), new Position(0, 3)));
            Assert.Contains("(DEFMACRO DEFMACRO (...) ...)", hover.Contents.Value);
        }

        [Fact]
        public void GetHoverTextAfterIncrementalUpdate()
        {
            var server = new LanguageServer();
            server.Initialize(new InitializeParams(0, Array.Empty<WorkspaceFolder>()));
            server.TextDocumentDidOpen(new DidOpenTextDocumentParams(new TextDocumentItem("file:///some-uri", "some-language-id", 1, "(defun add (a b) (+ a b))")));
            // incremental update sets `Range` and `RangeLength` to non-null values
            server.TextDocumentDidChange(new DidChangeTextDocumentParams(new VersionedTextDocumentIdentifier("file:///some-uri", 2), new[] { new TextDocumentContentChangeEvent(new Protocol.Range(new Position(0, 1), new Position(0, 6)), 5, "defmacro") }));
            var hover = server.TextDocumentHover(new HoverParams(new TextDocumentIdentifier("file:///some-uri"), new Position(0, 3)));
            Assert.Contains("(DEFMACRO DEFMACRO (...) ...)", hover.Contents.Value);
        }
    }
}
