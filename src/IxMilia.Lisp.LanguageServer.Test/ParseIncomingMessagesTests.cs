using System.IO;
using System.Linq;
using IxMilia.Lisp.LanguageServer.Protocol;
using Newtonsoft.Json;
using Xunit;

namespace IxMilia.Lisp.LanguageServer.Test
{
    public class ParseIncomingMessagesTests
    {
        private T DeserializeObject<T>(string json)
        {
            var serializer = new JsonSerializer();
            Serializer.ConfigureSerializer(serializer);
            var reader = new JsonTextReader(new StringReader(json));
            var result = serializer.Deserialize<T>(reader);
            return result;
        }

        [Fact]
        public void ParseDidChangeTextDocumentParams()
        {
            var result = DeserializeObject<DidChangeTextDocumentParams>(@"{""textDocument"":{""uri"":""some-uri"",""version"":123},""contentChanges"":[{""text"":""some-text"",""range"":{""start"":{""line"":1,""character"":2},""end"":{""line"":3,""character"":4}}}]}");
            Assert.Equal("some-uri", result.TextDocument.Uri);
            Assert.Equal(123, result.TextDocument.Version);
            Assert.Equal("some-text", result.ContentChanges.Single().Text);
            Assert.Equal(1u, result.ContentChanges.Single().Range.Start.Line);
            Assert.Equal(2u, result.ContentChanges.Single().Range.Start.Character);
            Assert.Equal(3u, result.ContentChanges.Single().Range.End.Line);
            Assert.Equal(4u, result.ContentChanges.Single().Range.End.Character);
            Assert.Null(result.ContentChanges.Single().RangeLength);
        }

        [Fact]
        public void ParseDidCloseTextDocumentParams()
        {
            var result = DeserializeObject<DidCloseTextDocumentParams>(@"{""textDocument"":{""uri"":""some-uri""}}");
            Assert.Equal("some-uri", result.TextDocument.Uri);
        }

        [Fact]
        public void ParseCompletionParams()
        {
            var result = DeserializeObject<CompletionParams>(@"{""context"":{""triggerKind"":2,""triggerCharacter"":""(""},""textDocument"":{""uri"":""some-uri""},""position"":{""line"":12,""character"":34}}");
            Assert.Equal(CompletionTriggerKind.TriggerCharacter, result.Context.TriggerKind);
            Assert.Equal('(', result.Context.TriggerCharacter);
            Assert.Equal("some-uri", result.TextDocument.Uri);
            Assert.Equal(12u, result.Position.Line);
            Assert.Equal(34u, result.Position.Character);
        }

        [Fact]
        public void ParseDidOpenTextDocumentParams()
        {
            var result = DeserializeObject<DidOpenTextDocumentParams>(@"{""textDocument"":{""uri"":""some-uri"",""languageId"":""some-language-id"",""version"":123,""text"":""some-text""}}");
            Assert.Equal("some-uri", result.TextDocument.Uri);
            Assert.Equal("some-language-id", result.TextDocument.LanguageId);
            Assert.Equal(123, result.TextDocument.Version);
            Assert.Equal("some-text", result.TextDocument.Text);
        }

        [Fact]
        public void ParseHoverParams()
        {
            var result = DeserializeObject<HoverParams>(@"{""textDocument"":{""uri"":""some-text-document""},""position"":{""line"":12,""character"":34}}");
            Assert.Equal("some-text-document", result.TextDocument.Uri);
            Assert.Equal(12u, result.Position.Line);
            Assert.Equal(34u, result.Position.Character);
        }

        [Fact]
        public void ParseInitializeParamsWithWorkspaceFolders()
        {
            var result = DeserializeObject<InitializeParams>(@"{""processId"":123,""workspaceFolders"":[{""uri"":""some-document-uri"",""name"":""some-name""}]}");
            Assert.Equal(123, result.ProcessId);
            Assert.Equal("some-document-uri", result.WorkspaceFolders.Single().Uri);
            Assert.Equal("some-name", result.WorkspaceFolders.Single().Name);
        }

        [Fact]
        public void ParseInitializeParamsWithoutWorkspaceFolders()
        {
            var result = DeserializeObject<InitializeParams>(@"{""processId"":123}");
            Assert.Equal(123, result.ProcessId);
            Assert.Empty(result.WorkspaceFolders);
        }
    }
}
