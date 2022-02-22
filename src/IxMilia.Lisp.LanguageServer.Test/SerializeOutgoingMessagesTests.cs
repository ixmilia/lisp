using System.IO;
using IxMilia.Lisp.LanguageServer.Protocol;
using Newtonsoft.Json;
using Xunit;

namespace IxMilia.Lisp.LanguageServer.Test
{
    public class SerializeOutgoingMessagesTests
    {
        private string SerializeObject<T>(T value)
        {
            var serializer = new JsonSerializer();
            Serializer.ConfigureSerializer(serializer);
            var writer = new StringWriter();
            serializer.Serialize(writer, value);
            var json = writer.ToString();
            return json;
        }

        [Fact]
        public void SerializeHover()
        {
            var obj = new Hover(new MarkupContent(MarkupKind.Markdown, "some markdown"));
            var json = SerializeObject(obj);
            Assert.Equal(@"{""contents"":{""kind"":""markdown"",""value"":""some markdown""}}", json);
        }

        [Fact]
        public void SerializeInitializeResultWithSyncKindFull()
        {
            var obj = new InitializeResult(TextDocumentSyncKind.Full);
            var json = SerializeObject(obj);
            Assert.Equal(@"{""capabilities"":{""textDocumentSync"":{""openClose"":true,""change"":1},""hoverProvider"":true}}", json);
        }

        [Fact]
        public void SerializeInitializeResultWithSyncKindIncremental()
        {
            var obj = new InitializeResult(TextDocumentSyncKind.Incremental);
            var json = SerializeObject(obj);
            Assert.Equal(@"{""capabilities"":{""textDocumentSync"":{""openClose"":true,""change"":2},""hoverProvider"":true}}", json);
        }
    }
}
