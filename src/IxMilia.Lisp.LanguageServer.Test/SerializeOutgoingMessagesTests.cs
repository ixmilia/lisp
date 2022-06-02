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
        public void SerializeCompletionList()
        {
            var obj = new CompletionList(false, new[]
            {
                new CompletionItem("some-completion-item-label", "some-completion-item-detail", new MarkupContent(MarkupKind.Markdown, "this is `markdown`")),
            });
            var json = SerializeObject(obj);
            Assert.Equal(@"{""isIncomplete"":false,""items"":[{""label"":""some-completion-item-label"",""detail"":""some-completion-item-detail"",""documentation"":{""kind"":""markdown"",""value"":""this is `markdown`""}}]}", json);
        }

        [Fact]
        public void SerializeEvalResult()
        {
            var obj = new EvalResult(false, "some-result");
            var json = SerializeObject(obj);
            Assert.Equal(@"{""isError"":false,""content"":""some-result""}", json);
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
            Assert.Equal(@"{""capabilities"":{""textDocumentSync"":{""openClose"":true,""change"":1},""completionProvider"":{""triggerCharacters"":["" "",""("","":""]},""hoverProvider"":true,""semanticTokensProvider"":{""legend"":{""tokenTypes"":[""type"",""class"",""enum"",""interface"",""struct"",""typeParameter"",""parameter"",""variable"",""property"",""enumMember"",""event"",""function"",""method"",""macro"",""keyword"",""modifier"",""comment"",""string"",""number"",""regexp"",""operator""],""tokenModifiers"":[""declaration""]},""full"":true}}}", json);
        }

        [Fact]
        public void SerializeInitializeResultWithSyncKindIncremental()
        {
            var obj = new InitializeResult(TextDocumentSyncKind.Incremental);
            var json = SerializeObject(obj);
            Assert.Equal(@"{""capabilities"":{""textDocumentSync"":{""openClose"":true,""change"":2},""completionProvider"":{""triggerCharacters"":["" "",""("","":""]},""hoverProvider"":true,""semanticTokensProvider"":{""legend"":{""tokenTypes"":[""type"",""class"",""enum"",""interface"",""struct"",""typeParameter"",""parameter"",""variable"",""property"",""enumMember"",""event"",""function"",""method"",""macro"",""keyword"",""modifier"",""comment"",""string"",""number"",""regexp"",""operator""],""tokenModifiers"":[""declaration""]},""full"":true}}}", json);
        }

        [Fact]
        public void SerializeSemanticTokens()
        {
            var obj = new SemanticTokens() { Data = new uint[] { 0, 1, 2, 3, 4 } };
            var json = SerializeObject(obj);
            Assert.Equal(@"{""data"":[0,1,2,3,4]}", json);
        }
    }
}
