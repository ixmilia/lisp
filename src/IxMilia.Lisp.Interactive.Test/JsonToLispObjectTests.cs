using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Interactive.Test
{
    public class JsonToLispObjectTests
    {
        [Fact]
        public void ParseTrue()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson("true", out var result));
            Assert.True(result.IsTLike());
        }

        [Fact]
        public void ParseFalse()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson("false", out var result));
            Assert.True(result.IsNil());
        }

        [Fact]
        public void ParseNull()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson("null", out var result));
            Assert.True(result.IsNil());
        }

        [Fact]
        public void ParseEmptyArray()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson("[]", out var result));
            Assert.True(result.IsNil());
        }

        [Fact]
        public void ParseFloat()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson("1.0", out var result));
            Assert.Equal(1.0, ((LispFloat)result).Value);
        }

        [Fact]
        public void ParseInteger()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson("1", out var result));
            Assert.Equal(1, ((LispInteger)result).Value);
        }

        [Fact]
        public void ParseString()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson(@"""abc""", out var result));
            Assert.Equal("abc", ((LispString)result).Value);
        }

        [Fact]
        public void ParseArray()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson("[1]", out var result));
            Assert.Equal(1, ((LispInteger)((LispList)result).ToList().Single()).Value);
        }

        [Fact]
        public void ParseObject()
        {
            Assert.True(LispValueDeclarer.TryGetLispObjectFromJson(@"{""key1"":""value1"",""key2"":2}", out var result));
            Assert.Equal(@"(:key1 ""value1"" :key2 2)", result.ToString());
        }
    }
}
