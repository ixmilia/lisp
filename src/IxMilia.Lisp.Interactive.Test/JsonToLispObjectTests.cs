using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace IxMilia.Lisp.Interactive.Test
{
    public class JsonToLispObjectTests
    {
        private static async Task<LispObject> ParseJson(string json)
        {
            var host = await LispHost.CreateAsync();
            Assert.True(LispKernel.TryGetLispObjectFromJson(host, json, out var result));
            return result;
        }

        [Fact]
        public async Task ParseTrue()
        {
            var result = await ParseJson("true");
            Assert.True(result.IsT());
        }

        [Fact]
        public async Task ParseFalse()
        {
            var result = await ParseJson("false");
            Assert.True(result.IsNil());
        }

        [Fact]
        public async Task ParseNull()
        {
            var result = await ParseJson("null");
            Assert.True(result.IsNil());
        }

        [Fact]
        public async Task ParseEmptyArray()
        {
            var result = await ParseJson("[]");
            Assert.True(result.IsNil());
        }

        [Fact]
        public async Task ParseFloat()
        {
            var result = await ParseJson("1.0");
            Assert.Equal(1.0, ((LispFloat)result).Value);
        }

        [Fact]
        public async Task ParseInteger()
        {
            var result = await ParseJson("1");
            Assert.Equal(1, ((LispInteger)result).Value);
        }

        [Fact]
        public async Task ParseString()
        {
            var result = await ParseJson(@"""abc""");
            Assert.Equal("abc", ((LispString)result).Value);
        }

        [Fact]
        public async Task ParseArray()
        {
            var result = await ParseJson("[1]");
            Assert.Equal(1, ((LispInteger)((LispList)result).ToList().Single()).Value);
        }

        [Fact]
        public async Task ParseObject()
        {
            var result = await ParseJson(@"{""key1"":""value1"",""key2"":2}");
            Assert.Equal(@"(:key1 ""value1"" :key2 2)", result.ToString());
        }
    }
}
