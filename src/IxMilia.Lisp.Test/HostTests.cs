using System.IO;
using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class HostTests
    {
        [Fact]
        public void ReadObjects()
        {
            var input = new StringReader("(abc \n 2)");
            var host = new LispHost(input: input);
            var objects = host.ReadCompleteObjects().ToList();
            var list = ((LispList)objects.Single()).ToList();
            Assert.Equal(2, list.Count);
            Assert.Equal("abc", ((LispSymbol)list[0]).Value);
            Assert.Equal(2, ((LispInteger)list[1]).Value);
        }
    }
}
