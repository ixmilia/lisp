using System.IO;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ObjectReadTests
    {
        private LispObject Read(string text)
        {
            var host = new LispHost();
            var input = new LispStream("", new StringReader(text), TextWriter.Null);
            var reader = new LispObjectReader(host, input, true, host.Nil, false);
            var result = reader.Read();
            return result;
        }

        [Fact]
        public void EmptyLists()
        {
            Assert.Equal(LispNilList.Instance, Read("()"));
            Assert.Equal(LispNilList.Instance, Read(" ( ) "));
        }

        [Fact]
        public void NestedLists()
        {
            Assert.Equal(LispList.FromItems(new LispString("a")), Read(" (\"a\") "));
            Assert.Equal(LispList.FromItems(new LispString("a"), LispNilList.Instance, new LispString("b")), Read(" ( \"a\" () \"b\" ) "));
        }

        [Fact]
        public void Strings()
        {
            Assert.Equal("", ((LispString)Read("\"\"")).Value);
            Assert.Equal("a", ((LispString)Read(" \"a\" ")).Value);
            Assert.Equal("\\\"\\", ((LispString)Read(" \"\\\\\\\"\\\\\" ")).Value);
        }
    }
}
