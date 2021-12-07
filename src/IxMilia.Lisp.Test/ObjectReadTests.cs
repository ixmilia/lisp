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
            Assert.Equal(LispList.FromItems(new LispInteger(4), LispNilList.Instance), Read("(4())"));
        }

        [Fact]
        public void Strings()
        {
            Assert.Equal("", ((LispString)Read("\"\"")).Value);
            Assert.Equal("a", ((LispString)Read(" \"a\" ")).Value);
            Assert.Equal("\\\"\\", ((LispString)Read(" \"\\\\\\\"\\\\\" ")).Value);
        }

        [Fact]
        public void Numbers()
        {
            Assert.Equal(4, ((LispInteger)Read("4")).Value);
            Assert.Equal(4, ((LispInteger)Read("+4")).Value);
            Assert.Equal(-14, ((LispInteger)Read("-14")).Value);
            Assert.Equal("1/2", Read("1/2").ToString());
            Assert.Equal("1/2", Read("+1/2").ToString());
            Assert.Equal("-1/2", Read("-1/2").ToString());
        }

        [Fact]
        public void Keywords()
        {
            Assert.Equal(":ABC", ((LispKeyword)Read(" :abc ")).Keyword);
            Assert.Equal("&REST", ((LispLambdaListKeyword)Read(" &rest ")).Keyword);
        }
    }
}
