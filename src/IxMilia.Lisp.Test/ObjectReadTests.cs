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

        [Theory]
        [InlineData("0", 0)]
        [InlineData("3", 3)]
        [InlineData("+3", 3)]
        [InlineData("-31", -31)]
        public void Integers(string code, int value)
        {
            var number = (LispInteger)Read(code);
            Assert.Equal(value, number.Value);
        }

        [Theory]
        [InlineData("1/2", "1/2")]
        [InlineData("+1/2", "1/2")]
        [InlineData("-1/2", "-1/2")]
        public void Ratios(string code, string value)
        {
            var ratio = (LispRatio)Read(code);
            Assert.Equal(value, ratio.ToString());
        }

        [Theory]
        [InlineData("3.5", 3.5)]
        [InlineData("3.5e4", 3.5e4)]
        [InlineData("+3.5e4", 3.5e4)]
        [InlineData("-3.5e4", -3.5e4)]
        [InlineData("3.5e+4", 3.5e4)]
        [InlineData("3.5e-4", 3.5e-4)]
        public void Floats(string code, double value)
        {
            var number = (LispFloat)Read(code);
            Assert.Equal(value, number.Value);
        }

        [Fact]
        public void Keywords()
        {
            Assert.Equal(":ABC", ((LispKeyword)Read(" :abc ")).Keyword);
            Assert.Equal("&REST", ((LispLambdaListKeyword)Read(" &rest ")).Keyword);
        }
    }
}
