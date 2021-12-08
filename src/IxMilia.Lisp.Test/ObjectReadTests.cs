using System.IO;
using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ObjectReadTests
    {
        private LispObject Read(string text)
        {
            var host = new LispHost();
            var input = new LispStream("", new StringReader(text), TextWriter.Null);
            var reader = new LispObjectReader(host, input, false, host.Nil, false);
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
        public void DottedList()
        {
            var list = (LispList)Read("(1 . 2)");
            Assert.Equal(1, list.Length);
            Assert.Equal(new[] { 1, 2 }, list.ToList().Cast<LispInteger>().Select(n => n.Value).ToArray());
            Assert.False(list.IsProperList);
        }

        [Fact]
        public void BadDottedList()
        {
            var error = (LispError)Read("(1 2 . 3 . 4)");
            Assert.Equal(1, error.SourceLocation?.Line);
            Assert.Equal(10, error.SourceLocation?.Column);
            Assert.Equal("Unexpected duplicate '.' in list at (1, 10); first '.' at (1, 6)", error.Message);
        }

        [Fact]
        public void UnmatchedLeftParen()
        {
            var error = (LispError)Read("(1 2 3");
            Assert.Equal("Unmatched '(' at (1, 1) (depth 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Line);
            Assert.Equal(1, error.SourceLocation?.Column);
        }

        [Fact]
        public void UnmatchedRightParen()
        {
            var error = (LispError)Read(")");
            Assert.Equal("Unexpected character ')' at position (1, 1)", error.Message);
            Assert.Equal(1, error.SourceLocation?.Line);
            Assert.Equal(1, error.SourceLocation?.Column);
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

        [Theory]
        [InlineData(@"#\a", 'a')]
        [InlineData(@"#\/", '/')]
        [InlineData(@"#\'", '\'')]
        public void Characters(string code, char expected)
        {
            var c = (LispCharacter)Read(code);
            Assert.Equal(expected, c.Value);
        }

        [Fact]
        public void Keywords()
        {
            Assert.Equal(":ABC", ((LispKeyword)Read(" :abc ")).Keyword);
            Assert.Equal("&REST", ((LispLambdaListKeyword)Read(" &rest ")).Keyword);
        }

        [Fact]
        public void QuotedNamedFunctions()
        {
            Assert.Equal("READ", ((LispQuotedNamedFunctionReference)Read("#'read")).Name);
        }

        [Fact]
        public void Symbols()
        {
            Assert.Equal("+", ((LispSymbol)Read("+")).Value);
            Assert.Equal(">>", ((LispSymbol)Read(">>")).Value);
            Assert.Equal("SOME:SYMBOL", ((LispSymbol)Read("some:symbol")).Value);
        }

        [Fact]
        public void SourceLocations()
        {
            var list = (LispList)Read(" ( a b c ( 1 2 3 ) ) ");
            Assert.Equal(1, list.SourceLocation?.Line);
            Assert.Equal(2, list.SourceLocation?.Column);

            var listValues = list.ToList();
            Assert.Equal(1, listValues[0].SourceLocation?.Line);
            Assert.Equal(4, listValues[0].SourceLocation?.Column);
            Assert.Equal(1, listValues[1].SourceLocation?.Line);
            Assert.Equal(6, listValues[1].SourceLocation?.Column);
            Assert.Equal(1, listValues[2].SourceLocation?.Line);
            Assert.Equal(8, listValues[2].SourceLocation?.Column);

            var innerList = (LispList)listValues[3];
            Assert.Equal(1, innerList.SourceLocation?.Line);
            Assert.Equal(10, innerList.SourceLocation?.Column);

            var innerListValues = innerList.ToList();
            Assert.Equal(1, innerListValues[0].SourceLocation?.Line);
            Assert.Equal(12, innerListValues[0].SourceLocation?.Column);
            Assert.Equal(1, innerListValues[1].SourceLocation?.Line);
            Assert.Equal(14, innerListValues[1].SourceLocation?.Column);
            Assert.Equal(1, innerListValues[2].SourceLocation?.Line);
            Assert.Equal(16, innerListValues[2].SourceLocation?.Column);
        }

        [Fact]
        public void ParsedObjectsHaveParentsSet()
        {
            var rootList = Read(@"
(defun test-function ()
    (+ 1 2)
    (* (- 3 4) (/ 5 6)))
");
            Assert.Null(rootList.Parent);

            // check top level children
            var children = rootList.GetChildren().ToList();
            Assert.Equal(5, children.Count);
            foreach (var child in children)
            {
                Assert.True(ReferenceEquals(child.Parent, rootList), $"child = [{child}]; child.Parent [{child.Parent}] != rootNode [{rootList}]");
            }

            // spot-check one level more
            var multiplyExpression = children.Last();
            var nextLevelChildren = multiplyExpression.GetChildren().ToList();
            Assert.Equal(3, nextLevelChildren.Count);
            foreach (var child in nextLevelChildren)
            {
                Assert.True(ReferenceEquals(child.Parent, multiplyExpression));
            }
        }
    }
}
