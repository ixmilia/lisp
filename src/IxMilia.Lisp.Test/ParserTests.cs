using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ParserTests
    {
        private static IEnumerable<LispObject> Parse(string code)
        {
            var tokens = new LispTokenizer(code).GetTokens();
            var nodes = new LispParser(tokens).Parse().Nodes;
            return nodes;
        }

        private static LispObject SingleSyntaxNode(string code)
        {
            var nodes = Parse(code);
            return nodes.Single();
        }

        [Fact]
        public void ParseConstants()
        {
            Assert.Equal("a", ((LispSymbol)SingleSyntaxNode("a")).Value);
            Assert.Equal(3, ((LispInteger)SingleSyntaxNode("3")).Value);
            Assert.Equal(3.0, ((LispFloat)SingleSyntaxNode("3.0")).Value);
            Assert.Equal("a", ((LispString)SingleSyntaxNode("\"a\"")).Value);
        }

        [Theory]
        [InlineData("\"\"")]
        [InlineData("\"\\n\"")]
        [InlineData("\"\\t\"")]
        [InlineData("\"\\\"\"")]
        [InlineData("\"\\\\\"")]
        public void RoundTripStrings(string text)
        {
            var str = (LispString)SingleSyntaxNode(text);
            var roundTripped = (LispString)SingleSyntaxNode(str.ToString());
            Assert.Equal(str.Value, roundTripped.Value);
        }

        [Fact]
        public void ParseList()
        {
            Assert.Empty(((LispList)SingleSyntaxNode("()")).ToList());
            Assert.Equal(new[] { "a" }, ((LispList)SingleSyntaxNode("(a)")).ToList().Cast<LispSymbol>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1 }, ((LispList)SingleSyntaxNode("(1)")).ToList().Cast<LispInteger>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1, 2 }, ((LispList)SingleSyntaxNode("(1 2)")).ToList().Cast<LispInteger>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1, 2, 3 }, ((LispList)SingleSyntaxNode("( 1 2 3 )")).ToList().Cast<LispInteger>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { typeof(LispInteger), typeof(LispSymbol)}, ((LispList)SingleSyntaxNode("( 1 x )")).ToList().Select(e => e.GetType()).ToArray());
            Assert.True(((LispList)SingleSyntaxNode("(1 2)")).IsProperList);
        }

        [Fact]
        public void DottedList()
        {
            var list = (LispList)SingleSyntaxNode("(1 . 2)");
            Assert.Equal(1, list.Length);
            Assert.Equal(new[] { 1, 2 }, list.ToList().Cast<LispInteger>().Select(n => n.Value).ToArray());
            Assert.False(list.IsProperList);
        }

        [Fact]
        public void BadDottedList()
        {
            var error = (LispError)Parse("(1 2 . 3 . 4)").First();
            Assert.Equal(1, error.Line);
            Assert.Equal(10, error.Column);
            Assert.Equal("Unexpected duplicate '.' in list at (1, 10); first '.' at (1, 6)", error.Message);
        }

        [Fact]
        public void Quoted()
        {
            Assert.Equal(new LispSymbol("a"), SingleSyntaxNode("a"));
            Assert.Equal(new LispQuotedObject(new LispSymbol("a")), SingleSyntaxNode("'a"));
            Assert.Equal(new LispQuotedObject(new LispQuotedObject(new LispSymbol("a"))), SingleSyntaxNode("''a"));
            Assert.Equal(new LispQuotedObject(new LispList(new LispInteger(1))), SingleSyntaxNode("'(1)"));
        }

        [Fact]
        public void UnmatchedLeftParen()
        {
            var error = (LispError)SingleSyntaxNode("(+ 1 2");
            Assert.Equal("Unmatched '(' at (1, 1) (depth 1)", error.Message);
            Assert.Equal(1, error.Line);
            Assert.Equal(1, error.Column);
        }

        [Fact]
        public void UnmatchedRightParen()
        {
            var nodes = Parse("(+ 1 2))").ToList();
            Assert.Equal(2, nodes.Count);
            Assert.IsType<LispList>(nodes.First());
            var error = (LispError)nodes.Last();
            Assert.Equal("Unexpected ')' at (1, 8)", error.Message);
            Assert.Equal(1, error.Line);
            Assert.Equal(8, error.Column);
        }
    }
}
