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
            var nodes = new LispParser(tokens).Parse();
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
            Assert.Equal(3.0, ((LispNumber)SingleSyntaxNode("3")).Value);
            Assert.Equal("a", ((LispString)SingleSyntaxNode("\"a\"")).Value);
        }

        [Fact]
        public void ParseList()
        {
            Assert.Empty(((LispList)SingleSyntaxNode("()")).Value);
            Assert.Equal(new[] { "a" }, ((LispList)SingleSyntaxNode("(a)")).Value.Cast<LispSymbol>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1.0 }, ((LispList)SingleSyntaxNode("(1)")).Value.Cast<LispNumber>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1.0, 2.0 }, ((LispList)SingleSyntaxNode("(1 2)")).Value.Cast<LispNumber>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1.0, 2.0, 3.0 }, ((LispList)SingleSyntaxNode("( 1 2 3 )")).Value.Cast<LispNumber>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { typeof(LispNumber), typeof(LispSymbol)}, ((LispList)SingleSyntaxNode("( 1 x )")).Value.Select(e => e.GetType()).ToArray());
        }

        [Fact]
        public void Quoted()
        {
            Assert.False(((LispSymbol)SingleSyntaxNode("a")).IsQuoted);
            Assert.True(((LispSymbol)SingleSyntaxNode("'a")).IsQuoted);
            Assert.False(((LispList)SingleSyntaxNode("(1)")).IsQuoted);
            Assert.True(((LispList)SingleSyntaxNode("'(1)")).IsQuoted);
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
