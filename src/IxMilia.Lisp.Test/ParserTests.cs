using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ParserTests
    {
        private static LispObject SingleSyntaxNode(string code)
        {
            var tokens = new LispTokenizer(code).GetTokens();
            var nodes = new LispParser(tokens).Parse();
            return nodes.Single();
        }

        [Fact]
        public void ParseConstants()
        {
            Assert.Equal("a", ((LispAtom)SingleSyntaxNode("a")).Value);
            Assert.Equal(3.0, ((LispNumber)SingleSyntaxNode("3")).Value);
            Assert.Equal("a", ((LispString)SingleSyntaxNode("\"a\"")).Value);
        }

        [Fact]
        public void ParseList()
        {
            Assert.Empty(((LispList)SingleSyntaxNode("()")).Value);
            Assert.Equal(new[] { "a" }, ((LispList)SingleSyntaxNode("(a)")).Value.Cast<LispAtom>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1.0 }, ((LispList)SingleSyntaxNode("(1)")).Value.Cast<LispNumber>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1.0, 2.0 }, ((LispList)SingleSyntaxNode("(1 2)")).Value.Cast<LispNumber>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { 1.0, 2.0, 3.0 }, ((LispList)SingleSyntaxNode("( 1 2 3 )")).Value.Cast<LispNumber>().Select(n => n.Value).ToArray());
            Assert.Equal(new[] { typeof(LispNumber), typeof(LispAtom)}, ((LispList)SingleSyntaxNode("( 1 x )")).Value.Select(e => e.GetType()).ToArray());
        }
    }
}
