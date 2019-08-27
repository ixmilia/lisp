using System.Linq;
using IxMilia.Lisp.Parser;
using IxMilia.Lisp.Tokens;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class ParserTests
    {
        private static LispSyntax SingleSyntaxNode(string code)
        {
            var tokens = new LispTokenizer(code).GetTokens();
            var nodes = new LispParser(tokens).Parse();
            return nodes.Single();
        }

        [Fact]
        public void ParseConstants()
        {
            Assert.Equal("a", ((LispAtomSyntax)SingleSyntaxNode("a")).Atom.Value);
            Assert.Equal(3.0, ((LispNumberSyntax)SingleSyntaxNode("3")).Number.Value);
            Assert.Equal("a", ((LispStringSyntax)SingleSyntaxNode("\"a\"")).String.Text);
        }

        [Fact]
        public void ParseList()
        {
            Assert.Empty(((LispListSyntax)SingleSyntaxNode("()")).Elements);
            Assert.Empty(((LispRawListSyntax)SingleSyntaxNode("'()")).Elements);
            Assert.Equal(new[] { "a" }, ((LispListSyntax)SingleSyntaxNode("(a)")).Elements.Cast<LispAtomSyntax>().Select(n => n.Atom.Value).ToArray());
            Assert.Equal(new[] { 1.0 }, ((LispListSyntax)SingleSyntaxNode("(1)")).Elements.Cast<LispNumberSyntax>().Select(n => n.Number.Value).ToArray());
            Assert.Equal(new[] { 1.0, 2.0 }, ((LispListSyntax)SingleSyntaxNode("(1 2)")).Elements.Cast<LispNumberSyntax>().Select(n => n.Number.Value).ToArray());
            Assert.Equal(new[] { 1.0, 2.0, 3.0 }, ((LispListSyntax)SingleSyntaxNode("( 1 2 3 )")).Elements.Cast<LispNumberSyntax>().Select(n => n.Number.Value).ToArray());
            Assert.Equal(new[] { 1.0 }, ((LispRawListSyntax)SingleSyntaxNode("'( 1 )")).Elements.Cast<LispNumberSyntax>().Select(n => n.Number.Value).ToArray());
            Assert.Equal(new[] { LispSyntaxType.Number, LispSyntaxType.Atom }, ((LispRawListSyntax)SingleSyntaxNode("'( 1 x )")).Elements.Select(e => e.Type).ToArray());
        }
    }
}
