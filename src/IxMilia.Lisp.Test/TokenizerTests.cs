using System.Collections.Generic;
using System.Linq;
using IxMilia.Lisp.Tokens;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class TokenizerTests
    {
        private static IEnumerable<LispToken> Tokens(string code)
        {
            var tokenizer = new LispTokenizer(code);
            var tokens = tokenizer.GetTokens();
            return tokens;
        }

        private static LispToken SingleToken(string code)
        {
            var tokens = Tokens(code);
            return tokens.Single();
        }

        private static LispTokenType[] GetTokenTypes(string code)
        {
            var tokens = Tokens(code);
            return tokens.Select(t => t.Type).ToArray();
        }

        private static void CheckTokenTypes(string code, params LispTokenType[] tokenTypes)
        {
            var types = GetTokenTypes(code);
            Assert.Equal(tokenTypes, types);
        }

        [Theory]
        [InlineData("")]
        [InlineData("  ")]
        [InlineData(" \t ")]
        [InlineData(" \n ")]
        [InlineData(";\n")]
        public void LeadingTrivia(string trivia)
        {
            var token = (LispLeftParenToken)SingleToken(trivia + "(");
            Assert.Equal(trivia, token.LeadingTrivia.ToString());
        }

        [Theory]
        [InlineData(")", 1, 1)]
        [InlineData(" )", 1, 2)]
        [InlineData(" \t)", 1, 3)]
        [InlineData("\t )", 1, 3)]
        [InlineData(" \t )", 1, 4)]
        [InlineData("\n)", 2, 1)]
        [InlineData("\r\n)", 2, 1)]
        [InlineData(";\r\n)", 2, 1)]
        [InlineData("\r\n )", 2, 2)]
        public void TokenOffsets(string code, int line, int column)
        {
            var token = (LispRightParenToken)SingleToken(code);
            Assert.Equal(line, token.Line);
            Assert.Equal(column, token.Column);
        }

        [Fact]
        public void TokenTypes()
        {
            CheckTokenTypes("(", LispTokenType.LeftParen);
            CheckTokenTypes("'(", LispTokenType.LeftParen);
            CheckTokenTypes("()", LispTokenType.LeftParen, LispTokenType.RightParen);
            CheckTokenTypes(" ( ) ", LispTokenType.LeftParen, LispTokenType.RightParen);
            CheckTokenTypes("- 3", LispTokenType.Symbol, LispTokenType.Number);
        }

        [Theory]
        [InlineData("0", 0.0)]
        [InlineData("3", 3.0)]
        [InlineData("+3", 3.0)]
        [InlineData("-3", -3.0)]
        [InlineData("3.5", 3.5)]
        [InlineData("3.5e4", 3.5e4)]
        public void Numbers(string code, double value)
        {
            var number = (LispNumberToken)SingleToken(code);
            Assert.Equal(value, number.Value);
        }

        [Theory]
        [InlineData("\"\"", "")]
        [InlineData("\"\\n\"", "\n")]
        [InlineData("\"\\\"\"", "\"")]
        public void Strings(string code, string text)
        {
            var str = (LispStringToken)SingleToken(code);
            Assert.Equal(text, str.Value);
        }
    }
}
