using Xunit;

namespace IxMilia.Lisp.Test
{
    public class FormatTests : TestBase
    {
        [Fact]
        public void NoArgumentTokens()
        {
            var formatString = "a~%bc~&~ade~sf";
            Assert.True(LispFormatter.TryGetFormatTokens(formatString, out var tokens, out var errorMessage), $"Error from formatter: {errorMessage}");
            Assert.Equal(8, tokens.Count);

            Assert.Equal("a", ((LispLiteralFormatToken)tokens[0]).GetTokenText(formatString));
            Assert.Equal("~%", ((LispFormatTokenNewLine)tokens[1]).GetTokenText(formatString));
            Assert.Equal("bc", ((LispLiteralFormatToken)tokens[2]).GetTokenText(formatString));
            Assert.Equal("~&", ((LispFormatTokenLineStart)tokens[3]).GetTokenText(formatString));
            Assert.Equal("~a", ((LispFormatTokenAExpression)tokens[4]).GetTokenText(formatString));
            Assert.Equal("de", ((LispLiteralFormatToken)tokens[5]).GetTokenText(formatString));
            Assert.Equal("~s", ((LispFormatTokenSExpression)tokens[6]).GetTokenText(formatString));
            Assert.Equal("f", ((LispLiteralFormatToken)tokens[7]).GetTokenText(formatString));
        }

        [Fact]
        public void NewlineDirectives()
        {
            TestFormat("a\nb", "a~%b");
        }

        [Fact]
        public void EnsureLineStartDirective()
        {
            TestFormat("a\nb\nc", "a~&b~&~&~&c");
        }

        [Fact]
        public void SimpleFormatArguments()
        {
            TestFormat(
                "A: a, B: (b), C: 5",
                "A: ~s, B: ~s, C: ~s",
                LispSymbol.CreateFromString("a"),
                new LispList(LispSymbol.CreateFromString("b")),
                new LispInteger(5));
        }

        [Fact]
        public void FormatStringValues()
        {
            TestFormat(
                "[ \"a\" ]",
                "[ ~s ]",
                new LispString("a"));
            TestFormat(
                "[ a ]",
                "[ ~a ]",
                new LispString("a"));
        }

        [Fact]
        public void FormatWithWidth()
        {
            TestFormat(
                "\"abc\"     .4",
                "~10s.~s",
                new LispString("abc"), new LispInteger(4));
            TestFormat(
                "\"abc\".4",
                "~2s.~s",
                new LispString("abc"), new LispInteger(4));
            TestFormat(
                "abc       .4",
                "~10a.~a",
                new LispString("abc"), new LispInteger(4));
            TestFormat(
                "abc.4",
                "~2a.~a",
                new LispString("abc"), new LispInteger(4));
        }

        [Fact]
        public void FormatWithDifferentCasing()
        {
            TestFormat(
                "\"abc\".4",
                "~S.~s",
                new LispString("abc"),
                new LispInteger(4));
        }

        private static void TestFormat(string expected, string formatString, params LispObject[] args)
        {
            Assert.True(LispFormatter.TryFormatString(formatString, args, out var result), $"Error from formatter: {result}");
            Assert.Equal(expected, result);
        }
    }
}
