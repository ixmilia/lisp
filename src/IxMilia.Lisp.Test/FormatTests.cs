using System.Linq;
using Xunit;

namespace IxMilia.Lisp.Test
{
    public class FormatTests : TestBase
    {
        [Fact]
        public void NoArgumentTokens()
        {
            var formatString = "a~%bc~&~Ade~Sf";
            var tokens = LispFormatter.GetFormatTokens(formatString).ToList();
            Assert.Equal(8, tokens.Count);

            Assert.Equal("a", tokens[0].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[0]);

            Assert.Equal("~%", tokens[1].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[1]);
            Assert.Equal('%', ((LispEscapeSequenceFormatToken)tokens[1]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[1]).GetArgument(formatString));

            Assert.Equal("bc", tokens[2].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[2]);

            Assert.Equal("~&", tokens[3].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[3]);
            Assert.Equal('&', ((LispEscapeSequenceFormatToken)tokens[3]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[3]).GetArgument(formatString));

            Assert.Equal("~A", tokens[4].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[4]);
            Assert.Equal('A', ((LispEscapeSequenceFormatToken)tokens[4]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[4]).GetArgument(formatString));

            Assert.Equal("de", tokens[5].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[5]);

            Assert.Equal("~S", tokens[6].GetString(formatString));
            Assert.IsType<LispEscapeSequenceFormatToken>(tokens[6]);
            Assert.Equal('S', ((LispEscapeSequenceFormatToken)tokens[6]).EscapeCode);
            Assert.Equal("", ((LispEscapeSequenceFormatToken)tokens[6]).GetArgument(formatString));

            Assert.Equal("f", tokens[7].GetString(formatString));
            Assert.IsType<LispRawFormatToken>(tokens[7]);
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
                "A: ~S, B: ~S, C: ~S",
                new LispSymbol("a"),
                new LispList(new LispSymbol("b")),
                new LispInteger(5));
        }

        [Fact]
        public void FormatStringValues()
        {
            TestFormat(
                "[ \"a\" ]",
                "[ ~S ]",
                new LispString("a"));
            TestFormat(
                "[ a ]",
                "[ ~A ]",
                new LispString("a"));
        }

        [Fact]
        public void FormatWithWidth()
        {
            TestFormat(
                "\"abc\"     .4",
                "~10S.~S",
                new LispString("abc"), new LispInteger(4));
            TestFormat(
                "\"abc\".4",
                "~2S.~S",
                new LispString("abc"), new LispInteger(4));
        }

        [Fact]
        public void FormatWithDifferentCasing()
        {
            TestFormat(
                "\"abc\".4",
                "~s.~s",
                new LispString("abc"),
                new LispInteger(4));
        }

        private static void TestFormat(string expected, string formatString, params LispObject[] args)
        {
            string result;
            Assert.True(LispFormatter.TryFormatString(formatString, args, out result), $"Error from formatter: {result}");
            Assert.Equal(expected, result);
        }
    }
}
